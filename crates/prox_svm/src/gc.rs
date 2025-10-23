use crate::value::Class;
use crate::value::Closure;
use crate::value::Instance;
use crate::value::Method;
use crate::value::Upvalue;
use crate::value::native::NativeFunction;
use compact_str::CompactString;
use core::any;
use core::convert;
use core::fmt;
use core::iter;
use core::marker;
use core::mem;
use core::num::NonZeroU32;
use prox_bytecode::pool::ConstantInterner;

/// A raw index into an arena.
/// This has no associated generation and is to be used in the
/// internal implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ArenaPtr(NonZeroU32);

impl ArenaPtr {
    fn new(index: usize) -> Option<Self> {
        NonZeroU32::new(u32::try_from(index).ok()?).map(ArenaPtr)
    }

    fn to_index(self) -> usize {
        (u32::from(self.0) as usize) - 1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Generation(u32);

impl Generation {
    const fn increment(self) -> Self {
        Self(self.0 + 1)
    }
}

#[derive(Debug, Clone)]
enum Entry<T> {
    Free {
        generation: Generation,
        next_free: Option<ArenaPtr>,
    },
    Used {
        is_alive: bool,
        generation: Generation,
        value: T,
    },
}

#[derive(Debug)]
pub struct Arena<T> {
    data: Vec<Entry<T>>,
    free_list: Option<ArenaPtr>,
}

#[derive(Debug)]
pub struct ArenaIterator<'data, T> {
    inner: &'data Arena<T>,
    index: usize,
}

impl<'data, T> iter::Iterator for ArenaIterator<'data, T> {
    type Item = (usize, &'data T);

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.inner.data.len() {
            let index = self.index;
            let entry = self.inner.data.get(index)?;
            self.index += 1;
            match *entry {
                Entry::Free { .. } => {}
                Entry::Used { ref value, .. } => return Some((index, value)),
            }
        }
        None
    }
}

pub struct ArenaIndex<T> {
    ptr: ArenaPtr,
    generation: Generation,
    _marker: marker::PhantomData<T>,
}

impl<T> ArenaIndex<T> {
    const fn from_raw_parts(ptr: ArenaPtr, generation: Generation) -> Self {
        Self {
            ptr,
            generation,
            _marker: marker::PhantomData,
        }
    }
}

impl<T> fmt::Debug for ArenaIndex<T> {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ArenaIndex")
            .field("ptr", &self.ptr)
            .field("generation", &self.generation)
            .field("type", &any::type_name::<T>())
            .finish_non_exhaustive()
    }
}

impl<T> Clone for ArenaIndex<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for ArenaIndex<T> {}

impl<T> PartialEq for ArenaIndex<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr && self.generation == other.generation
    }
}

impl<T> Eq for ArenaIndex<T> {}

impl<T> Arena<T> {
    pub const fn new() -> Self {
        Self {
            data: Vec::new(),
            free_list: None,
        }
    }

    pub fn alloc(&mut self, value: T) -> ArenaIndex<T> {
        if let Some(index) = self.next_free() {
            self.data[index.ptr.to_index()] = Entry::Used {
                generation: index.generation,
                value,
                is_alive: true,
            };
            index
        } else {
            let generation = Generation(0);
            self.data.push(Entry::Used {
                generation,
                value,
                is_alive: true,
            });
            let ptr = ArenaPtr::new(self.data.len()).expect("ran out of capacity.");
            ArenaIndex::from_raw_parts(ptr, generation)
        }
    }

    fn next_free(&mut self) -> Option<ArenaIndex<T>> {
        match self.free_list {
            None => None,
            Some(index) => match self.data[index.to_index()] {
                Entry::Free {
                    generation,
                    next_free,
                } => {
                    self.free_list = next_free;
                    Some(ArenaIndex::from_raw_parts(index, generation))
                }
                #[expect(clippy::panic, reason = "no clean way to deal with this.")]
                Entry::Used { .. } => panic!("arena free list is corrupted!"),
            },
        }
    }

    const fn iter(&self) -> ArenaIterator<'_, T> {
        ArenaIterator {
            inner: self,
            index: 0,
        }
    }
}

impl<T> Arena<T> {
    pub fn get(&self, index: ArenaIndex<T>) -> Option<&T> {
        let ptr = index.ptr;
        if ptr.to_index() >= self.data.len() {
            return None;
        }

        match self.data[ptr.to_index()] {
            Entry::Used {
                generation,
                ref value,
                ..
            } if generation == index.generation => Some(value),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, index: ArenaIndex<T>) -> Option<&mut T> {
        let ptr = index.ptr;
        if ptr.to_index() >= self.data.len() {
            return None;
        }

        match &mut self.data[ptr.to_index()] {
            &mut Entry::Used {
                generation,
                ref mut value,
                ..
            } if generation == index.generation => Some(value),
            _ => None,
        }
    }

    pub fn mark(&mut self, index: ArenaIndex<T>) {
        let ptr = index.ptr;
        if ptr.to_index() >= self.data.len() {
            return;
        }

        match self.data[ptr.to_index()] {
            Entry::Used {
                generation,
                ref mut is_alive,
                ..
            } if generation == index.generation => {
                *is_alive = true;
            }
            _ => {}
        }
    }

    pub fn remove(&mut self, index: ArenaIndex<T>) {
        let ptr = index.ptr;
        if ptr.to_index() >= self.data.len() {
            return;
        }

        match self.data[ptr.to_index()] {
            Entry::Used { generation, .. } if generation == index.generation => {
                let _entry = mem::replace(
                    &mut self.data[ptr.to_index()],
                    Entry::Free {
                        generation: generation.increment(),
                        next_free: self.free_list,
                    },
                );
                self.free_list = Some(ptr);
            }
            _ => {}
        }
    }
}

#[derive(Debug)]
pub struct ValueAllocator {
    strings: Arena<CompactString>,
    closures: Arena<Closure>,
    natives: Arena<Box<dyn NativeFunction>>,
    upvalues: Arena<Upvalue>,
    classes: Arena<Class>,
    instances: Arena<Instance>,
    methods: Arena<Method>,
}

impl ValueAllocator {
    pub const fn new() -> Self {
        Self {
            strings: Arena::new(),
            closures: Arena::new(),
            natives: Arena::new(),
            upvalues: Arena::new(),
            classes: Arena::new(),
            instances: Arena::new(),
            methods: Arena::new(),
        }
    }

    pub fn make_string_from_owned(&mut self, string: CompactString) -> ArenaIndex<CompactString> {
        self.strings.alloc(string)
    }

    pub fn make_string(&mut self, string: &str) -> ArenaIndex<CompactString> {
        self.strings.alloc(string.into())
    }

    pub fn resolve_string(&self, index: ArenaIndex<CompactString>) -> Option<&str> {
        self.strings.get(index).map(CompactString::as_str)
    }

    pub fn make_closure(&mut self, closure: Closure) -> ArenaIndex<Closure> {
        self.closures.alloc(closure)
    }

    pub fn resolve_closure(&self, index: ArenaIndex<Closure>) -> Option<&Closure> {
        self.closures.get(index)
    }

    pub fn make_native(
        &mut self,
        native: Box<dyn NativeFunction>,
    ) -> ArenaIndex<Box<dyn NativeFunction>> {
        self.natives.alloc(native)
    }

    pub fn resolve_native(
        &self,
        index: ArenaIndex<Box<dyn NativeFunction>>,
    ) -> Option<&dyn NativeFunction> {
        self.natives.get(index).map(convert::AsRef::as_ref)
    }

    pub fn make_upvalue(&mut self, upvalue: Upvalue) -> ArenaIndex<Upvalue> {
        self.upvalues.alloc(upvalue)
    }

    pub fn resolve_upvalue(&self, index: ArenaIndex<Upvalue>) -> Option<&Upvalue> {
        self.upvalues.get(index)
    }

    pub fn resolve_upvalue_mut(&mut self, index: ArenaIndex<Upvalue>) -> Option<&mut Upvalue> {
        self.upvalues.get_mut(index)
    }

    pub fn make_class(&mut self, class: Class) -> ArenaIndex<Class> {
        self.classes.alloc(class)
    }

    pub fn resolve_class(&self, index: ArenaIndex<Class>) -> Option<&Class> {
        self.classes.get(index)
    }

    pub fn resolve_class_mut(&mut self, index: ArenaIndex<Class>) -> Option<&mut Class> {
        self.classes.get_mut(index)
    }

    pub fn make_instance(&mut self, instance: Instance) -> ArenaIndex<Instance> {
        self.instances.alloc(instance)
    }

    pub fn resolve_instance(&self, index: ArenaIndex<Instance>) -> Option<&Instance> {
        self.instances.get(index)
    }

    pub fn resolve_instance_mut(&mut self, index: ArenaIndex<Instance>) -> Option<&mut Instance> {
        self.instances.get_mut(index)
    }

    pub fn make_method(&mut self, method: Method) -> ArenaIndex<Method> {
        self.methods.alloc(method)
    }

    pub fn resolve_method(&self, index: ArenaIndex<Method>) -> Option<&Method> {
        self.methods.get(index)
    }
}

impl ValueAllocator {
    pub fn dump(
        &self,
        buffer: &mut impl fmt::Write,
        interner: &ConstantInterner,
    ) -> Result<(), fmt::Error> {
        writeln!(buffer, "Strings:")?;
        for (index, string) in self.strings.iter() {
            writeln!(buffer, "#{index} = {string}")?;
        }

        writeln!(buffer, "Natives:")?;
        for (index, func) in self.natives.iter() {
            writeln!(buffer, "#{index} = {}", func.name())?;
        }

        writeln!(buffer, "Closures:")?;
        for (index, func) in self.closures.iter() {
            writeln!(buffer, "#{index} = {}", func.resolve(self, interner))?;
        }

        writeln!(buffer, "Upvalues:")?;
        for (index, func) in self.upvalues.iter() {
            writeln!(buffer, "#{index} = {func:?}")?;
        }

        writeln!(buffer, "Classes:")?;
        for (index, func) in self.classes.iter() {
            writeln!(buffer, "#{index} = {func:?}")?;
        }

        Ok(())
    }
}

impl ValueAllocator {
    pub fn prepare_to_collect(&mut self) {
        macro_rules! mark_dead {
            ($alloc:expr) => {
                $alloc.data.iter_mut().for_each(|entry| {
                    if let &mut Entry::Used {
                        ref mut is_alive, ..
                    } = entry
                    {
                        *is_alive = false;
                    }
                });
            };
        }

        mark_dead!(self.strings);
        mark_dead!(self.closures);
    }

    pub fn collect(&mut self) {
        macro_rules! free_dead {
            ($alloc:expr) => {
                for index in 0..$alloc.data.len() {
                    let entry = &$alloc.data[index];
                    if let &Entry::Used {
                        is_alive: false,
                        generation,
                        ..
                    } = entry
                    {
                        $alloc.remove(ArenaIndex::from_raw_parts(
                            ArenaPtr::new(index + 1).expect("guaranteed to be non-zero."),
                            generation,
                        ));
                    }
                }
            };
        }

        free_dead!(self.strings);
        free_dead!(self.closures);
    }

    pub fn mark_string(&mut self, index: ArenaIndex<CompactString>) {
        self.strings.mark(index);
    }

    pub fn mark_closure(&mut self, index: ArenaIndex<Closure>) {
        self.closures.mark(index);
    }
}
