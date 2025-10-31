//! Arena based allocator and garbage collector.

extern crate alloc;

use crate::value::Class;
use crate::value::Closure;
use crate::value::Instance;
use crate::value::Method;
use crate::value::Trace as _;
use crate::value::Upvalue;
use crate::value::Value;
use crate::value::native::NativeFunction;
use alloc::rc::Rc;
use compact_str::CompactString;
use core::any;
use core::convert;
use core::fmt;
use core::iter;
use core::marker;
use core::mem;
use core::num::NonZeroU32;
use prox_bytecode::pool::ConstantInterner;

/// The growth factor relative to the number of allocated bytes after collection.
const GC_HEAP_GROW_FACTOR: usize = 2;

/// A raw index into an arena.
/// This has no associated generation and is to be used in the
/// internal implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ArenaPtr(NonZeroU32);

impl ArenaPtr {
    /// Create a new arena pointer failing if the index does not fit into
    /// the internal representation or if it is zero i.e. the null pointer.
    fn new(index: usize) -> Option<Self> {
        NonZeroU32::new(u32::try_from(index).ok()?).map(ArenaPtr)
    }

    /// Convert to index to be used when indexing the internal arena data representation.
    fn to_index(self) -> usize {
        (u32::from(self.0) as usize) - 1
    }
}

/// An entry's generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Generation(u32);

impl Generation {
    /// Increment the generation.
    const fn increment(self) -> Self {
        Self(self.0 + 1)
    }

    /// Return the raw integer.
    pub(crate) const fn raw(self) -> u32 {
        self.0
    }
}

/// The color of an entry to be used during mark and sweep garbage collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MarkColor {
    /// Unvisited.
    White,
    /// Visited but references aren't traced yet.
    Grey,
    /// Visited with references traced.
    Black,
}

/// An entry in an arena.
#[derive(Debug, Clone)]
enum Entry<T> {
    /// A free entry in the arena.
    /// Can be reused for new allocations.
    Free {
        /// The entry's generation.
        generation: Generation,
        /// The next free entry if it exists.
        next_free: Option<ArenaPtr>,
    },
    /// A used entry in the arena.
    Used {
        /// The color of the entry.
        color: MarkColor,
        /// The entry's generation.
        generation: Generation,
        /// The entry's value.
        value: T,
    },
}

/// An arena allocator.
#[derive(Debug)]
pub(crate) struct Arena<T> {
    /// The underlying data store.
    data: Vec<Entry<T>>,
    /// The list of free entries.
    free_list: Option<ArenaPtr>,
    /// The name of the arena.
    name: &'static str,
}

/// An iterator over used entries in an arena.
#[derive(Debug)]
pub(crate) struct ArenaIterator<'data, T> {
    /// The arena to iterate over.
    inner: &'data Arena<T>,
    /// The current index.
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

/// A handle to a value allocated by an arena.
pub(crate) struct ArenaIndex<T> {
    /// The index to an arena.
    ptr: ArenaPtr,
    /// The entry's generation.
    generation: Generation,
    /// Type marker to tie handle to allocated value type.
    _marker: marker::PhantomData<T>,
}

impl<T> ArenaIndex<T> {
    /// Construct an arena index from raw parts.
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
    /// Initialise a new arena allocator.
    const fn new(name: &'static str) -> Self {
        Self {
            data: Vec::new(),
            free_list: None,
            name,
        }
    }

    /// Allocate a value and return a handle to it.
    fn alloc(&mut self, value: T) -> ArenaIndex<T> {
        if let Some(index) = self.next_free() {
            self.data[index.ptr.to_index()] = Entry::Used {
                generation: index.generation,
                value,
                color: MarkColor::White,
            };
            index
        } else {
            let generation = Generation(0);
            self.data.push(Entry::Used {
                generation,
                value,
                color: MarkColor::White,
            });
            let ptr = ArenaPtr::new(self.data.len()).expect("ran out of capacity.");
            ArenaIndex::from_raw_parts(ptr, generation)
        }
    }

    /// Return the number of used bytes in the arena.
    fn num_bytes(&self) -> usize {
        self.data
            .iter()
            .filter(|ent| matches!(ent, Entry::Used { .. }))
            .count()
            * mem::size_of::<Entry<T>>()
    }

    /// Return the number of bytes used by an entry.
    #[expect(clippy::unused_self, reason = "need to be a method to use in macros.")]
    const fn entry_size(&self) -> usize {
        mem::size_of::<Entry<T>>()
    }

    /// Return the next free entry if it exists.
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

    /// Create an iterator over live entries.
    const fn iter(&self) -> ArenaIterator<'_, T> {
        ArenaIterator {
            inner: self,
            index: 0,
        }
    }
}

/// Errors when interfacing with an arena allocator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArenaError {
    /// The handle's index is out of bounds.
    OutOfBounds {
        /// The out of bounds index.
        index: usize,
        /// The name of the arena.
        name: &'static str,
    },
    /// The handle's generation is out of date.
    WrongGeneration {
        /// The entry's generation.
        expected: Generation,
        /// The handle's generation.
        actual: Generation,
        /// The name of the arena.
        name: &'static str,
    },
    /// The handle points to a freed entry.
    Free {
        /// The index to the free entry.
        index: usize,
        /// The name of the arena.
        name: &'static str,
    },
}

impl<T> Arena<T> {
    /// Get a reference to the value pointed to by the handle if it is valid.
    fn get(&self, index: ArenaIndex<T>) -> Result<&T, ArenaError> {
        let ptr = index.ptr;
        if ptr.to_index() >= self.data.len() {
            return Err(ArenaError::OutOfBounds {
                index: ptr.to_index(),
                name: self.name,
            });
        }

        match self.data[ptr.to_index()] {
            Entry::Used {
                generation,
                ref value,
                ..
            } if generation == index.generation => Ok(value),
            Entry::Used { generation, .. } => Err(ArenaError::WrongGeneration {
                expected: generation,
                actual: index.generation,
                name: self.name,
            }),
            Entry::Free { .. } => Err(ArenaError::Free {
                index: index.ptr.to_index(),
                name: self.name,
            }),
        }
    }

    /// Get a mutable reference to the value pointed to by the handle if it is valid.
    fn get_mut(&mut self, index: ArenaIndex<T>) -> Result<&mut T, ArenaError> {
        let ptr = index.ptr;
        if ptr.to_index() >= self.data.len() {
            return Err(ArenaError::OutOfBounds {
                index: ptr.to_index(),
                name: self.name,
            });
        }

        #[expect(clippy::match_ref_pats, reason = "need to use &mut to borrow mutably.")]
        match &mut self.data[ptr.to_index()] {
            &mut Entry::Used {
                generation,
                ref mut value,
                ..
            } if generation == index.generation => Ok(value),
            &mut Entry::Used { generation, .. } => Err(ArenaError::WrongGeneration {
                expected: generation,
                actual: index.generation,
                name: self.name,
            }),
            &mut Entry::Free { .. } => Err(ArenaError::Free {
                index: index.ptr.to_index(),
                name: self.name,
            }),
        }
    }

    /// Mark the value pointed to by the handle if it is valid with the given color.
    /// Also return whether the value was already marked or not i.e. it was not white before being marked.
    fn mark(&mut self, index: ArenaIndex<T>, mark: MarkColor) -> Result<bool, ArenaError> {
        let ptr = index.ptr;
        if ptr.to_index() >= self.data.len() {
            return Err(ArenaError::OutOfBounds {
                index: index.ptr.to_index(),
                name: self.name,
            });
        }

        match self.data[ptr.to_index()] {
            Entry::Used {
                generation,
                ref mut color,
                ..
            } if generation == index.generation => {
                let is_marked = matches!(color, MarkColor::Grey | MarkColor::Black);
                *color = mark;
                Ok(is_marked)
            }
            Entry::Used { generation, .. } => Err(ArenaError::WrongGeneration {
                expected: generation,
                actual: index.generation,
                name: self.name,
            }),
            Entry::Free { .. } => Err(ArenaError::Free {
                index: index.ptr.to_index(),
                name: self.name,
            }),
        }
    }

    /// Remove an entry and add it to the free list.
    fn remove(&mut self, index: ArenaIndex<T>) {
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

type Native = Rc<dyn NativeFunction>;

/// A handle to an object.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ObjectIndex {
    /// A handle to a string.
    String(ArenaIndex<CompactString>),
    /// A handle to a closure.
    Closure(ArenaIndex<Closure>),
    /// A handle to a native function.
    Native(ArenaIndex<Native>),
    /// A handle to an upvalue.
    Upvalue(ArenaIndex<Upvalue>),
    /// A handle to a class.
    Class(ArenaIndex<Class>),
    /// A handle to an instance.
    Instance(ArenaIndex<Instance>),
    /// A handle to a method.
    Method(ArenaIndex<Method>),
}

macro_rules! generate_from_arena_index {
    ($index:ty => $variant:ident) => {
        impl convert::From<$index> for ObjectIndex {
            fn from(value: $index) -> Self {
                Self::$variant(value)
            }
        }
    };
}

generate_from_arena_index!(ArenaIndex<CompactString> => String);
generate_from_arena_index!(ArenaIndex<Closure> => Closure);
generate_from_arena_index!(ArenaIndex<Native> => Native);
generate_from_arena_index!(ArenaIndex<Upvalue> => Upvalue);
generate_from_arena_index!(ArenaIndex<Class> => Class);
generate_from_arena_index!(ArenaIndex<Instance> => Instance);
generate_from_arena_index!(ArenaIndex<Method> => Method);

impl From<ObjectIndex> for Value {
    fn from(value: ObjectIndex) -> Self {
        match value {
            ObjectIndex::String(index) => Self::String(index),
            ObjectIndex::Closure(index) => Self::Closure(index),
            ObjectIndex::Native(index) => Self::Native(index),
            ObjectIndex::Upvalue(index) => Self::Upvalue(index),
            ObjectIndex::Class(index) => Self::Class(index),
            ObjectIndex::Instance(index) => Self::Instance(index),
            ObjectIndex::Method(index) => Self::Method(index),
        }
    }
}

/// Errors when interfacing with the object allocator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AllocatorError {
    /// The handle's index is out of bounds.
    OutOfBounds {
        /// The out of bounds index.
        index: usize,
        /// The name of the arena.
        name: &'static str,
    },
    /// The handle's generation is out of date.
    WrongGeneration {
        /// The entry's generation.
        expected: Generation,
        /// The handle's generation.
        actual: Generation,
        /// The name of the arena.
        name: &'static str,
    },
    /// The handle points to a freed entry.
    Free {
        /// The index to the free entry.
        index: usize,
        /// The name of the arena.
        name: &'static str,
    },
}

impl From<ArenaError> for AllocatorError {
    fn from(value: ArenaError) -> Self {
        match value {
            ArenaError::OutOfBounds { index, name } => Self::OutOfBounds { index, name },
            ArenaError::WrongGeneration {
                expected,
                actual,
                name,
            } => Self::WrongGeneration {
                expected,
                actual,
                name,
            },
            ArenaError::Free { index, name } => Self::Free { index, name },
        }
    }
}

/// Allocator for objects.
#[derive(Debug)]
pub(crate) struct ObjectAllocator {
    /// Arena allocator for strings.
    strings: Arena<CompactString>,
    /// Arena allocator for closures.
    closures: Arena<Closure>,
    /// Arena allocator for native functions.
    natives: Arena<Native>,
    /// Arena allocator for upvalues.
    upvalues: Arena<Upvalue>,
    /// Arena allocator for classes.
    classes: Arena<Class>,
    /// Arena allocator for instances.
    instances: Arena<Instance>,
    /// Arena allocator for methods.
    methods: Arena<Method>,

    // GC
    /// The work queue for objects to trace.
    grey_stack: Vec<ObjectIndex>,
    /// The capacity in bytes before garbage collection.
    capacity: usize,
}

impl ObjectAllocator {
    /// Initialise the object allocator.
    pub(crate) const fn new() -> Self {
        Self {
            strings: Arena::new("strings"),
            closures: Arena::new("closures"),
            natives: Arena::new("natives"),
            upvalues: Arena::new("upvalues"),
            classes: Arena::new("classes"),
            instances: Arena::new("instances"),
            methods: Arena::new("methods"),
            grey_stack: Vec::new(),
            capacity: 1024,
        }
    }

    /// Allocate a string given an owned string.
    pub(crate) fn make_string_from_owned(
        &mut self,
        string: CompactString,
    ) -> ArenaIndex<CompactString> {
        self.strings.alloc(string)
    }

    /// Allocate a string given a string slice.
    pub(crate) fn make_string(&mut self, string: &str) -> ArenaIndex<CompactString> {
        self.strings.alloc(string.into())
    }

    /// Allocate a closure.
    pub(crate) fn make_closure(&mut self, closure: Closure) -> ArenaIndex<Closure> {
        self.closures.alloc(closure)
    }
    /// Allocate an upvalue.
    pub(crate) fn make_upvalue(&mut self, upvalue: Upvalue) -> ArenaIndex<Upvalue> {
        self.upvalues.alloc(upvalue)
    }

    /// Allocate a native function.
    pub(crate) fn make_native(&mut self, native: Native) -> ArenaIndex<Native> {
        self.natives.alloc(native)
    }

    /// Allocate a class.
    pub(crate) fn make_class(&mut self, class: Class) -> ArenaIndex<Class> {
        self.classes.alloc(class)
    }

    /// Allocate an instance.
    pub(crate) fn make_instance(&mut self, instance: Instance) -> ArenaIndex<Instance> {
        self.instances.alloc(instance)
    }

    /// Allocate a method.
    pub(crate) fn make_method(&mut self, method: Method) -> ArenaIndex<Method> {
        self.methods.alloc(method)
    }

    /// Return a reference to a string given a valid handle.
    pub(crate) fn resolve_string(
        &self,
        index: ArenaIndex<CompactString>,
    ) -> Result<&str, AllocatorError> {
        self.strings
            .get(index)
            .map(CompactString::as_str)
            .map_err(convert::Into::into)
    }

    /// Return a reference to a closure given a valid handle.
    pub(crate) fn resolve_closure(
        &self,
        index: ArenaIndex<Closure>,
    ) -> Result<&Closure, AllocatorError> {
        self.closures.get(index).map_err(convert::Into::into)
    }

    /// Return a reference to a native function given a valid handle.
    pub(crate) fn resolve_native(
        &self,
        index: ArenaIndex<Native>,
    ) -> Result<&Native, AllocatorError> {
        self.natives.get(index).map_err(convert::Into::into)
    }

    /// Return a reference to an upvalue given a valid handle.
    pub(crate) fn resolve_upvalue(
        &self,
        index: ArenaIndex<Upvalue>,
    ) -> Result<&Upvalue, AllocatorError> {
        self.upvalues.get(index).map_err(convert::Into::into)
    }

    /// Return a reference to a class given a valid handle.
    pub(crate) fn resolve_class(&self, index: ArenaIndex<Class>) -> Result<&Class, AllocatorError> {
        self.classes.get(index).map_err(convert::Into::into)
    }

    /// Return a reference to an instance given a valid handle.
    pub(crate) fn resolve_instance(
        &self,
        index: ArenaIndex<Instance>,
    ) -> Result<&Instance, AllocatorError> {
        self.instances.get(index).map_err(convert::Into::into)
    }

    /// Return a reference to a method given a valid handle.
    pub(crate) fn resolve_method(
        &self,
        index: ArenaIndex<Method>,
    ) -> Result<&Method, AllocatorError> {
        self.methods.get(index).map_err(convert::Into::into)
    }

    /// Return a mutable reference to an upvalue value given a valid handle.
    pub(crate) fn resolve_upvalue_mut(
        &mut self,
        index: ArenaIndex<Upvalue>,
    ) -> Result<&mut Upvalue, AllocatorError> {
        self.upvalues.get_mut(index).map_err(convert::Into::into)
    }

    /// Return a mutable reference to an upvalue value given a valid handle.
    pub(crate) fn resolve_class_mut(
        &mut self,
        index: ArenaIndex<Class>,
    ) -> Result<&mut Class, AllocatorError> {
        self.classes.get_mut(index).map_err(convert::Into::into)
    }

    /// Return a mutable reference to an upvalue value given a valid handle.
    pub(crate) fn resolve_instance_mut(
        &mut self,
        index: ArenaIndex<Instance>,
    ) -> Result<&mut Instance, AllocatorError> {
        self.instances.get_mut(index).map_err(convert::Into::into)
    }
}

/// Apply a macro to all arenas in the object allocator.
macro_rules! apply_to_arenas {
    ($object:expr => $op:ident) => {
        $op!($object.strings);
        $op!($object.closures);
        $op!($object.natives);
        $op!($object.upvalues);
        $op!($object.classes);
        $op!($object.instances);
        $op!($object.methods);
    };
}

impl ObjectAllocator {
    /// Dump the contents of the object allocator.
    pub(crate) fn dump(
        &self,
        buffer: &mut impl fmt::Write,
        interner: &ConstantInterner,
    ) -> Result<(), fmt::Error> {
        writeln!(buffer, "Strings: {} bytes", self.strings.num_bytes())?;
        for (index, string) in self.strings.iter() {
            const CUTOFF: usize = 30;
            if string.chars().count() < CUTOFF {
                writeln!(buffer, "#{index} = \"{string}\"")?;
            } else {
                writeln!(
                    buffer,
                    "#{index} = \"{}... [0..{CUTOFF}]",
                    string.chars().take(CUTOFF).collect::<CompactString>()
                )?;
            }
        }

        writeln!(buffer, "Natives: {} bytes", self.natives.num_bytes())?;
        for (index, func) in self.natives.iter() {
            writeln!(buffer, "#{index} = {}", func.name())?;
        }

        writeln!(buffer, "Closures: {} bytes", self.closures.num_bytes())?;
        for (index, func) in self.closures.iter() {
            writeln!(buffer, "#{index} = {}", func.resolve(self, interner))?;
        }

        writeln!(buffer, "Upvalues: {} bytes", self.upvalues.num_bytes())?;
        for (index, func) in self.upvalues.iter() {
            writeln!(buffer, "#{index} = {func:?}")?;
        }

        writeln!(buffer, "Classes: {} bytes", self.classes.num_bytes())?;
        for (index, func) in self.classes.iter() {
            writeln!(buffer, "#{index} = {func:?}")?;
        }

        writeln!(buffer, "Instances: {} bytes", self.instances.num_bytes())?;
        for (index, func) in self.instances.iter() {
            writeln!(buffer, "#{index} = {func:?}")?;
        }

        writeln!(buffer, "Methods: {} bytes", self.methods.num_bytes())?;
        for (index, func) in self.methods.iter() {
            writeln!(buffer, "#{index} = {func:?}")?;
        }

        Ok(())
    }
}

impl ObjectAllocator {
    /// Return whether the allocator wants to garbage collect.
    pub(crate) fn should_collect(&self) -> bool {
        self.num_bytes() > self.capacity
    }

    /// Prepare the allocator for garbage collection by marking all entries as unvisited.
    pub(crate) fn prepare_to_collect(&mut self) {
        macro_rules! mark_white {
            ($alloc:expr) => {
                $alloc.data.iter_mut().for_each(|entry| {
                    if let &mut Entry::Used { ref mut color, .. } = entry {
                        *color = MarkColor::White;
                    }
                });
            };
        }

        apply_to_arenas!(self => mark_white);
    }

    /// Garbage collect all unreached entries after tracing references.
    /// Also returns the number of freed bytes.
    pub(crate) fn collect(&mut self) -> Result<usize, AllocatorError> {
        // Trace references.
        self.trace_references()?;

        let mut freed = 0;
        macro_rules! free_white {
            ($alloc:expr) => {
                for index in 0..$alloc.data.len() {
                    let entry = &$alloc.data[index];
                    if let &Entry::Used {
                        color: MarkColor::White,
                        generation,
                        ..
                    } = entry
                    {
                        $alloc.remove(ArenaIndex::from_raw_parts(
                            ArenaPtr::new(index + 1).expect("guaranteed to be non-zero."),
                            generation,
                        ));
                        freed += $alloc.entry_size();
                    }
                }
            };
        }
        apply_to_arenas!(self => free_white);
        self.capacity = self.num_bytes() * GC_HEAP_GROW_FACTOR;

        Ok(freed)
    }

    /// Mark an object pointed to by the index with the given color.
    pub(crate) fn mark(
        &mut self,
        index: ObjectIndex,
        mark: MarkColor,
    ) -> Result<(), AllocatorError> {
        let is_marked = match index {
            ObjectIndex::String(arena_index) => self.strings.mark(arena_index, mark)?,
            ObjectIndex::Closure(arena_index) => self.closures.mark(arena_index, mark)?,
            ObjectIndex::Native(arena_index) => self.natives.mark(arena_index, mark)?,
            ObjectIndex::Upvalue(arena_index) => self.upvalues.mark(arena_index, mark)?,
            ObjectIndex::Class(arena_index) => self.classes.mark(arena_index, mark)?,
            ObjectIndex::Instance(arena_index) => self.instances.mark(arena_index, mark)?,
            ObjectIndex::Method(arena_index) => self.methods.mark(arena_index, mark)?,
        };

        if !is_marked && matches!(mark, MarkColor::Grey) {
            self.grey_stack.push(index);
        }

        Ok(())
    }

    /// Trace all references by blackening all grey objects.
    fn trace_references(&mut self) -> Result<(), AllocatorError> {
        while let Some(index) = self.grey_stack.pop() {
            Value::from(index).blacken(self)?;
        }
        Ok(())
    }

    /// The number of bytes taken by live entries.
    pub(crate) fn num_bytes(&self) -> usize {
        self.strings.num_bytes()
            + self.closures.num_bytes()
            + self.natives.num_bytes()
            + self.upvalues.num_bytes()
            + self.classes.num_bytes()
            + self.instances.num_bytes()
            + self.methods.num_bytes()
    }
}
