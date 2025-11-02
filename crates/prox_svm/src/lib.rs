//! A stack-based virtual machine implementation.

extern crate alloc;

/// Runtime errors.
pub mod error;
/// Allocator and garbage collector.
mod gc;
/// The IO interface.
pub mod io;
/// The VM values.
mod value;

use crate::{
    error::{RuntimeError, RuntimeErrorKind},
    gc::{ArenaIndex, ObjectAllocator},
    io::IoContext,
    value::{
        Class, Closure, Instance, Method, Trace as _, Upvalue,
        native::{Clock, NativeFunction, Stringify},
    },
};
use alloc::rc::Rc;
use arrayvec::ArrayVec;
use prox_bytecode::{Opcode, chunk::ChunkId};
use prox_bytecode::{StackSlot, function::Upvalue as ProtoUpvalue};
use prox_compiler::Compilation;
use prox_interner::Symbol;
use prox_span::Span;
use std::collections::{HashMap, hash_map::Entry};
use value::Value;

/// The control flow of the VM execution.
#[derive(Debug)]
enum ControlFlow {
    /// Continue execution.
    Continue,
    /// Finish execution.
    Done,
}

/// A call frame.
#[derive(Debug, Clone)]
struct CallFrame {
    /// The instruction pointer.
    ip: usize,
    /// The call frame's closure.
    closure: ArenaIndex<Closure>,
    /// The base stack slot for the call frame.
    base: StackSlot,
}

/// The virtual machine.
struct Vm {
    /// The call stack.
    call_stack: ArrayVec<CallFrame, 64>,
    /// The value stack.
    stack: ArrayVec<Value, 256>,
    /// The globals.
    globals: HashMap<Symbol, Value>,
    /// The open upvalues that may need to be closed.
    open_upvalues: Option<ArenaIndex<Upvalue>>,
    /// The debug mode.
    is_debug: bool,
    /// Last executed chunk ID.
    last_chunk: ChunkId,
    /// Last executed IP.
    last_ip: usize,
}

/// Use when resolving a handle using the allocator to return the correct runtime error.
macro_rules! dereference {
    ($option:expr => $span:expr) => {
        $option
    };
}

impl Vm {
    /// Initialise the virtual machine.
    fn new(is_debug: bool) -> Self {
        Self {
            call_stack: ArrayVec::new(),
            stack: ArrayVec::new(),
            globals: HashMap::new(),
            open_upvalues: None,
            is_debug,
            last_chunk: ChunkId(0),
            last_ip: 0,
        }
    }

    /// Add a builtin.
    fn add_builtin(
        &mut self,
        allocator: &mut ObjectAllocator,
        code: &mut Compilation,
        func: Rc<dyn NativeFunction>,
    ) {
        let sym = code.resolver.intern(func.name());
        self.globals
            .insert(sym, Value::Native(allocator.make_native(func)));
    }

    /// Run the virtual machine on the given code.
    fn run(
        &mut self,
        context: &mut impl IoContext,
        code: &mut Compilation,
    ) -> Result<(), RuntimeError> {
        let mut allocator = ObjectAllocator::new();
        // Add builtins.
        {
            self.add_builtin(&mut allocator, code, Rc::new(Clock));
            self.add_builtin(&mut allocator, code, Rc::new(Stringify));
        }

        // Set up stack.
        let script_symbol = code.resolver.intern("script");
        let script = allocator.make_closure(Closure {
            name: script_symbol,
            chunk: ChunkId::from(0),
            arity: 0,
            upvalues: Vec::new(),
        });
        self.call_stack.push(CallFrame {
            ip: 0,
            closure: script,
            base: StackSlot::from(0),
        });
        self.stack.push(Value::Closure(script));

        loop {
            let result = self.interpret(&mut allocator, context, code);
            match result {
                Ok(ControlFlow::Continue) => {
                    if allocator.should_collect() {
                        let span = code
                            .pool
                            .get_chunk(self.last_chunk)
                            .and_then(|chunk| chunk.spans.get(self.last_ip))
                            .copied()
                            .unwrap_or(Span {
                                start: 0,
                                length: 1,
                            });
                        self.debug_stack(&allocator, code);
                        allocator.prepare_to_collect();
                        self.mark_roots(&mut allocator)
                            .map_err(|kind| RuntimeError { kind, span })?;
                        let freed = allocator.collect().map_err(|err| RuntimeError {
                            kind: err.into(),
                            span,
                        })?;
                        if self.is_debug {
                            tracing::debug!("Freed {freed} bytes.");
                        }
                    }
                }
                Ok(ControlFlow::Done) => {
                    break;
                }
                Err(kind) => {
                    let span = code
                        .pool
                        .get_chunk(self.last_chunk)
                        .and_then(|chunk| chunk.spans.get(self.last_ip))
                        .copied()
                        .unwrap_or(Span {
                            start: 0,
                            length: 1,
                        });
                    return Err(RuntimeError { kind, span });
                }
            }
        }

        Ok(())
    }

    #[inline]
    #[expect(clippy::too_many_lines, reason = "this function is hard to decompose.")]
    #[expect(
        clippy::cognitive_complexity,
        reason = "this function is hard to decompose."
    )]
    fn interpret(
        &mut self,
        allocator: &mut ObjectAllocator,
        context: &mut impl IoContext,
        code: &Compilation,
    ) -> Result<ControlFlow, RuntimeErrorKind> {
        macro_rules! pop_value {
            ($span:expr) => {
                self.stack.pop().ok_or(RuntimeErrorKind::EmptyStack)
            };
        }

        macro_rules! peek_value {
            ($slot:expr) => {
                self.stack.get($slot).ok_or(RuntimeErrorKind::EmptyStack)
            };

            () => {
                self.stack.last().ok_or(RuntimeErrorKind::EmptyStack)
            };
        }

        macro_rules! pop_frame {
            ($span:expr) => {
                self.call_stack
                    .pop()
                    .ok_or(RuntimeErrorKind::EmptyCallStack)
            };
        }

        let (ip, base, base_closure) = {
            let call_frame = self
                .call_stack
                .last()
                .ok_or(RuntimeErrorKind::EmptyCallStack)?;
            (call_frame.ip, call_frame.base, call_frame.closure)
        };
        self.last_ip = ip;
        let chunk = {
            let closure = allocator.resolve_closure(base_closure)?;
            self.last_chunk = closure.chunk;
            code.pool
                .get_chunk(closure.chunk)
                .ok_or(RuntimeErrorKind::InvalidChunkDereference { id: closure.chunk })
        }?;

        let Some(inst) = chunk.stream.get(ip) else {
            return Ok(ControlFlow::Done);
        };

        let next_instruction_offset = ip + 1;

        macro_rules! current_frame {
            () => {
                match self.call_stack.last_mut() {
                    Some(frame) => Ok(frame),
                    None => Err(RuntimeErrorKind::EmptyCallStack),
                }
            };
        }

        #[expect(clippy::pattern_type_mismatch, reason = "clear enough.")]
        match inst {
            Opcode::Call(arity) => {
                let arity = *arity;
                let slot = self.stack.len() - 1 - (arity as usize);
                let callee = &self.stack[slot];
                match *callee {
                    Value::Closure(index) => {
                        return self.call_closure(
                            allocator,
                            StackSlot::try_from(slot).expect("stack can't grow past u32."),
                            arity,
                            index,
                            next_instruction_offset,
                        );
                    }
                    Value::Method(index) => {
                        let method = dereference!(allocator.resolve_method(index) => span!())?;
                        self.stack[slot] = Value::Instance(method.receiver);
                        return self.call_closure(
                            allocator,
                            StackSlot::try_from(slot).expect("stack can't grow past u32."),
                            arity,
                            method.closure,
                            next_instruction_offset,
                        );
                    }
                    Value::Native(index) => {
                        let closure = Rc::clone(allocator.resolve_native(index)?);
                        if closure.arity() != arity {
                            return Err(RuntimeErrorKind::InvalidArgumentCount {
                                expected: closure.arity(),
                                actual: arity,
                            });
                        }
                        let args = &self.stack[slot + 1..];
                        let result = closure.call(allocator, &code.resolver, args)?;
                        self.stack.truncate(slot);
                        self.stack.push(result);
                    }
                    Value::Class(index) => {
                        self.stack[slot] = Value::Instance(allocator.make_instance(Instance {
                            class: index,
                            fields: HashMap::new(),
                        }));
                        let class = dereference!(allocator.resolve_class(index) => span!())?;
                        if let Some(init) = class.methods.get(&code.resolver.init_sym()) {
                            return self.call_closure(
                                allocator,
                                StackSlot::try_from(slot).expect("stack can't grow past u32."),
                                arity,
                                *init,
                                next_instruction_offset,
                            );
                        } else if arity != 0 {
                            return Err(RuntimeErrorKind::InvalidArgumentCount {
                                expected: 0,
                                actual: arity,
                            });
                        }
                    }
                    _ => {
                        return Err(RuntimeErrorKind::InvalidCallee);
                    }
                }
            }
            Opcode::Print => {
                let value = pop_value!(span!())?;
                writeln!(context, "{}", value.resolve(allocator, &code.resolver))
                    .map_err(|_err| RuntimeErrorKind::Io)?;
            }
            Opcode::Return => {
                let result = pop_value!(span!())?;
                let popped_frame = pop_frame!(span!())?;
                // Pop the main function.
                if self.call_stack.is_empty() {
                    pop_value!(span!())?;
                    return Ok(ControlFlow::Done);
                }
                self.close_upvalues(allocator, popped_frame.base)?;
                self.stack.truncate(popped_frame.base.to_usize());
                self.stack.push(result);
                return Ok(ControlFlow::Continue);
            }
            Opcode::Inherit => {
                let Value::Class(sub_class_handle) = pop_value!(span!())? else {
                    return Err(RuntimeErrorKind::InvalidSubClass);
                };
                let &Value::Class(super_class_handle) = peek_value!()? else {
                    return Err(RuntimeErrorKind::InvalidSuperClass);
                };

                let super_class =
                    dereference!(allocator.resolve_class(super_class_handle) => span!())?;

                let super_methods: Vec<_> = super_class
                    .methods
                    .iter()
                    .map(|(key, val)| (*key, *val))
                    .collect();
                let sub_class =
                    dereference!(allocator.resolve_class_mut(sub_class_handle) => span!())?;
                sub_class.methods.extend(super_methods);
            }
            Opcode::AttachMethod => {
                let Value::Closure(closure_handle) = pop_value!(span!())? else {
                    return Err(RuntimeErrorKind::InvalidMethodAttach);
                };
                let closure_name =
                    dereference!(allocator.resolve_closure(closure_handle) => span!())?.name;

                let class = {
                    let &Value::Class(handle) = peek_value!()? else {
                        return Err(RuntimeErrorKind::InvalidClassAttach);
                    };
                    dereference!(allocator.resolve_class_mut(handle) => span!())
                }?;
                class.methods.insert(closure_name, closure_handle);
            }
            Opcode::Pop => {
                pop_value!(span!())?;
            }
            Opcode::GetSuper(symbol) => {
                let Value::Class(super_class_handle) = pop_value!(span!())? else {
                    return Err(RuntimeErrorKind::InvalidSuperClass);
                };

                let Value::Instance(instance_handle) = pop_value!(span!())? else {
                    return Err(RuntimeErrorKind::InvalidInstance);
                };

                let super_class =
                    dereference!(allocator.resolve_class(super_class_handle) => span!())?;
                let Some(method) = super_class.methods.get(symbol) else {
                    return Err(RuntimeErrorKind::UndefinedField { name: *symbol });
                };
                let bound_method = allocator.make_method(Method {
                    closure: *method,
                    receiver: instance_handle,
                });
                self.stack.push(Value::Method(bound_method));
            }
            Opcode::GetGlobal(symbol) => {
                let value = self
                    .globals
                    .get(symbol)
                    .ok_or(RuntimeErrorKind::InvalidAccess(*symbol))?;
                self.stack.push(*value);
            }
            Opcode::SetGlobal(symbol) => {
                let value = pop_value!(span!())?;
                if let Entry::Occupied(mut entry) = self.globals.entry(*symbol) {
                    entry.insert(value);
                    self.stack.push(value);
                } else {
                    return Err(RuntimeErrorKind::InvalidAccess(*symbol));
                }
            }
            Opcode::DefineGlobal(symbol) => {
                let value = pop_value!(span!())?;
                self.globals.insert(*symbol, value);
            }
            Opcode::GetLocal(slot_offset) => {
                let slot = slot_offset.to_usize() + base.to_usize();
                self.stack.push(self.stack[slot]);
            }
            Opcode::SetLocal(slot_offset) => {
                let value = peek_value!()?;
                let slot = *slot_offset + base;
                self.stack[slot.to_usize()] = *value;
            }
            Opcode::GetProperty(symbol) => {
                let Value::Instance(handle) = pop_value!(span!())? else {
                    return Err(RuntimeErrorKind::InvalidGet);
                };

                let instance = dereference!(allocator.resolve_instance(handle) => span!())?;
                let class = dereference!(allocator.resolve_class(instance.class) => span!())?;

                if let Some(value) = instance.fields.get(symbol) {
                    self.stack.push(*value);
                } else if let Some(method) = class.methods.get(symbol) {
                    let bound_method = allocator.make_method(Method {
                        closure: *method,
                        receiver: handle,
                    });
                    self.stack.push(Value::Method(bound_method));
                } else {
                    return Err(RuntimeErrorKind::UndefinedField { name: *symbol });
                }
            }
            Opcode::SetProperty(symbol) => {
                let value = pop_value!(span!())?;
                let Value::Instance(handle) = pop_value!(span!())? else {
                    return Err(RuntimeErrorKind::InvalidSet);
                };

                let instance = dereference!(allocator.resolve_instance_mut(handle) => span!())?;

                instance.fields.insert(*symbol, value);
                self.stack.push(value);
            }
            Opcode::GetUpvalue(upvalue_index) => {
                self.debug_stack(allocator, code);
                let handle = base_closure;
                let closure = dereference!(allocator.resolve_closure(handle) => span!())?;
                let upvalue = dereference!(allocator
                    .resolve_upvalue(closure.upvalues[upvalue_index.to_usize()]) => span!())?;
                match *upvalue {
                    Upvalue::Open { slot, .. } => {
                        let value = self.stack[slot.to_usize()];
                        self.stack.push(value);
                    }
                    Upvalue::Closed { ref value } => {
                        self.stack.push(*value);
                    }
                }
            }
            Opcode::SetUpvalue(upvalue_index) => {
                let value = peek_value!()?;
                let handle = base_closure;
                let closure = dereference!(allocator.resolve_closure(handle) => span!())?;
                let upvalue = dereference!(allocator
                    .resolve_upvalue_mut(closure.upvalues[upvalue_index.to_usize()])
                    => span!())?;
                match *upvalue {
                    Upvalue::Open { slot, .. } => {
                        self.stack[slot.to_usize()] = *value;
                    }
                    Upvalue::Closed {
                        value: ref mut closed_value,
                    } => {
                        *closed_value = *value;
                    }
                }
            }
            Opcode::CloseUpvalue => {
                self.close_upvalues(
                    allocator,
                    StackSlot::try_from(self.stack.len() - 1)
                        .expect("stack should never grow past u32."),
                )?;
                pop_value!(span!())?;
            }
            Opcode::Jump(next_offset) => {
                current_frame!()?.ip = ip
                    .checked_add_signed(next_offset.to_i32() as isize)
                    .expect("jumps should not exceed usize.");
                return Ok(ControlFlow::Continue);
            }
            Opcode::JumpIfFalse(next_offset) => {
                let value = peek_value!()?;
                if !value.truthy() {
                    current_frame!()?.ip = ip
                        .checked_add_signed(next_offset.to_i32() as isize)
                        .expect("jumps should not exceed usize.");
                    return Ok(ControlFlow::Continue);
                }
            }
            Opcode::Neg => {
                let value = pop_value!(span!())?;
                self.stack.push(value.neg()?);
            }
            Opcode::Not => {
                let value = pop_value!(span!())?;
                self.stack.push(Value::Bool(!value.truthy()));
            }
            Opcode::Mul => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(lhs.mul(&rhs)?);
            }
            Opcode::Div => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(lhs.div(&rhs)?);
            }
            Opcode::Add => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(Value::add(allocator, &lhs, &rhs)?);
            }
            Opcode::Sub => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(lhs.sub(&rhs)?);
            }
            Opcode::Lt => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(lhs.less_than(&rhs)?);
            }
            Opcode::Le => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(lhs.less_than_or_equal(&rhs)?);
            }
            Opcode::Gt => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(lhs.greater_than(&rhs)?);
            }
            Opcode::Ge => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack.push(lhs.greater_than_or_equal(&rhs)?);
            }
            Opcode::Ne => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack
                    .push(Value::Bool(!Value::eq(allocator, &lhs, &rhs)));
            }
            Opcode::Eq => {
                let rhs = pop_value!(span!())?;
                let lhs = pop_value!(span!())?;
                self.stack
                    .push(Value::Bool(Value::eq(allocator, &lhs, &rhs)));
            }
            Opcode::LoadFalse => self.stack.push(Value::Bool(false)),
            Opcode::LoadTrue => self.stack.push(Value::Bool(true)),
            Opcode::LoadNil => self.stack.push(Value::Nil),
            Opcode::LoadNumber(index) => {
                let number = code.resolver.resolve_number(*index).ok_or_else(|| {
                    RuntimeErrorKind::InvalidSymbol {
                        symbol: index.to_u32(),
                        name: "number",
                    }
                })?;
                self.stack.push(Value::Number(number));
            }
            Opcode::LoadString(symbol) => {
                let string = code.resolver.resolve_string(*symbol).ok_or_else(|| {
                    RuntimeErrorKind::InvalidSymbol {
                        symbol: symbol.raw(),
                        name: "string",
                    }
                })?;
                self.stack
                    .push(Value::String(allocator.make_string(string)));
            }
            Opcode::LoadClosure(index) => {
                let closure = code.resolver.resolve_closure(*index).ok_or_else(|| {
                    RuntimeErrorKind::InvalidSymbol {
                        symbol: index.to_u32(),
                        name: "closure",
                    }
                })?;
                let mut upvalues = Vec::with_capacity(closure.upvalues.len());

                for upvalue in closure.upvalues.iter() {
                    match *upvalue {
                        ProtoUpvalue::Local(stack_slot) => {
                            upvalues.push(self.capture_upvalue(allocator, stack_slot + base)?);
                        }
                        ProtoUpvalue::Upvalue(upvalue_index) => {
                            let frame_closure =
                                dereference!(allocator.resolve_closure(base_closure) => span!())?;
                            upvalues.push(frame_closure.upvalues[upvalue_index.to_usize()]);
                        }
                    }
                }
                self.stack
                    .push(Value::Closure(allocator.make_closure(Closure {
                        name: closure.name,
                        chunk: closure.chunk,
                        arity: closure.arity,
                        upvalues,
                    })));
            }
            Opcode::LoadClass(index) => {
                let class = code.resolver.resolve_class(*index).ok_or_else(|| {
                    RuntimeErrorKind::InvalidSymbol {
                        symbol: index.to_u32(),
                        name: "class",
                    }
                })?;
                self.stack.push(Value::Class(allocator.make_class(Class {
                    name: class.name,
                    methods: HashMap::new(),
                })));
            }
        }

        current_frame!()?.ip = next_instruction_offset;

        Ok(ControlFlow::Continue)
    }

    /// Call a closure.
    #[inline]
    fn call_closure(
        &mut self,
        allocator: &ObjectAllocator,
        callee_slot: StackSlot,
        arity: u8,
        callee: ArenaIndex<Closure>,
        next_ip: usize,
    ) -> Result<ControlFlow, RuntimeErrorKind> {
        let closure = allocator.resolve_closure(callee)?;
        if closure.arity != arity {
            return Err(RuntimeErrorKind::InvalidArgumentCount {
                expected: closure.arity,
                actual: arity,
            });
        }
        let new_frame = CallFrame {
            ip: 0,
            closure: callee,
            base: callee_slot,
        };
        self.call_stack
            .last_mut()
            .ok_or(RuntimeErrorKind::EmptyCallStack)?
            .ip = next_ip;
        self.call_stack.push(new_frame);
        Ok(ControlFlow::Continue)
    }

    /// Capture a local upvalue.
    fn capture_upvalue(
        &mut self,
        allocator: &mut ObjectAllocator,
        slot: StackSlot,
    ) -> Result<ArenaIndex<Upvalue>, RuntimeErrorKind> {
        let mut prev_upvalue_index = None;
        let mut current_upvalue_index = self.open_upvalues;

        while let Some(current_index) = current_upvalue_index {
            let &Upvalue::Open {
                slot: current_slot,
                next,
            } = allocator.resolve_upvalue(current_index)?
            else {
                return Err(RuntimeErrorKind::InvalidOpenUpvalue);
            };

            if current_slot < slot {
                break;
            }

            if current_slot == slot {
                return Ok(current_index);
            }

            prev_upvalue_index = Some(current_index);
            current_upvalue_index = next;
        }

        let new_upvalue = allocator.make_upvalue(Upvalue::Open {
            slot,
            next: current_upvalue_index,
        });

        match prev_upvalue_index {
            // Update previous node's next pointer
            Some(prev_index) => {
                let prev_upvalue = allocator.resolve_upvalue_mut(prev_index)?;
                match *prev_upvalue {
                    Upvalue::Open { ref mut next, .. } => {
                        *next = Some(new_upvalue);
                    }
                    Upvalue::Closed { .. } => {
                        return Err(RuntimeErrorKind::InvalidOpenUpvalue);
                    }
                }
            }
            // Insert at head
            None => {
                self.open_upvalues = Some(new_upvalue);
            }
        }

        Ok(new_upvalue)
    }

    // Can probably take an iterator over the popped values to avoid clones.
    fn close_upvalues(
        &mut self,
        allocator: &mut ObjectAllocator,
        last: StackSlot,
    ) -> Result<(), RuntimeErrorKind> {
        let mut current_upvalue_index = self.open_upvalues;

        while let Some(current_index) = current_upvalue_index {
            let (current_slot, next) = {
                let &Upvalue::Open {
                    slot: current_slot,
                    next,
                } = allocator.resolve_upvalue(current_index)?
                else {
                    return Err(RuntimeErrorKind::InvalidOpenUpvalue);
                };

                if current_slot < last {
                    break;
                }
                (current_slot, next)
            };

            let value = self.stack[current_slot.to_usize()];
            let current_upvalue @ &mut Upvalue::Open { .. } =
                allocator.resolve_upvalue_mut(current_index)?
            else {
                return Err(RuntimeErrorKind::InvalidOpenUpvalue);
            };
            *current_upvalue = Upvalue::Closed { value };
            current_upvalue_index = next;
        }
        self.open_upvalues = current_upvalue_index;
        Ok(())
    }

    /// Debug print the stack.
    fn debug_stack(&self, allocator: &ObjectAllocator, code: &Compilation) {
        if !self.is_debug {
            return;
        }
        tracing::debug!("Stack");
        for (index, value) in self.stack.iter().enumerate() {
            tracing::debug!("\t#{index}: {}", value.resolve(allocator, &code.resolver));
        }

        tracing::debug!("Heap");
        let mut buffer = String::new();
        allocator
            .dump(&mut buffer, &code.resolver)
            .expect("not handling buffer write failure.");
        tracing::debug!("{buffer}");
    }

    /// Mark roots in allocator.
    fn mark_roots(&self, allocator: &mut ObjectAllocator) -> Result<(), RuntimeErrorKind> {
        // Call stack roots.
        for frame in self.call_stack.iter() {
            Value::Closure(frame.closure).mark(allocator)?;
        }

        // Global variable roots.
        #[expect(
            clippy::iter_over_hash_type,
            reason = "garbage collection need not be deterministic."
        )]
        for value in self.globals.values() {
            value.mark(allocator)?;
        }

        // Stack roots.
        for value in self.stack.iter() {
            value.mark(allocator)?;
        }

        // Open upvalues.
        let mut current_upvalue_index = self.open_upvalues;
        while let Some(current_index) = current_upvalue_index {
            Value::Upvalue(current_index).mark(allocator)?;
            let next = {
                let &Upvalue::Open { next, .. } = allocator.resolve_upvalue(current_index)? else {
                    return Err(RuntimeErrorKind::InvalidOpenUpvalue);
                };

                next
            };
            current_upvalue_index = next;
        }

        Ok(())
    }
}

/// Interpret a compiled program.
///
/// # Errors
/// This function will error at the first runtime error it encounters.
pub fn run(
    context: &mut impl IoContext,
    code: &mut Compilation,
    is_debug: bool,
) -> Result<(), RuntimeError> {
    let mut vm = Vm::new(is_debug);

    vm.run(context, code)
}
