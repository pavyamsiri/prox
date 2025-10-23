mod error;
mod gc;
pub mod io;
mod value;

use crate::{
    error::{RuntimeError, RuntimeErrorKind},
    gc::{ArenaIndex, ValueAllocator},
    io::IoContext,
    value::{
        Class, Closure, Instance, Method, Upvalue,
        native::{Clock, NativeFunction as _},
    },
};
use prox_bytecode::{
    Opcode,
    chunk::{Chunk, ChunkId},
};
use prox_bytecode::{StackSlot, function::Upvalue as ProtoUpvalue};
use prox_compiler::Compilation;
use prox_interner::Symbol;
use prox_span::Span;
use std::collections::{HashMap, hash_map::Entry};
use value::Value;

#[derive(Debug)]
enum ControlFlow {
    Continue,
    Done,
}

#[derive(Debug, Clone)]
struct CallFrame {
    ip: usize,
    closure: ArenaIndex<Closure>,
    base: StackSlot,
}

struct Vm {
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<Symbol, Value>,

    open_upvalues: Option<ArenaIndex<Upvalue>>,
}

macro_rules! dereference {
    ($option:expr => $span:expr) => {
        $option.ok_or(RuntimeError {
            kind: RuntimeErrorKind::InvalidDereference,
            span: $span,
        })
    };
}

impl Vm {
    fn new() -> Self {
        Self {
            call_stack: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: None,
        }
    }

    fn run(
        &mut self,
        context: &mut impl IoContext,
        code: &mut Compilation,
    ) -> Result<(), RuntimeError> {
        let mut allocator = ValueAllocator::new();
        // Native
        {
            let clock = Clock;
            let clock_sym = code.resolver.intern(clock.name());
            self.globals.insert(
                clock_sym,
                Value::Native(allocator.make_native(Box::new(clock))),
            );
        }

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

        self.debug_stack(&allocator, code);
        let mut i = 0;
        while matches!(
            self.interpret(&mut allocator, context, code)?,
            ControlFlow::Continue
        ) {
            self.debug_stack(&allocator, code);
            i += 1;
        }

        Ok(())
    }

    #[expect(clippy::too_many_lines, reason = "for now")]
    fn interpret(
        &mut self,
        allocator: &mut ValueAllocator,
        context: &mut impl IoContext,
        code: &mut Compilation,
    ) -> Result<ControlFlow, RuntimeError> {
        macro_rules! pop_value {
            ($span:expr) => {
                self.stack.pop().ok_or(RuntimeError {
                    kind: RuntimeErrorKind::EmptyStack,
                    span: $span,
                })
            };
        }

        macro_rules! peek_value {
            ($slot:expr => $span:expr) => {
                self.stack.get($slot).ok_or(RuntimeError {
                    kind: RuntimeErrorKind::EmptyStack,
                    span: $span,
                })
            };

            ($span:expr) => {
                self.stack.last().ok_or(RuntimeError {
                    kind: RuntimeErrorKind::EmptyStack,
                    span: $span,
                })
            };
        }

        macro_rules! pop_frame {
            ($span:expr) => {
                self.call_stack.pop().ok_or(RuntimeError {
                    kind: RuntimeErrorKind::EmptyCallStack,
                    span: $span,
                })
            };
        }

        macro_rules! current_frame {
            ($span:expr) => {
                self.call_stack.last_mut().ok_or(RuntimeError {
                    kind: RuntimeErrorKind::EmptyCallStack,
                    span: $span,
                })
            };
        }

        let call_frame = self.call_stack.last().cloned().ok_or(RuntimeError {
            kind: RuntimeErrorKind::EmptyCallStack,
            span: Span {
                start: 0,
                length: 1,
            },
        })?;
        let base = call_frame.base;
        let chunk = {
            let closure = allocator
                .resolve_closure(call_frame.closure)
                .ok_or(RuntimeError {
                    kind: RuntimeErrorKind::InvalidDereference,
                    span: Span {
                        start: 0,
                        length: 1,
                    },
                })?;
            code.pool.get_chunk(closure.chunk).ok_or(RuntimeError {
                kind: RuntimeErrorKind::InvalidChunkDereference,
                span: Span {
                    start: 0,
                    length: 1,
                },
            })
        }?;

        if chunk.at(call_frame.ip).is_empty() {
            return Ok(ControlFlow::Done);
        }

        let Ok((offset, inst)) = Opcode::decode(chunk.at(call_frame.ip)) else {
            tracing::debug!(
                "Got no valid instruction from {:#?}",
                &chunk.at(call_frame.ip)[0..10]
            );
            let (span, _) = Self::get_span_and_line(chunk, call_frame.ip);
            return Err(RuntimeError {
                kind: RuntimeErrorKind::Segfault,
                span,
            });
        };

        let (span, line) = Self::get_span_and_line(chunk, call_frame.ip);
        {
            let mut buffer = String::new();

            inst.format(&mut buffer, "", &code.resolver, call_frame.ip)
                .expect("not handling buffer write failure.");
            tracing::debug!(
                "Executing {:04x}: {buffer} with new offset = {offset} @ {}..{} [line {}]",
                call_frame.ip,
                span.start,
                span.end(),
                line,
            );
        }
        match inst {
            Opcode::Call(arity) => {
                let slot = self.stack.len() - 1 - (arity as usize);
                let callee = &self.stack[slot];
                match *callee {
                    Value::Closure(index) => {
                        return self.call_closure(
                            allocator,
                            StackSlot::try_from(slot).expect("stack can't grow past u32."),
                            arity,
                            index,
                            call_frame.ip + offset,
                            span,
                        );
                    }
                    Value::Method(index) => {
                        let method = dereference!(allocator.resolve_method(index) => span)?;
                        self.stack[slot] = Value::Instance(method.receiver);
                        return self.call_closure(
                            allocator,
                            StackSlot::try_from(slot).expect("stack can't grow past u32."),
                            arity,
                            method.closure,
                            call_frame.ip + offset,
                            span,
                        );
                    }
                    Value::Native(index) => {
                        let closure = dereference!(allocator.resolve_native(index) => span)?;
                        if closure.arity() != arity {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::InvalidArgumentCount {
                                    expected: closure.arity(),
                                    actual: arity,
                                },
                                span,
                            });
                        }
                        let args = &self.stack[slot + 1..];
                        let result = closure
                            .call(args)
                            .map_err(|kind| RuntimeError { kind, span })?;
                        self.stack.push(result);
                    }
                    Value::Class(index) => {
                        self.stack[slot] = Value::Instance(allocator.make_instance(Instance {
                            class: index,
                            fields: HashMap::new(),
                        }));
                        let class = dereference!(allocator.resolve_class(index) => span)?;
                        if let Some(init) = class.methods.get(&code.resolver.intern("init")) {
                            return self.call_closure(
                                allocator,
                                StackSlot::try_from(slot).expect("stack can't grow past u32."),
                                arity,
                                *init,
                                call_frame.ip + offset,
                                span,
                            );
                        } else if arity != 0 {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::InvalidArgumentCount {
                                    expected: 0,
                                    actual: arity,
                                },
                                span,
                            });
                        }
                    }
                    _ => {
                        return Err(RuntimeError {
                            kind: RuntimeErrorKind::InvalidCallee,
                            span,
                        });
                    }
                }
            }
            Opcode::Print => {
                let value = pop_value!(span)?;
                writeln!(context, "{}", value.resolve(allocator, &code.resolver)).map_err(
                    |_err| RuntimeError {
                        kind: RuntimeErrorKind::Io,
                        span,
                    },
                )?;
            }
            Opcode::Return => {
                let result = pop_value!(span)?;
                let popped_frame = pop_frame!(span)?;
                // Pop the main function.
                if self.call_stack.is_empty() {
                    pop_value!(span)?;
                    return Ok(ControlFlow::Done);
                }
                self.close_upvalues(allocator, popped_frame.base)
                    .map_err(|kind| RuntimeError { kind, span })?;
                self.stack.truncate(popped_frame.base.to_usize());
                tracing::debug!("returning {}", result.resolve(allocator, &code.resolver));
                self.stack.push(result);
                return Ok(ControlFlow::Continue);
            }
            Opcode::Inherit => {
                let Value::Class(sub_class_handle) = pop_value!(span)? else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidSubClass,
                        span,
                    });
                };
                let &Value::Class(super_class_handle) = peek_value!(span)? else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidSuperClass,
                        span,
                    });
                };

                let super_class =
                    dereference!(allocator.resolve_class(super_class_handle) => span)?;

                let super_methods: Vec<_> = super_class
                    .methods
                    .iter()
                    .map(|(key, val)| (*key, *val))
                    .collect();
                let sub_class =
                    dereference!(allocator.resolve_class_mut(sub_class_handle) => span)?;
                sub_class.methods.extend(super_methods);
            }
            Opcode::AttachMethod => {
                let Value::Closure(closure_handle) = pop_value!(span)? else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidMethodAttach,
                        span,
                    });
                };
                let closure_name =
                    dereference!(allocator.resolve_closure(closure_handle) =>span)?.name;

                let class = {
                    let &Value::Class(handle) = peek_value!(span)? else {
                        return Err(RuntimeError {
                            kind: RuntimeErrorKind::InvalidClassAttach,
                            span,
                        });
                    };
                    dereference!(allocator.resolve_class_mut(handle) => span)
                }?;
                class.methods.insert(closure_name, closure_handle);
            }
            Opcode::Pop => {
                pop_value!(span)?;
            }
            Opcode::GetSuper(symbol) => {
                let Value::Class(super_class_handle) = pop_value!(span)? else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidSuperClass,
                        span,
                    });
                };

                let Value::Instance(instance_handle) = pop_value!(span)? else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidInstance,
                        span,
                    });
                };

                let super_class = dereference!(allocator.resolve_class(super_class_handle) =>span)?;
                let Some(method) = super_class.methods.get(&symbol) else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::UndefinedField { name: symbol },
                        span,
                    });
                };
                let bound_method = allocator.make_method(Method {
                    closure: *method,
                    receiver: instance_handle,
                });
                self.stack.push(Value::Method(bound_method));
            }
            Opcode::GetGlobal(symbol) => {
                let value = self.globals.get(&symbol).ok_or(RuntimeError {
                    kind: RuntimeErrorKind::InvalidAccess(symbol),
                    span,
                })?;
                self.stack.push(value.clone());
            }
            Opcode::SetGlobal(symbol) => {
                let value = pop_value!(span)?;
                if let Entry::Occupied(mut entry) = self.globals.entry(symbol) {
                    entry.insert(value.clone());
                    self.stack.push(value);
                } else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(symbol),
                        span,
                    });
                }
            }
            Opcode::DefineGlobal(symbol) => {
                let value = pop_value!(span)?;
                self.globals.insert(symbol, value);
            }
            Opcode::GetLocal(slot_offset) => {
                let slot = slot_offset.to_usize() + base.to_usize();
                self.stack.push(self.stack[slot].clone());
            }
            Opcode::SetLocal(slot_offset) => {
                let value = peek_value!(span)?;
                let slot = slot_offset + base;
                self.stack[slot.to_usize()] = value.clone();
            }
            Opcode::GetProperty(symbol) => {
                let Value::Instance(handle) = pop_value!(span)? else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidGet,
                        span,
                    });
                };

                let instance = dereference!(allocator.resolve_instance(handle) => span)?;
                let class = dereference!(allocator.resolve_class(instance.class) => span)?;

                if let Some(value) = instance.fields.get(&symbol) {
                    self.stack.push(value.clone());
                } else if let Some(method) = class.methods.get(&symbol) {
                    let bound_method = allocator.make_method(Method {
                        closure: *method,
                        receiver: handle,
                    });
                    self.stack.push(Value::Method(bound_method));
                } else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::UndefinedField { name: symbol },
                        span,
                    });
                }
            }
            Opcode::SetProperty(symbol) => {
                let value = pop_value!(span)?;
                let Value::Instance(handle) = pop_value!(span)? else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidSet,
                        span,
                    });
                };

                let instance = dereference!(allocator.resolve_instance_mut(handle) => span)?;

                instance.fields.insert(symbol, value.clone());
                self.stack.push(value);
            }
            Opcode::GetUpvalue(upvalue_index) => {
                tracing::debug!("current frame = {call_frame:#?}");
                self.debug_stack(allocator, code);
                let handle = call_frame.closure;
                let closure = dereference!(allocator.resolve_closure(handle) => span)?;
                let upvalue = dereference!(allocator
                    .resolve_upvalue(closure.upvalues[upvalue_index.to_usize()]) => span)?;
                match *upvalue {
                    Upvalue::Open { slot, .. } => {
                        let value = self.stack[slot.to_usize()].clone();
                        tracing::debug!("Pushing {}", value.resolve(allocator, &code.resolver));
                        self.stack.push(value);
                    }
                    Upvalue::Closed { ref value } => {
                        self.stack.push(value.clone());
                    }
                }
            }
            Opcode::SetUpvalue(upvalue_index) => {
                let value = peek_value!(span)?.clone();
                let Value::Closure(handle) = self.stack[call_frame.base.to_usize()] else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidCallFrame,
                        span,
                    });
                };
                let closure = dereference!(allocator.resolve_closure(handle) => span)?;
                let upvalue = dereference!(allocator
                    .resolve_upvalue_mut(closure.upvalues[upvalue_index.to_usize()])
                    => span)?;
                match *upvalue {
                    Upvalue::Open { slot, .. } => {
                        self.stack[slot.to_usize()] = value;
                    }
                    Upvalue::Closed {
                        value: ref mut closed_value,
                    } => {
                        *closed_value = value;
                    }
                }
            }
            Opcode::CloseUpvalue => {
                self.close_upvalues(
                    allocator,
                    StackSlot::try_from(self.stack.len() - 1)
                        .expect("stack should never grow past u32."),
                )
                .map_err(|kind| RuntimeError { kind, span })?;
                pop_value!(span)?;
            }
            Opcode::Jump(next_offset) => {
                current_frame!(span)?.ip = call_frame
                    .ip
                    .checked_add_signed(next_offset.to_i32() as isize)
                    .expect("jumps should not exceed usize.");
                return Ok(ControlFlow::Continue);
            }
            Opcode::JumpIfFalse(next_offset) => {
                let value = peek_value!(span)?;
                if !value.truthy() {
                    current_frame!(span)?.ip = call_frame
                        .ip
                        .checked_add_signed(next_offset.to_i32() as isize)
                        .expect("jumps should not exceed usize.");
                    return Ok(ControlFlow::Continue);
                }
            }
            Opcode::Neg => {
                let value = pop_value!(span)?;
                self.stack
                    .push(value.neg().map_err(|kind| RuntimeError { kind, span })?);
            }
            Opcode::Not => {
                let value = pop_value!(span)?;
                self.stack.push(Value::Bool(!value.truthy()));
            }
            Opcode::Mul => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack
                    .push(lhs.mul(&rhs).map_err(|kind| RuntimeError { kind, span })?);
            }
            Opcode::Div => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack
                    .push(lhs.div(&rhs).map_err(|kind| RuntimeError { kind, span })?);
            }
            Opcode::Add => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack.push(
                    Value::add(allocator, &lhs, &rhs)
                        .map_err(|kind| RuntimeError { kind, span })?,
                );
            }
            Opcode::Sub => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack
                    .push(lhs.sub(&rhs).map_err(|kind| RuntimeError { kind, span })?);
            }
            Opcode::Lt => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack.push(
                    lhs.less_than(&rhs)
                        .map_err(|kind| RuntimeError { kind, span })?,
                );
            }
            Opcode::Le => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack.push(
                    lhs.less_than_or_equal(&rhs)
                        .map_err(|kind| RuntimeError { kind, span })?,
                );
            }
            Opcode::Gt => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack.push(
                    lhs.greater_than(&rhs)
                        .map_err(|kind| RuntimeError { kind, span })?,
                );
            }
            Opcode::Ge => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack.push(
                    lhs.greater_than_or_equal(&rhs)
                        .map_err(|kind| RuntimeError { kind, span })?,
                );
            }
            Opcode::Ne => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack
                    .push(Value::Bool(!Value::eq(allocator, &lhs, &rhs)));
            }
            Opcode::Eq => {
                let rhs = pop_value!(span)?;
                let lhs = pop_value!(span)?;
                self.stack
                    .push(Value::Bool(Value::eq(allocator, &lhs, &rhs)));
            }
            Opcode::LoadFalse => self.stack.push(Value::Bool(false)),
            Opcode::LoadTrue => self.stack.push(Value::Bool(true)),
            Opcode::LoadNil => self.stack.push(Value::Nil),
            Opcode::LoadNumber(index) => {
                let number = dereference!(code.resolver.resolve_number(index) => span)?;
                self.stack.push(Value::Number(number));
            }
            Opcode::LoadString(symbol) => {
                let string = dereference!(code.resolver.resolve_string(symbol) => span)?;
                self.stack
                    .push(Value::String(allocator.make_string(string)));
            }
            Opcode::LoadClosure(index) => {
                let closure = dereference!(code.resolver.resolve_closure(index) => span)?;
                let mut upvalues = Vec::with_capacity(closure.upvalues.len());

                for upvalue in closure.upvalues.iter() {
                    match *upvalue {
                        ProtoUpvalue::Local(stack_slot) => {
                            upvalues.push(
                                self.capture_upvalue(allocator, stack_slot + call_frame.base)
                                    .map_err(|kind| RuntimeError { kind, span })?,
                            );
                        }
                        ProtoUpvalue::Upvalue(upvalue_index) => {
                            let frame_closure = dereference!(allocator.resolve_closure(call_frame.closure) => span)?;
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
                let class = dereference!(code.resolver.resolve_class(index) => span)?;
                self.stack.push(Value::Class(allocator.make_class(Class {
                    name: class.name,
                    methods: HashMap::new(),
                })));
            }
        }

        current_frame!(span)?.ip = call_frame.ip + offset;

        Ok(ControlFlow::Continue)
    }

    fn call_closure(
        &mut self,
        allocator: &ValueAllocator,
        callee_slot: StackSlot,
        arity: u8,
        callee: ArenaIndex<Closure>,
        next_ip: usize,
        span: Span,
    ) -> Result<ControlFlow, RuntimeError> {
        let closure = dereference!(allocator.resolve_closure(callee) => span)?;
        if closure.arity != arity {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidArgumentCount {
                    expected: closure.arity,
                    actual: arity,
                },
                span,
            });
        }
        let new_frame = CallFrame {
            ip: 0,
            closure: callee,
            base: callee_slot,
        };
        self.call_stack
            .last_mut()
            .ok_or(RuntimeError {
                kind: RuntimeErrorKind::EmptyCallStack,
                span,
            })?
            .ip = next_ip;
        self.call_stack.push(new_frame);
        Ok(ControlFlow::Continue)
    }

    fn capture_upvalue(
        &mut self,
        allocator: &mut ValueAllocator,
        slot: StackSlot,
    ) -> Result<ArenaIndex<Upvalue>, RuntimeErrorKind> {
        let mut prev_upvalue_index = None;
        let mut current_upvalue_index = self.open_upvalues;

        while let Some(current_index) = current_upvalue_index {
            let &Upvalue::Open {
                slot: current_slot,
                next,
            } = allocator
                .resolve_upvalue(current_index)
                .ok_or(RuntimeErrorKind::InvalidDereference)?
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
                let prev_upvalue = allocator
                    .resolve_upvalue_mut(prev_index)
                    .ok_or(RuntimeErrorKind::InvalidDereference)?;
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
        allocator: &mut ValueAllocator,
        last: StackSlot,
    ) -> Result<(), RuntimeErrorKind> {
        let mut current_upvalue_index = self.open_upvalues;

        while let Some(current_index) = current_upvalue_index {
            let (current_slot, next) = {
                let &Upvalue::Open {
                    slot: current_slot,
                    next,
                } = allocator
                    .resolve_upvalue(current_index)
                    .ok_or(RuntimeErrorKind::InvalidDereference)?
                else {
                    return Err(RuntimeErrorKind::InvalidOpenUpvalue);
                };

                if current_slot < last {
                    break;
                }
                (current_slot, next)
            };

            let value = self.stack[current_slot.to_usize()].clone();
            let current_upvalue @ &mut Upvalue::Open { .. } = allocator
                .resolve_upvalue_mut(current_index)
                .ok_or(RuntimeErrorKind::InvalidDereference)?
            else {
                return Err(RuntimeErrorKind::InvalidOpenUpvalue);
            };
            *current_upvalue = Upvalue::Closed { value };
            current_upvalue_index = next;
        }
        self.open_upvalues = current_upvalue_index;
        Ok(())
    }

    fn debug_stack(&self, allocator: &ValueAllocator, code: &Compilation) {
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

    fn get_span_and_line(chunk: &Chunk, ip: usize) -> (Span, usize) {
        fn try_get(chunk: &Chunk, ip: usize) -> Option<(Span, usize)> {
            let index = chunk.starts.iter().position(|&start| start == ip)?;
            let span = chunk.spans.get(index)?;
            let line = chunk.lines.get(index)?;

            Some((*span, *line))
        }

        try_get(chunk, ip).unwrap_or((
            Span {
                start: 0,
                length: 1,
            },
            1,
        ))
    }
}

/// Interpret a compiled program.
///
/// # Errors
/// This function will error at the first runtime error it encounters.
pub fn run(context: &mut impl IoContext, code: &mut Compilation) -> Result<(), RuntimeError> {
    let mut vm = Vm::new();

    vm.run(context, code)
}
