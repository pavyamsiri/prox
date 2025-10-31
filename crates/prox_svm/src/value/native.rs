//! The native function interface and runtime builtins.

use crate::{error::RuntimeErrorKind, gc::ObjectAllocator, value::Value};
use compact_str::ToCompactString as _;
use core::fmt;
use prox_bytecode::pool::ConstantInterner;
use std::time::{SystemTime, UNIX_EPOCH};

/// The native function interface.
pub(crate) trait NativeFunction: fmt::Debug {
    /// Call the native function given its arguments.
    fn call(
        &self,
        allocator: &mut ObjectAllocator,
        interner: &ConstantInterner,
        args: &[Value],
    ) -> Result<Value, RuntimeErrorKind>;
    /// The name of the function.
    fn name(&self) -> &'static str;
    /// The arity of the function.
    fn arity(&self) -> u8;
}

/// The clock builtin.
#[derive(Debug)]
pub(crate) struct Clock;

impl NativeFunction for Clock {
    fn call(
        &self,
        _allocator: &mut ObjectAllocator,
        _interner: &ConstantInterner,
        args: &[Value],
    ) -> Result<Value, RuntimeErrorKind> {
        assert_eq!(
            args.len(),
            self.arity() as usize,
            "should've checked before calling."
        );
        let now = SystemTime::now();
        let duration_since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
        #[expect(clippy::cast_precision_loss, reason = "no way to avoid this.")]
        Ok(Value::Number(duration_since_epoch.as_secs() as f64))
    }

    fn name(&self) -> &'static str {
        "clock"
    }

    fn arity(&self) -> u8 {
        0
    }
}

/// Stringify a value.
#[derive(Debug)]
pub(crate) struct Stringify;

impl NativeFunction for Stringify {
    fn call(
        &self,
        allocator: &mut ObjectAllocator,
        interner: &ConstantInterner,
        args: &[Value],
    ) -> Result<Value, RuntimeErrorKind> {
        assert_eq!(
            args.len(),
            self.arity() as usize,
            "should've checked before calling."
        );
        let value = args[0];

        let string = value.resolve(allocator, interner).to_compact_string();
        Ok(Value::String(allocator.make_string_from_owned(string)))
    }

    fn name(&self) -> &'static str {
        "str"
    }

    fn arity(&self) -> u8 {
        1
    }
}
