use crate::{error::RuntimeErrorKind, value::Value};
use core::fmt;
use std::time::{SystemTime, UNIX_EPOCH};

pub trait NativeFunction: fmt::Debug {
    fn call(&self, args: &[Value]) -> Result<Value, RuntimeErrorKind>;
    fn name(&self) -> &'static str;
    fn arity(&self) -> u8;
}

#[derive(Debug)]
pub struct Clock;

impl NativeFunction for Clock {
    fn call(&self, args: &[Value]) -> Result<Value, RuntimeErrorKind> {
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
