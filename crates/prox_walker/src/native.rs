use crate::{
    environment::SharedEnvironment, error::RuntimeError, interpreter::ResolvedAst, value::Value,
};
use core::fmt;
use prox_interner::Symbol;
use std::time::{SystemTime, UNIX_EPOCH};

pub trait NativeFunction: fmt::Debug {
    fn call(
        &self,
        environment: &SharedEnvironment,
        ast: &ResolvedAst,
    ) -> Result<Value, RuntimeError>;
    fn name(&self) -> &'static str;
    fn arity(&self) -> u8;
    fn parameters(&self) -> &'static [Symbol];
}

#[derive(Debug)]
pub(crate) struct Clock;

impl NativeFunction for Clock {
    fn call(
        &self,
        environment: &SharedEnvironment,
        ast: &ResolvedAst,
    ) -> Result<Value, RuntimeError> {
        let _ = environment;
        let _ = ast;
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

    fn parameters(&self) -> &'static [Symbol] {
        &[]
    }
}
