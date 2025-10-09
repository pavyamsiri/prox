use core::fmt;

/// The IO interface for an interpreter.
pub trait IoContext: fmt::Write {}

pub struct StdoutContext;

impl fmt::Write for StdoutContext {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        println!("{s}");
        Ok(())
    }
}

impl IoContext for StdoutContext {}

pub struct StderrContext;

impl fmt::Write for StderrContext {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        eprintln!("{s}");
        Ok(())
    }
}

impl IoContext for StderrContext {}

pub struct BufferContext {
    data: String,
}

impl fmt::Write for BufferContext {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.data.push_str(s);
        Ok(())
    }
}

impl IoContext for BufferContext {}

impl BufferContext {
    pub fn new() -> Self {
        Self {
            data: String::new(),
        }
    }
    pub fn flush(self) -> String {
        self.data
    }
}
