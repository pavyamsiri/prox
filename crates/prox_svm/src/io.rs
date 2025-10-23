use core::fmt;

/// The IO interface for an interpreter.
pub trait IoContext: fmt::Write {}

pub struct StdoutContext;

impl fmt::Write for StdoutContext {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        print!("{s}");
        Ok(())
    }
}

impl IoContext for StdoutContext {}

pub struct StderrContext;

impl fmt::Write for StderrContext {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        eprint!("{s}");
        Ok(())
    }
}

impl IoContext for StderrContext {}

#[derive(Default)]
pub struct BufferContext {
    data: String,
}

impl fmt::Write for BufferContext {
    #[expect(
        clippy::min_ident_chars,
        reason = "keep consistent with trait definition."
    )]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.data.push_str(s);
        Ok(())
    }
}

impl IoContext for BufferContext {}

impl BufferContext {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub const fn with_buffer(data: String) -> Self {
        Self { data }
    }

    #[must_use]
    pub fn flush(self) -> String {
        self.data
    }
}
