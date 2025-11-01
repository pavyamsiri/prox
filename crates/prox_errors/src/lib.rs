mod parser_cst;
mod parser_resolver;
mod svm_runtime;
mod walker_runtime;

/// Interface to display pretty errors.
pub trait ReportableError {
    /// Write pretty error to buffer.
    fn report(&self, buffer: &mut String, path: &str, text: &str);
}
