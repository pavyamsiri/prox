mod parser_cst;
mod parser_resolver;
mod walker_runtime;

pub trait ReportableError {
    fn report(&self, buffer: &mut String, path: &str, text: &str);
}
