mod parser_cst;
mod parser_resolver;

use core::ops;

pub trait ReportableError {
    fn report(&self, buffer: &mut String, path: &str, text: &str);
}
