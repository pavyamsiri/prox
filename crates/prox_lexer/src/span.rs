use core::ops;

/// A span over bytes.
#[derive(Debug, Clone, Copy)]
pub struct Span {
    /// The byte index of the start of the span.
    pub start: usize,
    /// The number of bytes in the span.
    pub length: usize,
}

impl Span {
    /// Convert the span into a range.
    pub const fn range(self) -> ops::Range<usize> {
        self.start..(self.start + self.length)
    }
}
