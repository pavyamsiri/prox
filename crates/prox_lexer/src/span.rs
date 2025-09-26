use core::{cmp, ops};

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
    #[must_use]
    pub const fn range(self) -> ops::Range<usize> {
        self.start..(self.start + self.length)
    }

    /// Merge two spans.
    #[must_use]
    pub fn merge(self, other: Self) -> Self {
        let this_range = self.range();
        let other_range = other.range();
        let start = cmp::min(this_range.start, other_range.start);
        let end = cmp::max(this_range.end, other_range.end);
        let length = end - start;
        Self { start, length }
    }
}
