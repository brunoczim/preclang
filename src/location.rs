pub type Location = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn anywhere(data: T) -> Self {
        Self { data, span: Span { start: 0, end: 0 } }
    }
}

pub trait SpanlessEq: Eq {
    fn spanless_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanlessEq for Span {
    fn spanless_eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> SpanlessEq for Spanned<T>
where
    T: SpanlessEq,
{
    fn spanless_eq(&self, other: &Self) -> bool {
        self.data.spanless_eq(&other.data)
    }
}
