use std::{ops::Deref, usize};

use miette::SourceSpan;


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Src<T> {
    inner: T,
    span: SourceSpan,
}

impl<T> Deref for Src<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> From<Src<T>> for SourceSpan {
    fn from(val: Src<T>) -> Self {
        val.span
    }
}

impl<T> From<&Src<T>> for SourceSpan {
    fn from(val: &Src<T>) -> Self {
        val.span
    }
}

impl<T> Src<T> {
    pub fn new<S: Into<SourceSpan>>(inner: T, span: S) -> Self {
        Self {
            inner,
            span: span.into(),
        }
    }

    pub fn void(inner: T) -> Self {
        Self::new(inner, 0..0)
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn span(&self) -> &SourceSpan {
        &self.span
    }

    pub fn pos(&self) -> usize {
        self.span.offset()
    }

    pub fn len(&self) -> usize {
        self.span.len()
    }

    pub fn end(&self) -> usize {
        self.pos() + self.len()
    }

    pub fn from_pos(&self, from: usize) -> SourceSpan {
        (from..self.end()).into()
    }

    pub fn to_pos(&self, to: usize) -> SourceSpan {
        (self.pos()..to).into()
    }

    pub fn from<S: Into<SourceSpan>>(&self, from: S) -> SourceSpan {
        let from: SourceSpan = from.into();
        self.from_pos(from.offset())
    }

    pub fn to<S: Into<SourceSpan>>(&self, to: S) -> SourceSpan {
        let to: SourceSpan = to.into();
        self.to_pos(to.offset() + to.len())
    }
}
