use std::{cell::RefCell, ops::Deref, usize};

use arcstr::ArcStr;
use miette::{bail, Diagnostic, LabeledSpan, MietteDiagnostic, NamedSource, Result, SourceSpan};
use thiserror::Error;

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

    pub fn from<S: Into<SourceSpan>>(&self, from: S) -> SourceSpan {
        let from: SourceSpan = from.into();
        (from.offset()..self.end()).into()
    }

    pub fn to<S: Into<SourceSpan>>(&self, to: S) -> SourceSpan {
        let to: SourceSpan = to.into();
        (self.pos()..(to.offset() + to.len())).into()
    }
}

#[derive(Debug)]
pub struct Context {
    name: ArcStr,
    src: ArcStr,
    errors: RefCell<Vec<MietteDiagnostic>>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Failed to compile")]
pub struct ContextError {
    #[related]
    errors: Vec<MietteDiagnostic>,
    #[source_code]
    miette_src: NamedSource,
}

impl Context {
    pub fn new<N: Into<ArcStr>, S: Into<ArcStr>>(name: N, src: S) -> Self {
        let name: ArcStr = name.into();
        let src: ArcStr = src.into();
        Self {
            name,
            src,
            errors: RefCell::new(vec![]),
        }
    }

    pub fn name(&self) -> ArcStr {
        self.name.clone()
    }

    pub fn error<E: Into<MietteDiagnostic>>(&self, error: E) {
        self.errors.borrow_mut().push(error.into());
    }

    pub fn src(&self) -> ArcStr {
        self.src.clone()
    }

    pub fn eof(&self) -> usize {
        self.src.len() - 1
    }

    pub fn fail(&self) -> ContextError {
        ContextError {
            miette_src: NamedSource::new(self.name.to_string(), self.src.to_string()),
            errors: self.errors.borrow().clone(),
        }
    }

    pub fn result<T>(&self, ok: T) -> Result<T> {
        if self.errors.borrow().is_empty() {
            Ok(ok)
        } else {
            bail!(self.fail())
        }
    }
}

impl Deref for Context {
    type Target = ArcStr;

    fn deref(&self) -> &Self::Target {
        &self.src
    }
}

pub trait AddLabel {
    fn add_label<L: Into<String>, S: Into<SourceSpan>>(self, label: L, span: S) -> Self;
}

impl AddLabel for MietteDiagnostic {
    fn add_label<L: Into<String>, S: Into<SourceSpan>>(self, l: L, s: S) -> Self {
        self.and_label(label(l, s))
    }
}

pub fn label<L: Into<String>, S: Into<SourceSpan>>(label: L, span: S) -> LabeledSpan {
    LabeledSpan::new_with_span(Some(label.into()), span.into())
}
