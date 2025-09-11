use std::fmt::Debug;

// -----------------------------------------------------------------------------
// Handle: Point, SpanHandle, ZipperHandle
// -----------------------------------------------------------------------------

/// A handle for a [Fragment].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Handle {
    Point(Point),
    Span(SpanHandleAndFocus),
    Zipper(ZipperHandleAndFocus),
}

pub type SpanHandleAndFocus = (SpanHandle, SpanFocus);

pub type ZipperHandleAndFocus = (ZipperHandle, ZipperFocus);

impl Default for Handle {
    fn default() -> Self {
        Self::Point(Point::default())
    }
}

/// A point between two [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Point(pub Path, pub Index);

/// A path from the top [Expr] to an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Path(pub Vec<Step>);

/// A step from an [Expr] to one of its kids.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(pub usize);

/// An index between kids, or before the first kid, or after the last kid of an
/// [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Default)]
pub struct Index(pub usize);

/// A handle for a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle {
    pub path: Path,
    pub left: Index,
    pub right: Index,
}

/// A focus of a [SpanHandle].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum SpanFocus {
    Left,
    Right,
}

/// A hanle for a [Zipper].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandle {
    pub outer_path: Path,
    pub outer_left: Index,
    pub outer_right: Index,
    pub inner_path: Path,
    pub inner_left: Index,
    pub inner_right: Index,
}

/// A focus fro a [ZipperHandle].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum ZipperFocus {
    OuterLeft,
    InnerLeft,
    OuterRight,
    InnerRight,
}

// -----------------------------------------------------------------------------
// Fragment: Expr, Span, Zipper
// -----------------------------------------------------------------------------

/// A fragment of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Fragment<L> {
    Span(Span<L>),
    Zipper(Expr<L>),
}

/// An expression of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Expr<L> {
    pub label: L,
    pub kids: Span<L>,
}

impl<L: Debug> Expr<L> {
    pub fn at_step<'a>(&'a self, step: &Step) -> &'a Expr<L> {
        self.kids.at_step(step)
    }

    pub fn at_path<'a>(&'a self, path: &Path) -> &'a Expr<L> {
        let mut e = self;
        for step in path.0.iter() {
            e = e.at_step(step)
        }
        e
    }

    pub fn example<F>(mk_label: &mut F, width: usize, depth: usize) -> Self
    where
        F: FnMut() -> L,
    {
        Expr {
            label: mk_label(),
            kids: Span(if depth == 0 {
                vec![]
            } else {
                (0..width)
                    .map(|_| Self::example(mk_label, width, depth - 1))
                    .collect()
            }),
        }
    }
}

/// A span of [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span<L>(pub Vec<Expr<L>>);

impl<L: Debug> Span<L> {
    pub fn at_step<'a>(&'a self, step: &Step) -> &'a Expr<L> {
        self.0
            .get(step.0)
            .unwrap_or_else(|| panic!("step out of bounds: span = {self:?}; step = {step:?}"))
    }
}

/// A tooth of an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub left: Vec<Expr<L>>,
    pub right: Vec<Expr<L>>,
}

/// A context around an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ExprContext<L>(pub Vec<Tooth<L>>);

/// A context around a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanContext<L> {
    pub outer: ExprContext<L>,
    pub inner: Tooth<L>,
}

/// A zipper between two [SpanHandle]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper<L> {
    pub outer_left: Vec<Expr<L>>,
    pub outer_right: Vec<Expr<L>>,
    pub inner: SpanContext<L>,
}
