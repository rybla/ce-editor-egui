use std::fmt::Debug;

use crate::utility::extract_from_vec_at_index;

// -----------------------------------------------------------------------------
// Index
// -----------------------------------------------------------------------------

/// An index between kids, or left the first kid, or right of the last kid of an
/// [Expr].
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Index(pub usize);

// -----------------------------------------------------------------------------
// Step
// -----------------------------------------------------------------------------

/// A step from an [Expr] to one of its kids.
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(pub usize);

// -----------------------------------------------------------------------------
// Path
// -----------------------------------------------------------------------------

/// A path from the top [Expr] to an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Path(pub Vec<Step>);

// -----------------------------------------------------------------------------
// Point
// -----------------------------------------------------------------------------

/// A point between two [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Point {
    pub path: Path,
    pub index: Index,
}

// -----------------------------------------------------------------------------
// Handle
// -----------------------------------------------------------------------------

/// A handle for a [Fragment].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Handle {
    Point(Point),
    Span(SpanHandle),
    Zipper(ZipperHandle),
}

// -----------------------------------------------------------------------------
// SpanHandle
// -----------------------------------------------------------------------------

/// A handle for a [Span].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle {
    pub path: Path,
    pub left: Index,
    pub right: Index,
}

// -----------------------------------------------------------------------------
// ZipperHandle
// -----------------------------------------------------------------------------

/// A handle for a [Zipper].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandle {
    pub outer_path: Path,
    pub outer_left: Index,
    pub outer_right: Index,
    pub middle_path: Path,
    pub inner_left: Index,
    pub inner_right: Index,
}

// -----------------------------------------------------------------------------
// Expr
// -----------------------------------------------------------------------------

/// An expression of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Expr<L> {
    pub label: L,
    pub kids: Span<L>,
}

impl<L: Debug + Clone> Expr<L> {
    pub fn at_path(&self, path: &Path) -> &Self {
        let mut e = self;
        for s in path.0.iter() {
            e = e.at_step(s);
        }
        e
    }

    pub fn at_path_owned(self, path: &Path) -> (Context<L>, Self) {
        let mut ctx: Context<L> = Context::empty();
        let mut e = self;
        for s in path.0.iter() {
            let (th, e_kid) = e.at_step_owned(s);
            ctx.0.push(th);
            e = e_kid;
        }
        (ctx, e)
    }

    fn at_step(&self, step: &Step) -> &Self {
        self.kids.at_step(step)
    }

    fn at_step_owned(self, s: &Step) -> (Tooth<L>, Expr<L>) {
        let (left, middle, right) = self.kids.at_step_owned(s);
        (
            Tooth {
                label: self.label,
                left: left,
                right: right,
            },
            middle,
        )
    }
}

// -----------------------------------------------------------------------------
// Fragment
// -----------------------------------------------------------------------------

/// A fragment of syntax.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Fragment<L> {
    Span(Span<L>),
    Zipper(Zipper<L>),
}

// -----------------------------------------------------------------------------
// Span
// -----------------------------------------------------------------------------

/// A span of [Expr]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span<L>(pub Vec<Expr<L>>);

impl<L: Debug + Clone> Span<L> {
    pub fn assert_step_in_bounds(&self, s: Step) {
        assert!(s.0 < self.0.len())
    }

    pub fn assert_index_in_bounds(&self, i: &Index) {
        assert!(i.0 <= self.0.len())
    }

    pub fn at_step(&self, step: &Step) -> &Expr<L> {
        self.0.get(step.0).unwrap()
    }

    pub fn at_step_owned(self, s: &Step) -> (Self, Expr<L>, Self) {
        let (left, middle, right) = extract_from_vec_at_index(self.0, s.0).unwrap();
        (Span(left), middle, Span(right))
    }
}

// -----------------------------------------------------------------------------
// Zipper
// -----------------------------------------------------------------------------

/// A zipper between two [SpanHandle]s.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper<L> {
    pub outer_left: Span<L>,
    pub outer_right: Span<L>,
    pub middle: Context<L>,
    pub inner_left: Span<L>,
    pub inner_right: Span<L>,
}

// -----------------------------------------------------------------------------
// ExprContext
// -----------------------------------------------------------------------------

/// A context around an [Expr].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Context<L>(pub Vec<Tooth<L>>);

impl<L: Debug + Clone> Context<L> {
    fn empty() -> Self {
        Self(vec![])
    }
}

/// A tooth of an [Expr] around a [Step].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub label: L,
    pub left: Span<L>,
    pub right: Span<L>,
}
