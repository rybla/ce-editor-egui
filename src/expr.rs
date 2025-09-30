use std::fmt::Debug;

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

/// A tooth of an [Expr] around a [Step].
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Tooth<L> {
    pub label: L,
    pub left: Span<L>,
    pub right: Span<L>,
}
