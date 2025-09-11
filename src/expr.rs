// -----------------------------------------------------------------------------
// Handle
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Handle {
    Point(Point),
    Span(SpanHandle),
    Zipper(ZipperHandle),
}

// -----------------------------------------------------------------------------
// Point
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Point(Path, Index);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Path(Vec<Step>);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(usize);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Index(usize);

// -----------------------------------------------------------------------------
// Span
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle(SpanH, SpanFocus);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanH {
    path: Path,
    left: Index,
    right: Index,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum SpanFocus {
    Left,
    Right,
}

// -----------------------------------------------------------------------------
// Zipper
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandle(ZipperH, ZipperFocus);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperH {
    outer_path: Path,
    outer_left: Index,
    outer_right: Index,
    inner_path: Path,
    inner_left: Index,
    inner_right: Index,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum ZipperFocus {
    OuterLeft,
    InnerLeft,
    OuterRight,
    InnerRight,
}

// -----------------------------------------------------------------------------
// Expr
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Expr<L> {
    pub label: L,
    pub kids: Vec<Expr<L>>,
}
