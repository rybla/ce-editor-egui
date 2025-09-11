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
pub struct Point(pub Path, pub Index);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Path(pub Vec<Step>);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(pub usize);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Index(pub usize);

// -----------------------------------------------------------------------------
// SpanHandle
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanHandle(pub SpanH, pub SpanFocus);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct SpanH {
    pub path: Path,
    pub left: Index,
    pub right: Index,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum SpanFocus {
    Left,
    Right,
}

// -----------------------------------------------------------------------------
// Span
// -----------------------------------------------------------------------------

pub struct Span<L>(pub Vec<Expr<L>>);

// -----------------------------------------------------------------------------
// ZipperHandle
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperHandle(pub ZipperH, pub ZipperFocus);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct ZipperH {
    pub outer_path: Path,
    pub outer_left: Index,
    pub outer_right: Index,
    pub inner_path: Path,
    pub inner_left: Index,
    pub inner_right: Index,
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
