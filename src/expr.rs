// -----------------------------------------------------------------------------
// Handle
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum Handle {
    Point(Point),
    Span(Span, SpanFocus),
    Zipper(Zipper, ZipperFocus),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Point(Path, Index);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Path(Vec<Step>);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Step(usize);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Index(usize);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Span(String);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum SpanFocus {}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Zipper(String);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub enum ZipperFocus {}

// -----------------------------------------------------------------------------
// Expr
// -----------------------------------------------------------------------------

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq)]
pub struct Expr<L> {
    pub label: L,
    pub kids: Vec<Expr<L>>,
}
