#![warn(clippy::all, rust_2018_idioms)]

mod app;
pub(crate) mod editor;
pub(crate) mod example;
pub use app::App;
mod expr;
