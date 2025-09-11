use crate::expr::*;

pub struct ExprLabel<Constructor, Diagnostic> {
    constructor: Constructor,
    diagnostic: Diagnostic,
}

pub struct EditorState<Constructor, Diagnostic> {
    pub initial_expr: Expr<ExprLabel<Constructor, Diagnostic>>,
    pub initial_handle: Handle,
}

pub struct EditMenu {}

pub struct Diagnostic {}

pub trait EditorSpec {
    type Constructor;
    type Diagnostic;

    fn name() -> String;

    fn initial_state() -> EditorState<Self::Constructor, Self::Diagnostic>;

    fn get_edit_menu(state: EditorState<Self::Constructor, Self::Diagnostic>) -> EditMenu;

    fn get_diagnostics(state: EditorState<Self::Constructor, Self::Diagnostic>) -> Vec<Diagnostic>;

    fn render_label(ui: &mut egui::Ui, label: ExprLabel<Self::Constructor, Self::Diagnostic>);
}
