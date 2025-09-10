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

pub trait EditorSpec<Constructor, Diagnostic> {
    fn name() -> String;

    fn initial_state() -> EditorState<Constructor, Diagnostic>;

    fn get_edit_menu(state: EditorState<Constructor, Diagnostic>) -> EditMenu;

    fn get_diagnostics(state: EditorState<Constructor, Diagnostic>) -> Vec<Diagnostic>;

    fn render_label(ui: &mut egui::Ui, label: ExprLabel<Constructor, Diagnostic>);
}
