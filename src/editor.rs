use crate::expr::*;

pub struct ExprLabel<Constructor, Diagnostic> {
    pub constructor: Constructor,
    pub diagnostic: Diagnostic,
}

pub struct EditorState<Constructor, Diagnostic> {
    pub expr: Expr<ExprLabel<Constructor, Diagnostic>>,
    pub handle: Handle,
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
