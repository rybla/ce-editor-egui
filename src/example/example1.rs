use crate::{
    editor::{self, EditorState},
    expr::*,
};

type Constructor = String;
type Diagnostic = String;

pub struct EditorSpec {}

impl EditorSpec {}

impl editor::EditorSpec for EditorSpec {
    type Constructor = Constructor;
    type Diagnostic = Diagnostic;

    fn name() -> String {
        format!("example1")
    }

    fn initial_state() -> editor::EditorState<Self> {
        let mut i = 0;

        let mut mk_label = || {
            i += 1;
            editor::ExprLabel {
                constructor: format!(" label_{i} "),
                diagnostic: format!(" diagnostic "),
            }
        };

        EditorState::new(
            // Expr::example(&mut mk_label, 2, 6),
            Expr::example(&mut mk_label, 2, 3),
            Default::default(),
        )
    }

    fn get_edit_menu(_state: &editor::EditorState<Self>) -> editor::EditMenu<Self> {
        editor::EditMenu::default()
    }

    fn get_diagnostics(_state: editor::EditorState<Self>) -> Vec<Self::Diagnostic> {
        vec![]
    }

    fn render_label(ui: &mut egui::Ui, label: &editor::ExprLabel<Self>) -> egui::Response {
        ui.label(egui::RichText::new(label.constructor.clone()))
    }

    fn is_valid_handle(handle: &Handle, expr: &Expr<editor::ExprLabel<Self>>) -> bool {
        match handle {
            Handle::Point(handle) => expr.at_expr(&handle.path).1.kids.0.len() > 0,
            Handle::Span(handle) => expr.at_expr(&handle.span_handle.path).1.kids.0.len() > 0,
            Handle::Zipper(handle) => {
                !handle.zipper_handle.middle_path.0.is_empty()
                    && !expr
                        .at_expr(&handle.zipper_handle.inner_path())
                        .1
                        .kids
                        .0
                        .is_empty()
            }
        }
    }
}
