use crate::{editor, expr::*};
use std::fmt::Debug;

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

    fn initial_state() -> editor::EditorState<Self::Constructor, Self::Diagnostic> {
        let mut i = 0;

        let mut mk_label = || {
            i += 1;
            editor::ExprLabel {
                constructor: format!(" label_{i} "),
                diagnostic: format!(" diagnostic "),
            }
        };

        editor::EditorState {
            // expr: Expr::example(&mut mk_label, 2, 6),
            expr: Expr::example(&mut mk_label, 2, 3),
            handle: Handle::default(),
        }
    }

    fn get_edit_menu(
        _state: editor::EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> editor::EditMenu {
        editor::EditMenu::default()
    }

    fn get_diagnostics(
        _state: editor::EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> Vec<Self::Diagnostic> {
        vec![]
    }

    fn render_label(
        ui: &mut egui::Ui,
        label: &editor::ExprLabel<Self::Constructor, Self::Diagnostic>,
    ) -> egui::Response {
        ui.label(egui::RichText::new(label.constructor.clone()))
    }

    fn is_valid_handle<L: Debug>(handle: &Handle, expr: &Expr<L>) -> bool {
        match handle {
            Handle::Point(handle) => expr.at_path(&handle.path).kids.0.len() > 0,
            Handle::Span(handle) => expr.at_path(&handle.span_handle.path).kids.0.len() > 0,
            Handle::Zipper(handle) => {
                !handle.zipper_handle.middle_path.0.is_empty()
                    && !expr
                        .at_path(&handle.zipper_handle.inner_path())
                        .kids
                        .0
                        .is_empty()
            }
        }
    }
}
