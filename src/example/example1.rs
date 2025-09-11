use crate::{editor, expr::*};

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
                constructor: format!("label_{i}"),
                diagnostic: format!("diagnostic"),
            }
        };

        editor::EditorState {
            expr: Expr::example(&mut mk_label, 4, 4),
            handle: Handle::default(),
        }
    }

    fn get_edit_menu(
        _state: editor::EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> editor::EditMenu {
        todo!()
    }

    fn get_diagnostics(
        _state: editor::EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> Vec<Self::Diagnostic> {
        todo!()
    }

    fn render_label(
        _ui: &mut egui::Ui,
        _label: editor::ExprLabel<Self::Constructor, Self::Diagnostic>,
    ) {
        todo!()
    }
}
