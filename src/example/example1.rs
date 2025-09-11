use crate::editor;

type Constructor = String;
type Diagnostic = String;

pub struct EditorSpec {}

impl EditorSpec {}

impl editor::EditorSpec for EditorSpec {
    type Constructor = Constructor;
    type Diagnostic = Diagnostic;

    fn name() -> String {
        todo!()
    }

    fn initial_state() -> editor::EditorState<Self::Constructor, Self::Diagnostic> {
        todo!()
    }

    fn get_edit_menu(
        _state: editor::EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> editor::EditMenu {
        todo!()
    }

    fn get_diagnostics(
        _state: editor::EditorState<Self::Constructor, Self::Diagnostic>,
    ) -> Vec<editor::Diagnostic> {
        todo!()
    }

    fn render_label(
        _ui: &mut egui::Ui,
        _label: editor::ExprLabel<Self::Constructor, Self::Diagnostic>,
    ) {
        todo!()
    }
}
