use crate::{editor::*, expr::*};

type Constructor = String;
type Diagnostic = String;

pub struct Example1 {}

macro_rules! make_edit_menu_option_that_inserts_frag {
    ($label: expr, $frag: expr) => {
        EditMenuOption {
            pattern: EditMenuPattern::Static($label),
            edit: |_query, state| {
                let mut state = state;
                state.handle = state.expr.insert(state.handle, $frag);
                Some(state)
            },
        }
    };
}

impl Example1 {}

impl EditorSpec for Example1 {
    type Constructor = Constructor;
    type Diagnostic = Diagnostic;

    fn name() -> String {
        format!("example1")
    }

    fn initial_state() -> CoreEditorState<Self> {
        let mut i = 0;

        let mut mk_label = || {
            i += 1;
            ExprLabel {
                constructor: format!(" label_{i} "),
                diagnostic: format!(" diagnostic "),
            }
        };

        CoreEditorState::new(Expr::example(&mut mk_label, 2, 3), Default::default())
    }

    fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption<Self>> {
        vec![
            make_edit_menu_option_that_inserts_frag!(
                format!("a"),
                Fragment::Span(Span(vec![Expr {
                    label: ExprLabel {
                        constructor: format!("A"),
                        diagnostic: Default::default(),
                    },
                    kids: Span(vec![]),
                }]))
            ),
            EditMenuOption {
                pattern: EditMenuPattern::Static(format!("copy")),
                edit: |_query, state| {
                    println!("[edit] copy");
                    state.expr.at_handle(&state.handle).map(|frag| {
                        let core = CoreEditorState {
                            expr: state.expr.clone(),
                            handle: state.handle.clone(),
                            clipboard: Some(frag),
                        };
                        println!("core = {core:#?}");
                        core
                    })
                },
            },
            // TODO: paste
            // TODO: cut
            // TODO: delete
            EditMenuOption {
                pattern: EditMenuPattern::Static(format!("id")),
                edit: |_query, state| Some(state.clone()),
            },
        ]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Self::Diagnostic> {
        vec![format!("this is an example diagnostic")]
    }

    fn is_valid_handle(handle: &Handle, expr: &Expr<ExprLabel<Self>>) -> bool {
        match handle {
            Handle::Point(handle) => expr.at_path(&handle.path).kids.0.len() > 0,
            Handle::Span(handle) => expr.at_path(&handle.path).kids.0.len() > 0,
            Handle::Zipper(handle) => {
                !handle.path_m.0.is_empty() && !expr.at_path(&handle.path_i()).kids.0.is_empty()
            }
        }
    }

    fn render_label(ui: &mut egui::Ui, label: &ExprLabel<Self>) -> egui::Response {
        ui.label(egui::RichText::new(label.constructor.clone()))
    }

    fn assemble_rendered_expr(
        _state: &mut EditorState<Self>,
        _ui: &mut egui::Ui,
        _path: &Path,
        _expr: &EditorExpr<Self>,
        _render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_, Self>>)>,
    ) -> egui::Response {
        todo!()
    }
}
