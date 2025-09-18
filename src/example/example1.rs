use crate::{editor::*, expr::*};

type Constructor = String;
type Diagnostic = String;

pub struct Example1 {}

macro_rules! make_edit_menu_option_that_inserts_frag {
    ($label: expr, $frag:expr) => {
        EditMenuOption {
            label: $label,
            edit: |state| {
                let (expr, handle) = state
                    .expr
                    .clone()
                    .insert_fragment_at_handle($frag, state.handle.clone());
                Some(CoreEditorState {
                    expr,
                    handle,
                    clipboard: state.clipboard.clone(),
                })
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

    fn initial_state() -> EditorState<Self> {
        let mut i = 0;

        let mut mk_label = || {
            i += 1;
            ExprLabel {
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

    fn get_edit_menu(_state: &EditorState<Self>) -> EditMenu<Self> {
        EditMenu {
            query: Default::default(),
            options: vec![
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
                    label: format!("copy"),
                    edit: |state| {
                        let frag = state.expr.get_fragment_at_handle(&state.handle)?;
                        Some(CoreEditorState {
                            expr: state.expr.clone(),
                            handle: state.handle.clone(),
                            clipboard: Some(frag),
                        })
                    },
                },
                // TODO: paste
                // TODO: cut
                // TODO: delete
                EditMenuOption {
                    label: format!("id"),
                    edit: |state| Some(state.clone()),
                },
            ],
            index: 0,
        }
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Self::Diagnostic> {
        vec![format!("this is an example diagnostic")]
    }

    fn render_label(ui: &mut egui::Ui, label: &ExprLabel<Self>) -> egui::Response {
        ui.label(egui::RichText::new(label.constructor.clone()))
    }

    fn is_valid_handle(handle: &Handle, expr: &Expr<ExprLabel<Self>>) -> bool {
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
