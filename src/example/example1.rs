use crate::{editor::*, expr::*};

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
    fn name() -> String {
        "example1".to_owned()
    }

    fn initial_state() -> CoreEditorState {
        let mut i = 0;

        let mut mk_label = || {
            i += 1;
            ExprLabel {
                constructor: Constructor::Literal(format!(" label_{i} ")),
                diagnostic: vec![Diagnostic(" diagnostic ".to_owned())],
            }
        };

        CoreEditorState::new(Expr::example(&mut mk_label, 2, 3), Default::default())
    }

    fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption> {
        vec![
            make_edit_menu_option_that_inserts_frag!(
                format!("a"),
                Fragment::Span(Span(vec![Expr {
                    label: ExprLabel {
                        constructor: Constructor::Literal("A".to_owned()),
                        diagnostic: Default::default(),
                    },
                    kids: Span(vec![]),
                }]))
            ),
            EditMenuOption {
                pattern: EditMenuPattern::Static("copy".to_owned()),
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
                pattern: EditMenuPattern::Static("id".to_owned()),
                edit: |_query, state| Some(state.clone()),
            },
        ]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Diagnostic> {
        vec![Diagnostic("this is an example diagnostic".to_owned())]
    }

    fn is_valid_handle(handle: &Handle, expr: &Expr<ExprLabel>) -> bool {
        match handle {
            Handle::Point(handle) => !expr.at_path(&handle.path).kids.0.is_empty(),
            Handle::Span(handle) => !expr.at_path(&handle.path).kids.0.is_empty(),
            Handle::Zipper(handle) => {
                !handle.path_m.0.is_empty() && !expr.at_path(&handle.path_i()).kids.0.is_empty()
            }
        }
    }

    fn assemble_rendered_expr(
        _ctx: &egui::Context,
        _ui: &mut egui::Ui,
        _state: &mut EditorState<Self>,
        _path: &Path,
        _expr: &EditorExpr,
        _render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        _literal: &str,
    ) {
        todo!()
    }
}
