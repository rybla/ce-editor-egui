use crate::{editor::*, expr::*};

type C = String;
type D = Vec<String>;

pub struct Ce {}

impl Ce {}

impl EditorSpec for Ce {
    type Constructor = C;
    type Diagnostic = D;

    fn name() -> String {
        format!("ce")
    }

    fn initial_state() -> CoreEditorState<Self> {
        CoreEditorState::new(
            Expr::new(
                ExprLabel {
                    constructor: format!("label"),
                    diagnostic: Default::default(),
                },
                Default::default(),
            ),
            Default::default(),
        )
    }

    fn get_edits(state: &EditorState<Self>) -> Vec<EditMenuOption<Self>> {
        vec![EditMenuOption::new(
            EditMenuPattern::Static(format!("lambda")),
            insert_node,
        )]
    }

    fn get_diagnostics(state: EditorState<Self>) -> Vec<Self::Diagnostic> {
        todo!()
    }

    fn is_valid_handle(handle: &Handle, expr: &EditorExpr<Self>) -> bool {
        todo!()
    }

    fn render_label(ui: &mut egui::Ui, label: &ExprLabel<Self>) -> egui::Response {
        todo!()
    }
}

fn insert_node(query: &String, state: &CoreEditorState<Ce>) -> Option<CoreEditorState<Ce>> {
    let state = state.clone();

    let (new_expr, new_handle) = state.expr.clone().insert_fragment_at_handle(
        Fragment::Zipper(Zipper {
            outer_left: Span(vec![]),
            outer_right: Span(vec![]),
            inner: SpanContext {
                outer: ExprContext(vec![]),
                inner: SpanTooth {
                    label: ExprLabel::new(query.clone(), vec![]),
                    left: Span(vec![]),
                    right: Span(vec![]),
                },
            },
        }),
        state.handle,
    );
    Some(CoreEditorState {
        expr: new_expr,
        handle: new_handle,
        clipboard: state.clipboard,
    })
}
