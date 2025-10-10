use crate::{
    editor::{self, *},
    ex,
    expr::*,
};
use lazy_static::lazy_static;
use map_macro::hash_map;
use std::collections::HashMap;

pub type Str = &'static str;

pub struct Rule {
    pub sort: Str,
    pub kids: &'static [Str],
}

macro_rules! rule {
    ( $sort: expr, [ $( $kid: expr ),* ] ) => {
        Rule { sort: $sort, kids: &[ $( $kid ),* ] }
    };
}

lazy_static! {
    static ref GRAMMAR: HashMap<Str, Rule> = hash_map! {
        // Prop
        "<" => rule!["Prop", ["Num", "Num"]],
        ">" => rule!["Prop", ["Num", "Num"]],
        ">=" => rule!["Prop", ["Num", "Num"]],
        ">=" => rule!["Prop", ["Num", "Num"]],
        "=" => rule!["Prop", ["Num", "Num"]],
        "and" => rule!["Prop", ["Prop", "Prop"]],
        "or" => rule!["Prop", ["Prop", "Prop"]],
        "arrow" => rule!["Prop", ["Prop", "Prop"]],
        "not" => rule!["Prop", ["Prop"]],
        "forall" => rule!["Prop", ["Var", "Prop"]],
        "exists" => rule!["Prop", ["Var", "Prop"]],
        // Num
        "+" => rule!["Num", ["Num", "Num"]],
        "-" => rule!["Num", ["Num", "Num"]],
        "*" => rule!["Num", ["Num", "Num"]],
        "/" => rule!["Num", ["Num", "Num"]],
        // TODO: but how can we add number literals if the labels are strings?
        // maybe we'll add these too:
        "abs" => rule!["Num", ["Num"]],
        "ceil" => rule!["Num", ["Num"]],
        "floor" => rule!["Num", ["Num"]],
    };
}

macro_rules! simple_edit {
    ( $name: expr ) => {
        EditMenuOption::new(EditMenuPattern::Static(format!($name)), |_query, state| {
            let mut state = state;

            let info = GRAMMAR.get($name).unwrap();

            let mut kids: Vec<Expr<ExprLabel>> = vec![];
            for _ in 0..info.kids.len() {
                kids.push(ex![
                    ExprLabel::new(Constructor::Literal(format!("arg")), vec![]),
                    []
                ]);
            }

            let handle = state.expr.insert(
                state.handle,
                Fragment::Zipper(Zipper {
                    span_ol: Span::empty(),
                    span_or: Span::empty(),
                    middle: Context(vec![Tooth {
                        label: ExprLabel::new(Constructor::Literal(format!($name)), vec![]),
                        span_l: Span::empty(),
                        span_r: Span(kids),
                    }]),
                }),
            );

            state.handle = handle;

            Some(state)
        })
    };
}

pub struct Fol {}

impl Fol {}

impl EditorSpec for Fol {
    fn name() -> String {
        format!("ce")
    }

    fn initial_state() -> CoreEditorState {
        CoreEditorState::new(
            Expr::new(
                ExprLabel {
                    constructor: Constructor::Root,
                    diagnostic: Default::default(),
                },
                Span::empty(),
            ),
            Default::default(),
        )
    }

    fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption> {
        vec![
            simple_edit!["<"],
            simple_edit![">"],
            simple_edit![">="],
            simple_edit!["<="],
            simple_edit!["="],
            simple_edit!["and"],
            simple_edit!["or"],
            simple_edit!["arrow"],
            simple_edit!["not"],
            simple_edit!["forall"],
            simple_edit!["exists"],
            simple_edit!["+"],
            simple_edit!["-"],
            simple_edit!["*"],
            simple_edit!["/"],
            simple_edit!["abs"],
            simple_edit!["ceil"],
            simple_edit!["floor"],
        ]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Diagnostic> {
        Default::default()
    }

    fn is_valid_handle(_h: &Handle, _e: &EditorExpr) -> bool {
        true
    }

    // TODO, this is currently just copied from ce
    fn assemble_rendered_expr(
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        state: &mut EditorState<Self>,
        path: &Path,
        _expr: &EditorExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        label: &String,
    ) {
        let color_scheme = editor::EditorState::<Fol>::color_scheme(ui);

        let selected = state.core.handle.contains_path(path);
        let fill_color = if selected {
            color_scheme.highlight_background
        } else {
            color_scheme.normal_background
        };
        let text_color = color_scheme.normal_text;

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(egui::Label::new(egui::RichText::new("(").color(text_color)).selectable(false));
        });

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(
                egui::Label::new(egui::RichText::new(format!("{}", label)).color(text_color))
                    .selectable(false),
            );
        });

        for (step, kid) in render_steps_and_kids.iter() {
            step.render(ctx, ui, state);
            if let Some(kid) = kid {
                kid.render(ctx, ui, state);
            }
        }

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(egui::Label::new(egui::RichText::new(")").color(text_color)).selectable(false));
        });
    }
}
