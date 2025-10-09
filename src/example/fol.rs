use crate::{
    editor::{self, *},
    expr::*,
};
use lazy_static::lazy_static;
use map_macro::hash_map;
use std::collections::HashMap;

// (Name, Sort, kids' sorts)
const _PROP_GRAMMAR: &[(&str, &str, &[&str])] = &[
    // Prop
    ("<", "Prop", &["Num", "Num"]),
    (">", "Prop", &["Num", "Num"]),
    (">=", "Prop", &["Num", "Num"]),
    (">=", "Prop", &["Num", "Num"]),
    ("=", "Prop", &["Num", "Num"]),
    ("And", "Prop", &["Prop", "Prop"]),
    ("Or", "Prop", &["Prop", "Prop"]),
    ("Arrow", "Prop", &["Prop", "Prop"]),
    ("Not", "Prop", &["Prop"]),
    ("Forall", "Prop", &["Var", "Prop"]),
    ("Exists", "Prop", &["Var", "Prop"]),
    // Num
    ("+", "Num", &["Num", "Num"]),
    ("-", "Num", &["Num", "Num"]),
    ("*", "Num", &["Num", "Num"]),
    ("/", "Num", &["Num", "Num"]),
    // TODO: but how can we add number literals if the labels are strings?
    // maybe we'll add these too:
    // ("abs", "Num", &["Num"]),
    // ("ceil", "Num", &["Num"]),
    // ("floor", "Num", &["Num"]),
];

pub type Str = &'static str;

pub struct Rule {
    pub sort: Str,
    pub kids: &'static [Str],
}

macro_rules! rule {
    ( $sort: expr, $( $kid: expr ),* ) => {
        Rule { sort: $sort, kids: &[ $( $kid ),* ] }
    };
}

lazy_static! {
    static ref GRAMMAR: HashMap<Str, Rule> = hash_map! {
        // Prop
        "<" => Rule { sort: "Prop", kids: &["Num", "Num"] },
        ">" => Rule { sort: "Prop", kids: &["Num", "Num"] },
        ">=" => Rule { sort: "Prop", kids: &["Num", "Num"] },
        ">=" => Rule { sort: "Prop", kids: &["Num", "Num"] },
        "=" => Rule { sort: "Prop", kids: &["Num", "Num"] },
        "And" => Rule { sort: "Prop", kids: &["Prop", "Prop"] },
        "Or" => Rule { sort: "Prop", kids: &["Prop", "Prop"] },
        "Arrow" => Rule { sort: "Prop", kids: &["Prop", "Prop"] },
        "Not" => Rule { sort: "Prop", kids: &["Prop"] },
        "Forall" => Rule { sort: "Prop", kids: &["Var", "Prop"] },
        "Exists" => Rule { sort: "Prop", kids: &["Var", "Prop"] },
        // Num
        "+" => Rule { sort: "Num", kids: &["Num", "Num"] },
        "-" => Rule { sort: "Num", kids: &["Num", "Num"] },
        "*" => Rule { sort: "Num", kids: &["Num", "Num"] },
        "/" => Rule { sort: "Num", kids: &["Num", "Num"] },
        // TODO: but how can we add number literals if the labels are strings?
        // maybe we'll add these too:
        "abs" => Rule { sort: "Num", kids: &["Num"] },
        "ceil" => Rule { sort: "Num", kids: &["Num"] },
        "floor" => Rule { sort: "Num", kids: &["Num"] },
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
        vec![]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Diagnostic> {
        Default::default()
    }

    fn is_valid_handle(_h: &Handle, _e: &EditorExpr) -> bool {
        true
    }

    // TODO, this is currently just copied from ce
    fn assemble_rendered_expr(
        state: &mut EditorState<Self>,
        ui: &mut egui::Ui,
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
            step.render(state, ui);
            if let Some(kid) = kid {
                kid.render(state, ui);
            }
        }

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(egui::Label::new(egui::RichText::new(")").color(text_color)).selectable(false));
        });
    }
}
