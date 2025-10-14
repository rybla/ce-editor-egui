use crate::{
    editor::{self, *},
    ex,
    expr::*,
};
use lazy_static::lazy_static;
use map_macro::hash_map;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
enum Sort {
    Prop,
    Num,
    Var,
}

use Sort::{Num, Prop, Var};

struct Rule {
    pub sort: Sort,
    pub kids: &'static [Sort],
}

macro_rules! rule {
    ( $sort: expr, [ $( $kid: expr ),* ] ) => {
        Rule { sort: $sort, kids: &[ $( $kid ),* ] }
    };
}

lazy_static! {
    static ref GRAMMAR: HashMap<String, Rule> = hash_map! {
        // Prop
        "<".to_owned() => rule![Prop, [Num, Num]],
        ">".to_owned() => rule![Prop, [Num, Num]],
        ">=".to_owned() => rule![Prop, [Num, Num]],
        ">=".to_owned() => rule![Prop, [Num, Num]],
        "=".to_owned() => rule![Prop, [Num, Num]],
            "and".to_owned() => rule![Prop, [Prop, Prop]],
        "or".to_owned() => rule![Prop, [Prop, Prop]],
        "arrow".to_owned() => rule![Prop, [Prop, Prop]],
        "not".to_owned() => rule![Prop, [Prop]],
        "forall".to_owned() => rule![Prop, [Var, Prop]],
        "exists".to_owned() => rule![Prop, [Var, Prop]],
        // Num
        "+".to_owned() => rule![Num, [Num, Num]],
        "-".to_owned() => rule![Num, [Num, Num]],
        "*".to_owned() => rule![Num, [Num, Num]],
        "/".to_owned() => rule![Num, [Num, Num]],
        // TODO: but how can we add number literals if the labels are strings?
        // maybe we'll add these too:
        "abs".to_owned() => rule![Num, [Num]],
        "ceil".to_owned() => rule![Num, [Num]],
        "floor".to_owned() => rule![Num, [Num]],
    };
}

fn get_rule(c: &Constructor) -> Option<&Rule> {
    match c {
        Constructor::Literal(lit) => Some(
            GRAMMAR
                .get(lit)
                .expect("symbol literal to be defined in grammar"),
        ),
        _ => None,
    }
}

#[expect(dead_code)]
fn get_sorts(span: &Span<ExprLabel>) -> Vec<Option<&Sort>> {
    span.0
        .iter()
        .map(|e| get_rule(&e.label.constructor).map(|r| &r.sort))
        .collect()
}

// This has to be a macro rather than a definition because as a function it
// would return a closure that captures the name dynamically.
macro_rules! make_simple_edit_menu_option {
    ( $name: expr ) => {
        EditMenuOption::new(
            EditMenuPattern::Static($name.to_owned()),
            |_query, state| {
                let mut state = state;

                let rule = GRAMMAR.get($name).unwrap();

                let mut kids: Vec<Expr<ExprLabel>> = vec![];
                for _ in 0..rule.kids.len() {
                    kids.push(ex![ExprLabel::new(Constructor::PosArg, vec![]), []]);
                }

                let handle = state.expr.insert(
                    state.handle,
                    Fragment::Zipper(Zipper {
                        span_ol: Span::empty(),
                        span_or: Span::empty(),
                        middle: Context(vec![Tooth {
                            label: ExprLabel::new(Constructor::Literal($name.to_owned()), vec![]),
                            span_l: Span::empty(),
                            span_r: Span(kids),
                        }]),
                    }),
                );

                state.handle = handle;

                Some(state)
            },
        )
    };
}

pub struct Fol {}

impl Fol {}

impl EditorSpec for Fol {
    fn name() -> String {
        "fol".to_owned()
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
            make_simple_edit_menu_option!["<"],
            make_simple_edit_menu_option![">"],
            make_simple_edit_menu_option![">="],
            make_simple_edit_menu_option!["<="],
            make_simple_edit_menu_option!["="],
            make_simple_edit_menu_option!["and"],
            make_simple_edit_menu_option!["or"],
            make_simple_edit_menu_option!["arrow"],
            make_simple_edit_menu_option!["not"],
            make_simple_edit_menu_option!["forall"],
            make_simple_edit_menu_option!["exists"],
            make_simple_edit_menu_option!["+"],
            make_simple_edit_menu_option!["-"],
            make_simple_edit_menu_option!["*"],
            make_simple_edit_menu_option!["/"],
            make_simple_edit_menu_option!["abs"],
            make_simple_edit_menu_option!["ceil"],
            make_simple_edit_menu_option!["floor"],
        ]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Diagnostic> {
        Default::default()
    }

    fn is_valid_handle_specialized(h: &Handle, root: &EditorExpr) -> bool {
        match h {
            Handle::Point(_p) => true,
            Handle::Span(_h) => true,
            // Handle::Zipper(h) => {
            //     // Try to extract a single sort at the outer [`SpanHandle`].
            //     let span_o = root.at_span_handle(&h.handle_o());
            //     let mut sorts_o: Vec<Sort> = get_sorts(&span_o)
            //         .into_iter()
            //         .filter_map(|o| o.map(|x| x.to_owned()))
            //         .collect();
            //     sorts_o.sort();
            //     sorts_o.dedup();
            //     let sort_o = match sorts_o.pop() {
            //         Some(_) if !sorts_o.is_empty() => None,
            //         Some(sort) => Some(sort),
            //         None => None,
            //     };
            //
            //     // Try to extract a single sort at the inner [`SpanHandle`].
            //     let span_i = root.at_span_handle(&h.handle_i());
            //     let mut sorts_i: Vec<Sort> = get_sorts(&span_i)
            //         .into_iter()
            //         .filter_map(|o| o.map(|x| x.to_owned()))
            //         .collect();
            //     sorts_i.sort();
            //     sorts_i.dedup();
            //     let sort_i = match sorts_i.pop() {
            //         Some(_) if !sorts_i.is_empty() => None,
            //         Some(sort) => Some(sort),
            //         None => None,
            //     };
            //
            //     // This isn't complete because it doesn't handle the differences
            //     // between forms that do not have an assigned sort.
            //     sort_o == sort_i
            // }
            Handle::Zipper(h) => {
                // As a simple first approach, just see if the parent expression
                // at each end of the zipper has the same sort.
                let e_o = root.at_path(&h.path_o);
                let sort_o = get_rule(&e_o.label.constructor).map(|r| r.sort.clone());
                let e_i = root.at_path(&h.path_i());
                let sort_i = get_rule(&e_i.label.constructor).map(|r| r.sort.clone());
                sort_o == sort_i
            }
        }
    }

    // TODO, this is currently just copied from ce
    fn assemble_rendered_expr(
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        ren_ctx: &RenderContext,
        state: &mut EditorState<Self>,
        path: &Path,
        _expr: &EditorExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        label: &str,
    ) {
        let color_scheme = editor::EditorState::<Self>::color_scheme(ui);

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
                egui::Label::new(egui::RichText::new(label.to_owned()).color(text_color))
                    .selectable(false),
            );
        });

        for (step, kid) in &render_steps_and_kids {
            step.render(ctx, ui, ren_ctx, state);
            if let Some(kid) = kid {
                kid.render(ctx, ui, ren_ctx, state);
            }
        }

        egui::Frame::new().fill(fill_color).show(ui, |ui| {
            ui.add(egui::Label::new(egui::RichText::new(")").color(text_color)).selectable(false));
        });
    }
}
