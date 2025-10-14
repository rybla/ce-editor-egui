use crate::{
    editor::{self, *},
    ex,
    expr::*,
    utility::pop_front,
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
    pub kids: RuleKids,
    pub infix: bool,
}

enum RuleKids {
    FixedArity(&'static [Sort]),
    FreeArity(Sort),
}
impl RuleKids {
    fn is_empty(&self) -> bool {
        match self {
            FixedArity(sorts) => sorts.is_empty(),
            FreeArity(_) => false,
        }
    }
}

use RuleKids::{FixedArity, FreeArity};

macro_rules! rule {
    ( $sort: expr, fixed-arity [ $( $kid: expr ),* ] ) => {
        Rule {
            sort: $sort,
            kids: FixedArity(&[ $( $kid ),* ]),
            infix: false
        }
    };
    ( $sort: expr, fixed-arity [ $( $kid: expr ),* ], infix ) => {
        Rule {
            sort: $sort,
            kids: FixedArity(&[ $( $kid ),* ]),
            infix: true
        }
    };
    ( $sort: expr, free-arity [ $kid: expr ] ) => {
        Rule {
            sort: $sort,
            kids: FreeArity($kid),
            infix: false
        }
    };
    ( $sort: expr, free-arity [ $kid: expr ], infix ) => {
        Rule {
            sort: $sort,
            kids: FreeArity($kid),
            infix: true
        }
    };
}

lazy_static! {
    static ref GRAMMAR: HashMap<String, Rule> = hash_map! {
        // Prop
        "<".to_owned() => rule![Prop, fixed-arity[Num, Num], infix],
        ">".to_owned() => rule![Prop, fixed-arity[Num, Num], infix],
        ">=".to_owned() => rule![Prop, fixed-arity[Num, Num], infix],
        "<=".to_owned() => rule![Prop, fixed-arity[Num, Num], infix],
        "=".to_owned() => rule![Prop, fixed-arity[Num, Num], infix],
            "and".to_owned() => rule![Prop, fixed-arity[Prop, Prop], infix],
        "or".to_owned() => rule![Prop, fixed-arity[Prop, Prop], infix],
        "=>".to_owned() => rule![Prop, fixed-arity[Prop, Prop], infix],
        "not".to_owned() => rule![Prop, fixed-arity[Prop]],
        "forall".to_owned() => rule![Prop, fixed-arity[Var, Prop]],
        "exists".to_owned() => rule![Prop, fixed-arity[Var, Prop]],
        // Num
        "+".to_owned() => rule![Num, fixed-arity[Num, Num], infix],
        "-".to_owned() => rule![Num, fixed-arity[Num, Num], infix],
        "*".to_owned() => rule![Num, fixed-arity[Num, Num], infix],
        "/".to_owned() => rule![Num, fixed-arity[Num, Num], infix],
        // TODO: but how can we add number literals if the labels are strings?
        // maybe we'll add these too:
        "abs".to_owned() => rule![Num, fixed-arity[Num]],
        "ceil".to_owned() => rule![Num, fixed-arity[Num]],
        "floor".to_owned() => rule![Num, fixed-arity[Num]],
        // others
        "list".to_owned() => rule![Num, free-arity[Num]],
    };

    static ref DEFAULT_LITERAL_RULE: Rule = Rule {
        sort: Var,
        kids: FixedArity(&[]),
        infix: false
    };
}

fn get_rule(c: &Constructor) -> Option<&Rule> {
    match c {
        Constructor::Literal(lit) => Some(GRAMMAR.get(lit).unwrap_or(&DEFAULT_LITERAL_RULE)),
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

                let rule = GRAMMAR
                    .get($name)
                    .unwrap_or_else(|| panic!("no rule for {}", $name));

                let mut kids: Vec<Expr<ExprLabel>> = vec![];
                match &rule.kids {
                    FixedArity(sorts) => {
                        for _ in 0..sorts.len() {
                            kids.push(ex![ExprLabel::new(Constructor::PosArg, vec![]), []]);
                        }
                    }
                    FreeArity(_sort) => {}
                }

                let tooth_into_first_kid = pop_front(&mut kids).map(|e| e.tooth_at_index(&Index(0)));

                let handle = state.expr.insert(
                    state.handle,
                    Fragment::Zipper(Zipper {
                        span_ol: Span::empty(),
                        span_or: Span::empty(),
                        middle: Context(match tooth_into_first_kid {
                            None => vec![Tooth {
                                label: ExprLabel::new(
                                    Constructor::Literal($name.to_owned()),
                                    vec![],
                                ),
                                span_l: Span::empty(),
                                span_r: Span(kids),
                            }],
                            Some(tooth_into_first_kid) => vec![
                                Tooth {
                                    label: ExprLabel::new(
                                        Constructor::Literal($name.to_owned()),
                                        vec![],
                                    ),
                                    span_l: Span::empty(),
                                    span_r: Span(kids),
                                },
                                tooth_into_first_kid,
                            ],
                        }),
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
            EditMenuOption {
                pattern: EditMenuPattern::Dynamic("variable".to_owned(), |query| {
                    if GRAMMAR.keys().any(|x| x == query) {
                        return None;
                    }
                    Some(query.clone())
                }),
                edit: |query, state| {
                    let mut state = state;
                    let handle = state.expr.insert(
                        state.handle,
                        Fragment::Span(Span(vec![Expr {
                            label: ExprLabel::new(Constructor::Literal(query.to_owned()), vec![]),
                            kids: Span::empty(),
                        }])),
                    );

                    state.handle = handle;

                    Some(state)
                },
            },
            make_simple_edit_menu_option!["<"],
            make_simple_edit_menu_option![">"],
            make_simple_edit_menu_option![">="],
            make_simple_edit_menu_option!["<="],
            make_simple_edit_menu_option!["="],
            make_simple_edit_menu_option!["and"],
            make_simple_edit_menu_option!["or"],
            make_simple_edit_menu_option!["=>"],
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
            make_simple_edit_menu_option!["list"],
        ]
    }

    fn get_diagnostics(_state: EditorState<Self>) -> Vec<Diagnostic> {
        Default::default()
    }

    fn is_valid_handle_specialized(h: &Handle, root: &EditorExpr) -> bool {
        match h {
            Handle::Point(p) => {
                let e = root.at_path(&p.path);
                match &e.label.constructor {
                    Constructor::Literal(_) => {
                        let sort = get_rule(&e.label.constructor)
                            .unwrap_or_else(|| panic!("no rule for {} ", e.label.constructor));
                        matches!(&sort.kids, FreeArity(_))
                    }
                    Constructor::Newline => false,
                    Constructor::Root | Constructor::PosArg => true,
                }
            }
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
                let sort_o = match e_o.pat2() {
                    (Constructor::PosArg, [k]) => get_rule(&k.label.constructor).map(|r| r.sort.clone()),
                    _ => None
                };
                let e_i = root.at_path(&h.path_i());
                let sort_i = match e_i.pat2() {
                    (Constructor::PosArg, [k]) => get_rule(&k.label.constructor).map(|r| r.sort.clone()),
                    _ => None
                };
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
        expr: &EditorExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        label: &str,
    ) {
        let color_scheme = editor::EditorState::<Self>::color_scheme(ui);
        let rule = get_rule(&expr.label.constructor)
            .unwrap_or_else(|| panic!("no rule for {}", expr.label.constructor));

        let selected = state.core.handle.contains_path(path);
        let fill_color = if selected {
            color_scheme.highlight_background
        } else {
            color_scheme.normal_background
        };

        if !rule.kids.is_empty() {
            egui::Frame::new().fill(fill_color).show(ui, |ui| {
                ui.add(
                    egui::Label::new(
                        egui::RichText::new("(")
                            .color(color_scheme.normal_text)
                            .text_style(egui::TextStyle::Monospace),
                    )
                    .selectable(false),
                );
            });
        }

        let render_label = |ui: &mut egui::Ui| {
            egui::Frame::new().fill(fill_color).show(ui, |ui| {
                ui.add(
                    egui::Label::new(
                        egui::RichText::new(label.to_owned())
                            .color(if rule.kids.is_empty() {
                                color_scheme.normal_text
                            } else {
                                color_scheme.keyword_text
                            })
                            .text_style(if rule.kids.is_empty() {
                                egui::TextStyle::Body
                            } else {
                                egui::TextStyle::Monospace
                            })
                            .strong(),
                    )
                    .selectable(false),
                );
            });
        };

        if !rule.infix {
            render_label(ui);
        }

        for (i, (step, kid)) in render_steps_and_kids.iter().enumerate() {
            step.render(ctx, ui, ren_ctx, state);
            if let Some(kid) = kid {
                kid.render(ctx, ui, ren_ctx, state);
            }
            if rule.infix && i == 0 {
                render_label(ui);
            }
        }

        if !rule.kids.is_empty() {
            egui::Frame::new().fill(fill_color).show(ui, |ui| {
                ui.add(
                    egui::Label::new(
                        egui::RichText::new(")")
                            .color(color_scheme.normal_text)
                            .text_style(egui::TextStyle::Monospace),
                    )
                    .selectable(false),
                );
            });
        }
    }
}
