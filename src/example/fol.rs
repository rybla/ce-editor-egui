#![allow(clippy::manual_let_else)]

use crate::{
    editor::{self, *},
    editor_ex, ex,
    expr::*,
    utility::pop_front,
};
use lazy_static::lazy_static;
use map_macro::hash_map;
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
// this is like Type but with less information
enum Sort {
    Proof,
    Prop,
    Num,
    Var,
}

use Sort::{Num, Proof, Prop, Var};

impl Sort {
    pub fn to_type(&self) -> Type<'_> {
        match self {
            Proof => panic!("can't convert a Proof to a Type"),
            Prop => Type::Prop,
            Num => Type::Num,
            Var => Type::Var,
        }
    }
}

#[expect(dead_code)] // until we actually construct Proof
#[derive(Debug, Clone)]
enum Type<'a> {
    Prop,
    Num,
    Var,
    Proof(&'a PlainExpr),
}

impl<'a> Type<'a> {
    pub fn get_sort(&self) -> Sort {
        match self {
            Type::Prop => Prop,
            Type::Num => Num,
            Type::Var => Var,
            Type::Proof(_) => Proof,
        }
    }
}

struct Rule {
    pub sort: Sort,
    pub kids: RuleKids,
    pub infix: bool,
}

enum RuleKids {
    FixedArity(&'static [Sort]),
    FreeArity(Sort),
    LiteralKid,
}

impl RuleKids {
    pub fn is_empty(&self) -> bool {
        match self {
            FixedArity(sorts) => sorts.is_empty(),
            _ => false,
        }
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            FixedArity(sorts) => Some(sorts.len()),
            _ => None,
        }
    }
}

use RuleKids::{FixedArity, FreeArity, LiteralKid};

macro_rules! rule {
    ( $sort: expr, [ $( $kid: expr ),* ] ) => {
        Rule {
            sort: $sort,
            kids: FixedArity(&[ $( $kid ),* ]),
            infix: false
        }
    };
    ( $sort: expr, [ $( $kid: expr ),* ], infix ) => {
        Rule {
            sort: $sort,
            kids: FixedArity(&[ $( $kid ),* ]),
            infix: true
        }
    };
    ( $sort: expr, LITERAL ) => {
        Rule {
            sort: $sort,
            kids: LiteralKid,
            infix: false
        }
    };
    ( $sort: expr, LIST $kid: expr ) => {
        Rule {
            sort: $sort,
            kids: FreeArity($kid),
            infix: false
        }
    };
    ( $sort: expr, LIST $kid: expr, infix ) => {
        Rule {
            sort: $sort,
            kids: FreeArity($kid),
            infix: true
        }
    };
}

/// This macro is a wrapper for the [`hash_map`] macro which lets you omit using
/// `to_owned` on each key when creating a [`HashMap`] with [`String`] keys.
macro_rules! string_hash_map {
    ( $( $key: literal => $val: expr ),* $(,)? ) => {
        hash_map!( $( $key.to_owned() => $val ),* )
    };
}

lazy_static! {
    static ref GRAMMAR: HashMap<String, Rule> = string_hash_map! {
        // Prop
        "var" => rule![Prop, LITERAL],
        "<" => rule![Prop, [Num, Num], infix],
        ">" => rule![Prop, [Num, Num], infix],
        ">=" => rule![Prop, [Num, Num], infix],
        "<=" => rule![Prop, [Num, Num], infix],
        "=" => rule![Prop, [Num, Num], infix],
        "and" => rule![Prop, [Prop, Prop], infix],
        "or" => rule![Prop, [Prop, Prop], infix],
        "=>" => rule![Prop, [Prop, Prop], infix],
        "not" => rule![Prop, [Prop]],
        "forall" => rule![Prop, [Var, Prop]],
        "exists" => rule![Prop, [Var, Prop]],
        // Num
        "+" => rule![Num, [Num, Num], infix],
        "-" => rule![Num, [Num, Num], infix],
        "*" => rule![Num, [Num, Num], infix],
        "/" => rule![Num, [Num, Num], infix],
        // TODO: but how can we add number literals if the labels are strings?
        // maybe we'll add these too:
        "abs" => rule![Num, [Num]],
        "ceil" => rule![Num, [Num]],
        "floor" => rule![Num, [Num]],
        // others
        "list" => rule![Num, LIST Num],
        // proof forms
        "intro_and" => rule![Proof, [Proof, Proof]],
        "intro_top" => rule![Proof, []],
        "elim_bot" => rule![Proof, [Proof]],
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
fn get_sorts(span: &Span<DiagEditorLabel>) -> Vec<Option<&Sort>> {
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

                let mut kids: Vec<DiagExpr> = vec![];
                match &rule.kids {
                    FixedArity(sorts) => {
                        for _ in 0..sorts.len() {
                            kids.push(ex![
                                GenEditorLabel::new(
                                    Constructor::PosArg,
                                    MutDiagnostics(Cell::new(vec![]))
                                ),
                                []
                            ]);
                        }
                    }
                    FreeArity(_sort) => {}
                    LiteralKid => {} // TODO: put some diagnostic if not 1 kid
                }

                let tooth_into_first_kid = pop_front(&mut kids).map(|e| e.into_tooth(Index(0)));

                let handle = state.expr.insert_fragment(
                    state.handle,
                    Fragment::Zipper(Zipper {
                        span_ol: Span::empty(),
                        span_or: Span::empty(),
                        middle: Context(match tooth_into_first_kid {
                            None => vec![Tooth {
                                label: GenEditorLabel::new(
                                    Constructor::Literal($name.to_owned()),
                                    MutDiagnostics(Cell::new(vec![])),
                                ),
                                span_l: Span::empty(),
                                span_r: Span(kids),
                            }],
                            Some(tooth_into_first_kid) => vec![
                                Tooth {
                                    label: GenEditorLabel::new(
                                        Constructor::Literal($name.to_owned()),
                                        MutDiagnostics(Cell::new(vec![])),
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

// -----------------------------------------------------------------------------
// begin the type-checker
// -----------------------------------------------------------------------------

/// inputs a [`Constructor::PosArg`] that is supposed to have one child with a
/// given expected type ignores newlines. if not one, it places an error
/// message. if so, it typechecks that.
fn check_pos_arg(
    success: &mut bool,
    ctx: HashMap<String, Type<'_>>,
    expected_type: &Type<'_>,
    pos_arg: &DiagExpr,
) {
    pos_arg.clear_diagnostics();
    // TODO: this needs to filter out newlines
    let without_newlines = pos_arg
        .pat_pos_arg()
        .iter()
        .filter(|x| !matches!(x.label.constructor, Constructor::Newline))
        .collect::<Vec<_>>();
    match without_newlines.as_slice() {
        [] => {
            pos_arg.add_diagnostic(Diagnostic("hole".to_owned()));
        }
        [kid] => {
            check_type_helper(success, ctx, expected_type, kid);
        }
        _kids => {
            pos_arg.add_diagnostic(Diagnostic("too many kids".to_owned()));
            *success = false;
        }
    }
}

fn check_free_args(
    success: &mut bool,
    ctx: &HashMap<String, Type<'_>>,
    expected_type: &Type<'_>,
    args: &[DiagExpr],
) {
    for arg in args {
        check_type_helper(success, ctx.clone(), expected_type, arg);
    }
}

fn check_type<'a>(
    ctx: HashMap<String, Type<'a>>,
    expected_type: &Type<'a>,
    expr: &DiagExpr,
) -> bool {
    let mut success = true;
    check_type_helper(&mut success, ctx, expected_type, expr);
    success
}

// clears old diagnostics on the expr, typechecks it, and places new diagnostics
// corresponding to the new type errors.
fn check_type_helper<'a>(
    success: &mut bool,
    ctx: HashMap<String, Type<'a>>,
    expected_type: &Type<'a>,
    expr: &DiagExpr,
) {
    let expected_sort = expected_type.get_sort();
    // clean up the old annotation
    expr.clear_diagnostics();

    let lit = match &expr.label.constructor {
        Constructor::Literal(lit) => lit,
        c => {
            panic!(
                "check_type_helper should only every be called on Literal constructors, instead of {c}"
            )
        }
    };

    // call this function to add error annotation, also sets the success flag
    let mut add_error = |d: Diagnostic| {
        expr.add_diagnostic(d);
        *success = false;
    };

    // get the corresponding rule
    let Rule {
        sort: rule_sort,
        kids: rule_kids,
        ..
    } = match GRAMMAR.get(lit) {
        None => {
            add_error(Diagnostic("foreign".to_owned()));
            return;
        }
        Some(rule) => rule,
    };

    // check sort
    if rule_sort != &expected_sort {
        add_error(Diagnostic(format!(
            "expected {expected_sort:?}; actually {rule_sort:?}"
        )));
    }

    // check arity
    match rule_kids.len() {
        Some(rule_kids_len) if rule_kids_len != expr.kids.0.len() => {
            let expr_kids_len = expr.kids.0.len();
            add_error(Diagnostic(format!(
                "expected {} kids; actually {} kids",
                rule_kids
                    .len()
                    .map_or_else(|| "infinity".to_owned(), |n| n.to_string()),
                expr_kids_len
            )));
        }
        _ => {}
    }

    // check kid sorts
    match (expr.pat_literal(), expected_type) {
        (("var", [x]), _) => {
            let x = match x.pat_literal() {
                (x, []) => x,
                _ => panic!("first child of Var should be Literal"),
            };
            if !ctx.contains_key(x) {
                add_error(Diagnostic("mal-scoped var".to_owned()));
            }
        }
        (("forall" | "exists", [x0, p]), _) => {
            let ctx = {
                let mut ctx = ctx;
                match x0.pat_pos_arg() {
                    [x] => match x.pat_literal() {
                        ("var", [x]) => match x.pat_literal() {
                            (x, []) => {
                                ctx.insert(x.to_owned(), Type::Num);
                                ctx
                            }
                            _ => panic!("first child of Var should be Literal"),
                        },
                        _ => ctx,
                    },
                    _ => ctx,
                }
            };
            check_pos_arg(success, ctx, &Type::Prop, p);
        }
        // The proof step cases are handled here
        ((label, _), Type::Proof(prop)) => {
            match ((label, expr.kids.0.as_slice()), prop.pat_literal()) {
                (("intro_and", [a, b]), ("and", [p, q])) => {
                    check_pos_arg(success, ctx.clone(), &Type::Proof(p), a);
                    check_pos_arg(success, ctx.clone(), &Type::Proof(q), b);
                }
                (("intro_top", []), ("top", [])) => {}
                (("elim_bot", [a]), _) => {
                    check_pos_arg(success, ctx, &Type::Proof(&editor_ex!("bot", [])), a);
                }
                _ => {
                    add_error(Diagnostic("invalid proof".to_owned()));
                }
            }
        }
        // default case that doesn't interact with environment, and uses simple sorts
        _ => match rule_kids {
            FixedArity(rule_kids) => {
                for (kid, expected_kid_sort) in expr.kids.0.iter().zip(rule_kids.iter()) {
                    check_pos_arg(success, ctx.clone(), &expected_kid_sort.to_type(), kid);
                }
            }
            FreeArity(expected_kid_sort) => {
                check_free_args(success, &ctx, &expected_kid_sort.to_type(), &expr.kids.0);
            }
            LiteralKid => {}
        },
    }
}

// -----------------------------------------------------------------------------
// end type-checker
// -----------------------------------------------------------------------------

pub struct Fol {}

impl Fol {}

impl EditorSpec for Fol {
    fn name() -> String {
        "fol".to_owned()
    }

    fn initial_state() -> PlainCoreState {
        CoreState::new(
            Expr::new(
                GenEditorLabel {
                    constructor: Constructor::Root,
                    diagnostics: (),
                },
                Span::empty(),
            ),
            Default::default(),
        )
    }

    fn diagnose(state: &CoreState<MutDiagnostics>) {
        for kid in &state.expr.kids.0 {
            let _success = check_type(hash_map! {}, &Type::Prop, kid);
        }
    }

    fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption> {
        vec![
            EditMenuOption {
                pattern: EditMenuPattern::Dynamic("variable".to_owned(), |query| {
                    if query.is_empty() || GRAMMAR.keys().any(|x| x == query) {
                        return None;
                    }
                    Some(query.to_owned())
                }),
                edit: |query, mut state| {
                    state.handle = state.expr.insert_fragment(
                        state.handle,
                        Fragment::Span(Span(vec![GenEditorExpr::new_lit(
                            "var".to_owned(),
                            vec![GenEditorExpr::new_lit(query.to_owned(), vec![])],
                        )])),
                    );
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
            make_simple_edit_menu_option!["intro_and"],
            make_simple_edit_menu_option!["intro_top"],
            make_simple_edit_menu_option!["elim_bot"],
        ]
    }

    fn is_valid_handle_specialized(h: &Handle, root: &DiagExpr) -> bool {
        match h {
            Handle::Point(p) => {
                let e = root.get_subexpr(&p.path);
                match &e.label.constructor {
                    Constructor::Literal(_) => {
                        let sort = get_rule(&e.label.constructor)
                            .unwrap_or_else(|| panic!("no rule for {}", e.label.constructor));
                        matches!(&sort.kids, FreeArity(_))
                    }
                    Constructor::Newline => false,
                    Constructor::Root | Constructor::PosArg => true,
                }
            }
            Handle::Span(h) => {
                Self::is_valid_handle(&Handle::Point(h.p_l()), root)
                    && Self::is_valid_handle(&Handle::Point(h.p_r()), root)
            }
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
                // // As a simple first approach, just see if the parent expression
                // // at each end of the zipper has the same sort.
                // let e_o = root.at_path(&h.path_o);
                // let sort_o = match e_o.pat2() {
                //     (Constructor::PosArg, [k]) => {
                //         get_rule(&k.label.constructor).map(|r| r.sort.clone())
                //     }
                //     _ => None,
                // };
                // let e_i = root.at_path(&h.path_i());
                // let sort_i = match e_i.pat2() {
                //     (Constructor::PosArg, [k]) => {
                //         get_rule(&k.label.constructor).map(|r| r.sort.clone())
                //     }
                //     _ => None,
                // };
                // sort_o == sort_i

                Self::is_valid_handle(&Handle::Span(h.handle_o()), root)
                    && Self::is_valid_handle(&Handle::Span(h.handle_i()), root)
            }
        }
    }

    fn assemble_rendered_expr(
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        rc: RenderContext<'_>,
        path: &Path,
        expr: &DiagExpr,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_>>)>,
        label: &str,
    ) {
        let color_scheme = editor::EditorState::<Self>::color_scheme(ui);
        let rule = get_rule(&expr.label.constructor)
            .unwrap_or_else(|| panic!("no rule for {}", expr.label.constructor));

        let selected = rc.handle.contains_path(path);
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

        let RenderContext {
            handle,
            root,
            color_scheme: color_scheme_1,
            drag_origin,
            menu,
            actions,
            interactive,
            indent_level,
        } = rc;
        for (i, (step, kid)) in render_steps_and_kids.iter().enumerate() {
            step.render::<Self>(
                ctx,
                ui,
                // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                RenderContext {
                    handle,
                    root,
                    color_scheme: color_scheme_1,
                    drag_origin,
                    menu,
                    actions,
                    interactive,
                    indent_level,
                },
            );
            if let Some(kid) = kid {
                kid.render::<Self>(
                    ctx,
                    ui,
                    // NOTE: It's really annoying, but apparently you have to do this in order to avoid ownership problems.
                    RenderContext {
                        handle,
                        root,
                        color_scheme: color_scheme_1,
                        drag_origin,
                        menu,
                        actions,
                        interactive,
                        indent_level,
                    },
                );
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
