#![allow(clippy::manual_let_else, clippy::enum_glob_use)]

use crate::{
    editor::{self, *},
    editor_ex, ex,
    expr::*,
    utility::pop_front,
};
use egui::RichText;
use lazy_static::lazy_static;
use map_macro::hash_map;
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
// this is just the grammatical sort, like Type but with less information
pub enum Sort {
    TopLevel,
    Declaration,
    Proof,
    Prop,
    Num,
    Var,
    Binding,
}

use Sort::*;

impl Sort {
    pub fn to_type(&self) -> Type {
        match self {
            Prop => Type::Prop,
            Num => Type::Num,
            Var => Type::Var,
            Declaration => Type::Declaration,
            _ => panic!("can't convert something to a Type"),
        }
    }
}

// grammatical sort and the type
#[derive(Debug, Clone)]
pub enum Type {
    Binding,
    Prop,
    Num,
    Var,
    Proof(PlainExpr),
    Declaration,
}

impl Type {
    pub fn get_sort(&self) -> Sort {
        match self {
            Self::Prop => Prop,
            Self::Num => Num,
            Self::Var => Var,
            Self::Proof(_) => Proof,
            Self::Declaration => Declaration,
            Self::Binding => Binding,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prop => write!(f, "Prop"),
            Self::Num => write!(f, "Num"),
            Self::Var => write!(f, "Var"),
            Self::Proof(p) => write!(f, "Proof({p})"),
            Self::Declaration => write!(f, "Declaration"),
            Self::Binding => write!(f, "Binding"),
        }
    }
}

struct Rule {
    pub sort: Sort,
    pub kids: RuleKids,
}

enum RuleKids {
    FixedArity(&'static [Sort], bool),
    FreeArity(Sort),
    LiteralKid,
}

impl RuleKids {
    pub fn is_parenthesized(&self) -> bool {
        match self {
            FixedArity(sorts, _) => !sorts.is_empty(),
            FreeArity(_) => true,
            LiteralKid => false,
        }
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            FixedArity(sorts, _) => Some(sorts.len()),
            _ => None,
        }
    }

    pub fn is_infixed(&self) -> bool {
        match self {
            FixedArity(_, infixed) => *infixed,
            _ => false,
        }
    }

    fn is_prefixed(&self) -> bool {
        match self {
            FixedArity(_, infixed) => !*infixed,
            FreeArity(_) => true,
            LiteralKid => false,
        }
    }
}

use RuleKids::*;

macro_rules! rule {
    ( $sort: expr, [ $( $kid: expr ),* ] ) => {
        Rule {
            sort: $sort,
            kids: FixedArity(&[ $( $kid ),* ], false),
        }
    };
    ( $sort: expr, [ $( $kid: expr ),* ], infix ) => {
        Rule {
            sort: $sort,
            kids: FixedArity(&[ $( $kid ),* ], true),
        }
    };
    ( $sort: expr, LITERAL ) => {
        Rule {
            sort: $sort,
            kids: LiteralKid,
        }
    };
    ( $sort: expr, LIST $kid: expr ) => {
        Rule {
            sort: $sort,
            kids: FreeArity($kid),
        }
    };
    ( $sort: expr, LIST $kid: expr, infix ) => {
        Rule {
            sort: $sort,
            kids: FreeArity($kid),
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
        // root
        "root" => rule![TopLevel, LIST Declaration],
        // Declaration
        "lemma" => rule![Declaration, [Binding, Prop, Proof]],
        // Prop
        "var" => rule![Num, LITERAL],
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
        "top" => rule![Prop, []],
        "bot" => rule![Prop, []],
        // Binding
        "binding" => rule![Binding, LITERAL],
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
        kids: FixedArity(&[], false),
    };
}

fn get_rule(c: &Constructor) -> Option<&Rule> {
    match c {
        Constructor::Literal(lit) => Some(GRAMMAR.get(lit).unwrap_or(&DEFAULT_LITERAL_RULE)),
        _ => None,
    }
}

#[expect(dead_code)]
fn get_sorts(span: &Span<MetaLabel<<Fol as EditorSpec>::M>>) -> Vec<Option<&Sort>> {
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

                let mut kids: Vec<MetaExpr<<Fol as EditorSpec>::M>> = vec![];
                match &rule.kids {
                    FixedArity(sorts, _) => {
                        for _ in 0..sorts.len() {
                            kids.push(ex![
                                Label::new(Constructor::PosArg, MutMetadata::default()),
                                []
                            ]);
                        }
                    }
                    FreeArity(_sort) => {}
                    LiteralKid => {} // TODO: add error if not 1 kid
                }

                let tooth_into_first_kid = pop_front(&mut kids).map(|e| e.into_tooth(Index(0)));

                let handle = state.root.insert_fragment(
                    state.handle,
                    Fragment::Zipper(Zipper {
                        span_ol: Span::empty(),
                        span_or: Span::empty(),
                        middle: Context(match tooth_into_first_kid {
                            None => vec![Tooth {
                                label: Label::new(
                                    Constructor::Literal($name.to_owned()),
                                    MutMetadata::default(),
                                ),
                                span_l: Span::empty(),
                                span_r: Span(kids),
                            }],
                            Some(tooth_into_first_kid) => vec![
                                Tooth {
                                    label: Label::new(
                                        Constructor::Literal($name.to_owned()),
                                        MutMetadata::default(),
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
    ctx: HashMap<String, Type>,
    expected_type: &Type,
    pos_arg: &MetaExpr<<Fol as EditorSpec>::M>,
) {
    // clear old errors
    pos_arg.modify_metadata(|mut m| {
        m.errors.clear();
        m
    });

    let without_newlines = pos_arg
        .to_pos_arg()
        .iter()
        .filter(|x| !matches!(x.label.constructor, Constructor::Newline))
        .collect::<Vec<_>>();

    match without_newlines.as_slice() {
        [] => {
            pos_arg.modify_metadata(|mut m| {
                m.errors.push(format!("hole of sort {expected_type}"));
                m
            });
        }
        [kid] => {
            check_type_helper(success, ctx, expected_type, kid);
        }
        _kids => {
            pos_arg.modify_metadata(|mut m| {
                m.errors.push("too many kids".to_owned());
                m
            });
            *success = false;
        }
    }
}

fn check_free_args(
    success: &mut bool,
    ctx: &HashMap<String, Type>,
    expected_type: &Type,
    args: &[MetaExpr<<Fol as EditorSpec>::M>],
) {
    for arg in args {
        check_type_helper(success, ctx.clone(), expected_type, arg);
    }
}

fn check_type(
    ctx: HashMap<String, Type>,
    expected_type: &Type,
    expr: &MetaExpr<<Fol as EditorSpec>::M>,
) -> bool {
    let mut success = true;
    check_type_helper(&mut success, ctx, expected_type, expr);
    success
}

// clears old metadata on the expr, typechecks it, and places new metadata
// corresponding to the new type errors.
fn check_type_helper(
    success: &mut bool,
    ctx: HashMap<String, Type>,
    expected_type: &Type,
    expr: &MetaExpr<<Fol as EditorSpec>::M>,
) {
    // clear old errors
    expr.modify_metadata(|mut m| {
        m.errors.clear();
        m
    });

    let expected_sort = expected_type.get_sort();

    if expr.label.constructor == Constructor::Root {
        for decl in &expr.kids.0 {
            check_type_helper(success, ctx.clone(), &Type::Declaration, decl);
        }
    } else {
        let lit = match &expr.label.constructor {
            Constructor::Literal(lit) => lit,
            c => {
                panic!(
                    "check_type_helper should only every be called on Literal constructors, instead of {c}"
                )
            }
        };

        // call this function to add error annotation, also sets the success flag
        let mut add_error = |e: String| {
            expr.modify_metadata(|mut m| {
                m.errors.push(e);
                m
            });
            *success = false;
        };

        // get the corresponding rule
        let Rule {
            sort: rule_sort,
            kids: rule_kids,
        } = match GRAMMAR.get(lit) {
            None => {
                add_error("foreign".to_owned());
                return;
            }
            Some(rule) => rule,
        };

        // check sort
        if rule_sort != &expected_sort {
            add_error(format!(
                "expected {expected_sort:?}; actually {rule_sort:?}"
            ));
        }

        // check arity
        match rule_kids.len() {
            Some(rule_kids_len) if rule_kids_len != expr.kids.0.len() => {
                let expr_kids_len = expr.kids.0.len();
                add_error(format!(
                    "expected {} kids; actually {} kids",
                    rule_kids
                        .len()
                        .map_or_else(|| "infinity".to_owned(), |n| n.to_string()),
                    expr_kids_len
                ));
            }
            _ => {}
        }

        // check kid sorts

        match (expr.to_lit(), expected_type) {
            (("lemma", [x, sig, imp]), _) => {
                // TODO: this is redundant with the rules of Lemma in Grammar
                check_pos_arg(success, ctx.clone(), &Type::Binding, x);
                check_pos_arg(success, ctx.clone(), &Type::Prop, sig);
                if let Some(sig) = sig.to_plain().kids.0.first() {
                    check_pos_arg(success, ctx, &Type::Proof(sig.clone()), imp);
                } else {
                    // don't check imp until a sig is provided
                }
            }
            (("var", [x]), _) => {
                let x = match x.to_lit() {
                    (x, []) => x,
                    _ => panic!("first child of Var should be Literal"),
                };
                if !ctx.contains_key(x) {
                    add_error("mal-scoped var".to_owned());
                }
            }
            (("forall" | "exists", [x0, a]), _) => {
                let ctx = {
                    let mut ctx = ctx;
                    match x0.to_pos_arg() {
                        [x] => {
                            check_pos_arg(success, ctx.clone(), &Type::Var, x);
                            match x.to_lit() {
                                ("var", [x]) => match x.to_lit() {
                                    (x, []) => {
                                        ctx.insert(x.to_owned(), Type::Num);
                                        ctx
                                    }
                                    _ => panic!("first child of Var should be Literal"),
                                },
                                _ => ctx,
                            }
                        }
                        _ => ctx,
                    }
                };
                check_pos_arg(success, ctx, &Type::Prop, a);
            }
            // The proof step cases are handled here
            ((label, _), Type::Proof(prop)) => {
                match ((label, expr.kids.0.as_slice()), prop.to_lit()) {
                    (("intro_and", [a, b]), ("and", [p, q])) => {
                        check_pos_arg(success, ctx.clone(), &Type::Proof(p.kids.0[0].clone()), a);
                        check_pos_arg(success, ctx.clone(), &Type::Proof(q.kids.0[0].clone()), b);
                    }
                    (("intro_top", []), ("top", [])) => {}
                    (("elim_bot", [a]), _) => {
                        check_pos_arg(success, ctx, &Type::Proof(editor_ex!("bot", [])), a);
                    }
                    _ => {
                        add_error("invalid proof".to_owned());
                    }
                }
            }
            // default case that doesn't interact with environment, and uses simple sorts
            _ => match rule_kids {
                FixedArity(rule_kids, _) => {
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
}

// -----------------------------------------------------------------------------
// end type-checker
// -----------------------------------------------------------------------------

pub struct Fol {}

impl Fol {}

#[derive(Debug, Default)]
pub struct M {
    pub errors: Vec<String>,
    pub ty_info: Option<TyInfo>,
}

impl M {
    pub fn push_error(&mut self, e: String) {
        self.errors.push(e);
    }
}

impl EditorMetadata for M {
    fn render_metadata(&self, ui: &mut egui::Ui) {
        egui::Frame::popup(ui.style())
            .shadow(egui::Shadow::NONE)
            .show(ui, |ui| {
                ui.set_max_width(200.0);
                ui.add(egui::Label::new(
                    egui::RichText::new("Diagnostics".to_owned())
                        .underline()
                        .size(10.0),
                ));

                // TODO: layout this more nicely
                for e in &self.errors {
                    ui.label(RichText::new(format!("Error: {e}")));
                    ui.end_row();
                }
            });
    }

    fn is_empty_metadata(&self) -> bool {
        self.errors.is_empty()
    }

    fn add_error(&mut self, e: String) {
        self.errors.push(e);
    }
}

#[derive(Debug)]
pub struct TyInfo {
    pub ctx: HashMap<String, Type>,
    pub ty: Type,
}

impl EditorSpec for Fol {
    type M = M;

    fn name() -> String {
        "fol".to_owned()
    }

    fn initial_state() -> PlainCoreState {
        CoreState::new(
            Expr::new(
                Label {
                    constructor: Constructor::Root,
                    metadata: (),
                },
                Span::empty(),
            ),
            Default::default(),
        )
    }

    fn annotate(state: &CoreState<MutMetadata<Self::M>>) {
        // for kid in &state.root.kids.0 {
        //     let _success = check_type(hash_map! {}, &Type::Prop, kid);
        // }

        // for kid in &state.root.kids.0 {
        //     let _success = check_type(hash_map! {}, &Type::Declaration, kid);
        // }

        // TODO: the &Type::Declaration is ignored here...
        check_type(hash_map! {}, &Type::Declaration, &state.root);

        {
            let mut ds = state.metadata.0.take();
            ds.push_error("this is an example top-level error".to_owned());
            state.metadata.0.set(ds);
        }
    }

    fn get_edits(_state: &EditorState<Self>) -> Vec<EditMenuOption<Self>> {
        vec![
            EditMenuOption {
                pattern: EditMenuPattern::Dynamic("variable".to_owned(), |query| {
                    if query.is_empty() || GRAMMAR.keys().any(|x| x == query) {
                        return None;
                    }
                    Some(query.to_owned())
                }),
                edit: |query, mut state| {
                    state.handle = state.root.insert_fragment(
                        state.handle,
                        Fragment::Span(Span(vec![EditorExpr::new_lit(
                            "var".to_owned(),
                            vec![EditorExpr::new_lit(query.to_owned(), vec![])],
                        )])),
                    );
                    Some(state)
                },
            },
            EditMenuOption {
                pattern: EditMenuPattern::Dynamic("binding".to_owned(), |query| {
                    if query.is_empty() || GRAMMAR.keys().any(|x| x == query) {
                        return None;
                    }
                    Some(query.to_owned())
                }),
                edit: |query, mut state| {
                    state.handle = state.root.insert_fragment(
                        state.handle,
                        Fragment::Span(Span(vec![EditorExpr::new_lit(
                            "binding".to_owned(),
                            vec![EditorExpr::new_lit(query.to_owned(), vec![])],
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
            make_simple_edit_menu_option!["top"],
            make_simple_edit_menu_option!["bot"],
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
            make_simple_edit_menu_option!["lemma"],
        ]
    }

    fn is_valid_handle_specialized(h: &Handle, root: &MetaExpr<Self::M>) -> bool {
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
        ui: &mut egui::Ui,
        rc: RenderContext<'_, Self>,
        path: &Path,
        expr: &MetaExpr<Self::M>,
        render_steps_and_kids: Vec<(RenderPoint<'_>, Option<RenderExpr<'_, Self>>)>,
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

        let parenthesized = rule.kids.is_parenthesized();

        if parenthesized {
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
                        egui::RichText::new(if rule.kids.is_infixed() {
                            format!(" {label} ")
                        } else {
                            label.to_owned()
                        })
                        .color(if rule.kids.is_parenthesized() {
                            color_scheme.keyword_text
                        } else {
                            color_scheme.normal_text
                        })
                        .text_style(egui::TextStyle::Monospace)
                        .strong(),
                    )
                    .selectable(false),
                );
            });
        };

        if rule.kids.is_prefixed() {
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
                kid.render(
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
            if rule.kids.is_infixed() && i == 0 {
                render_label(ui);
            }
        }

        if parenthesized {
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
