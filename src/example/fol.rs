#![allow(clippy::manual_let_else, clippy::enum_glob_use)]

use crate::{
    editor::{self, *},
    editor_ex, ex,
    expr::*,
    utility::pop_front,
};
use egui::RichText;
use itertools::Either;
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
    TopLevel,
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
            Self::TopLevel => TopLevel,
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
            Self::TopLevel => write!(f, "TopLevel"),
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

    pub fn matches_arity<'a, M>(&self, span: &SimplifiedEditorSpan<'a, M>) -> bool {
        match (self, span) {
            (FixedArity(sorts, _), SimplifiedEditorSpan::Fixed(kids)) => sorts.len() == kids.len(),
            (FixedArity(sorts, _), SimplifiedEditorSpan::Free(kids))
                if sorts.is_empty() && kids.is_empty() =>
            {
                true
            }
            (FreeArity(_sort), SimplifiedEditorSpan::Free(_kids)) => true,
            (LiteralKid, SimplifiedEditorSpan::Free(kids)) => kids.len() == 1,
            _ => false,
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
fn get_sorts(span: &Span<MetaLabel<M>>) -> Vec<Option<&Sort>> {
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

                let mut kids: Vec<MetaExpr<M>> = vec![];
                match &rule.kids {
                    FixedArity(sorts, _) => {
                        for _ in 0..sorts.len() {
                            kids.push(ex![
                                EditorLabel::new(Constructor::PosArg, MutMetadata::default()),
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
                                label: EditorLabel::new(
                                    Constructor::Literal($name.to_owned()),
                                    MutMetadata::default(),
                                ),
                                span_l: Span::empty(),
                                span_r: Span(kids),
                            }],
                            Some(tooth_into_first_kid) => vec![
                                Tooth {
                                    label: EditorLabel::new(
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

fn check(ctx: HashMap<String, Type>, expected_type: &Type, expr: &MetaExpr<M>) -> bool {
    let mut success = true;
    check_helper(&mut success, ctx, expected_type, expr);
    success
}

// places new metadata corresponding to the new type errors.
fn check_helper(
    success: &mut bool,
    ctx: HashMap<String, Type>,
    expected_type: &Type,
    expr: &MetaExpr<M>,
) {
    // call this function to add error annotation, also sets the success flag
    let mut add_error = |e: String| {
        expr.label.metadata.modify(|mut m| {
            m.errors.push(e);
            m
        });
        *success = false;
    };

    fn check_fixed_kid<'a>(
        success: &mut bool,
        ctx: &HashMap<String, Type>,
        expected_type: &'a Type,
        kid: &SimplifiedFixedKid<'a, MutMetadata<M>>,
    ) {
        match kid {
            Err(pos_arg) => {
                pos_arg.label.metadata.modify(|mut m| {
                    m.ty_info = Some(TyInfo {
                        ctx: ctx.clone(),
                        ty: expected_type.clone(),
                    });
                    if pos_arg.kids.0.is_empty() {
                        m.push_error("hole".to_owned());
                    } else {
                        m.push_error("overflow".to_owned());
                    }
                    m
                });

                *success = false;
            }
            Ok(kid) => check_helper(success, ctx.clone(), expected_type, kid),
        }
    }

    //

    expr.label.metadata.modify(|mut m| {
        m.ty_info = Some(TyInfo {
            ctx: ctx.clone(),
            ty: expected_type.clone(),
        });
        m
    });

    let expected_sort = expected_type.get_sort();

    match (&expr.label.constructor, expr.kids.simplify()) {
        (Constructor::Root, SimplifiedEditorSpan::Free(kids)) => {
            for kid in kids {
                check_helper(success, ctx.clone(), &Type::Declaration, kid);
            }
        }
        (Constructor::Literal(lit), kids) => {
            // get the corresponding rule
            let Rule {
                sort: rule_sort,
                kids: rule_kids,
            } = match GRAMMAR.get(lit.as_str()) {
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
            if !rule_kids.matches_arity(&kids) {
                add_error(format!(
                    "expected {}; actually {}",
                    match &rule_kids {
                        FixedArity(sorts, _) => format!("{} fixed kid(s)", sorts.len()),
                        FreeArity(_sort) => "free kids".to_owned(),
                        LiteralKid => "a literal kid".to_owned(),
                    },
                    match &kids {
                        SimplifiedEditorSpan::Free(kids) => format!("{} free kid(s)", kids.len()),
                        SimplifiedEditorSpan::Fixed(kids) => format!("{} fixed ki(s)", kids.len()),
                    }
                ));
            }

            match ((lit.as_str(), kids.as_ref()), expected_type) {
                (("lemma", Either::Right([x, sig, imp])), Type::Declaration) => {
                    check_fixed_kid(success, &ctx, &Type::Binding, x);
                    let x = if *success
                        && let Ok(x) = x
                        && let Some(x) = x.kids.0.first()
                    {
                        x.label.constructor.expect_literal()
                    } else {
                        return;
                    };

                    check_fixed_kid(success, &ctx, &Type::Prop, sig);
                    let sig = if *success && let Ok(sig) = sig {
                        sig.to_plain()
                    } else {
                        return;
                    };

                    let ctx = {
                        let mut ctx = ctx;
                        ctx.insert(x.clone(), Type::Prop);
                        ctx
                    };

                    check_fixed_kid(success, &ctx, &Type::Proof(sig), imp);
                }
                (("forall" | "exists", Either::Right([x, a])), _) => {
                    check_fixed_kid(success, &ctx, &Type::Binding, x);
                    let x = if *success
                        && let Ok(x) = x
                        && let Some(x) = x.kids.0.first()
                    {
                        x.label.constructor.expect_literal()
                    } else {
                        return;
                    };

                    let ctx = {
                        let mut ctx = ctx;
                        ctx.insert(x.clone(), Type::Prop);
                        ctx
                    };

                    check_fixed_kid(success, &ctx, &Type::Prop, a);
                }
                ((proof_con, proof_kids), Type::Proof(prop)) => {
                    match (
                        (proof_con, proof_kids),
                        (
                            prop.label.constructor.expect_literal().as_str(),
                            prop.kids.0.as_slice(),
                        ),
                    ) {
                        (("intro_and", Either::Right([a, b])), ("and", [p, q])) => {
                            check_fixed_kid(success, &ctx, &Type::Proof(p.clone()), a);
                            check_fixed_kid(success, &ctx, &Type::Proof(q.clone()), b);
                        }
                        (("intro_top", Either::Right([])), ("top", [])) => {}
                        (("elim_bot", Either::Right([a])), _) => {
                            check_fixed_kid(success, &ctx, &Type::Proof(editor_ex!["bot", []]), a);
                        }
                        _ => {
                            expr.label.metadata.modify(|mut m| {
                                m.push_error("invalid".to_owned());
                                m
                            });
                        }
                    }
                }
                _ => match rule_kids {
                    FixedArity(rule_kids, _) => {
                        assert_eq!(
                            expr.kids.0.len(),
                            rule_kids.len(),
                            "a form with FixedArity must have the proper number of kids"
                        );
                        for (kid, expected_kid_sort) in expr.kids.0.iter().zip(rule_kids.iter()) {
                            check_helper(success, ctx.clone(), &expected_kid_sort.to_type(), kid);
                        }
                    }
                    FreeArity(expected_kid_sort) => {
                        let expected_kid_type = expected_kid_sort.to_type();
                        for kid in &expr.kids.0 {
                            check_helper(success, ctx.clone(), &expected_kid_type, kid);
                        }
                    }
                    LiteralKid => {
                        if let SimplifiedEditorSpan::Free(kids) = kids
                            && let [x] = kids.as_ref()
                            && let Constructor::Literal(_) = x.label.constructor
                        {
                        } else {
                            add_error("expected a literal kid".to_owned());
                        }
                    }
                },
            }
        }
        _ => panic!("I don't know how to handle this EditorExpr: {expr}"),
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
                EditorLabel {
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

        // clear old errors
        state.root.map(&mut |m| {
            m.metadata.modify(|mut m| {
                m.errors.clear();
                m
            });
        });

        // TODO: the &Type::Declaration is ignored here...
        check(hash_map! {}, &Type::Declaration, &state.root);

        state.metadata.modify(|mut m| {
            m.push_error("this is an example top-level error".to_owned());
            m
        });
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
