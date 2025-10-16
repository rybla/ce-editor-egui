#![allow(dead_code, clippy::println_empty_string, non_upper_case_globals)]

use lazy_static::lazy_static;
use map_macro::hash_map;
use std::cell::Cell;
use std::fmt::Debug;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
struct Expr<Ann> {
    label: String,
    annotation: Ann,
    kids: Vec<Expr<Ann>>,
}

/// annotated expr. The Cell type allows interior mutability, so that only the annotations can be mutated.
type AnnExpr = Expr<Cell<Vec<String>>>;
/// unannotated expr
type PlainExpr = Expr<()>;

impl Display for AnnExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ex!({}", self.label)?;
        if !self.kids.is_empty() {
            write!(f, ", [")?;
            for (i, kid) in self.kids.iter().enumerate() {
                write!(f, "{kid}")?;
                if i < self.kids.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "]")?;
        }
        write!(f, ", {:?}", self.annotation.take())?;
        write!(f, ")")
    }
}

impl Display for PlainExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ex!({}", self.label)?;
        if !self.kids.is_empty() {
            write!(f, ", [")?;
            for (i, kid) in self.kids.iter().enumerate() {
                write!(f, "{kid}")?;
                if i < self.kids.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "]")?;
        }
        write!(f, ")")
    }
}

impl<Ann> Expr<Ann> {
    pub fn pat(&self) -> (&str, &[Self]) {
        (self.label.as_str(), self.kids.as_slice())
    }

    pub fn pat_owned<const N: usize>(self) -> (String, [Self; N]) {
        (
            self.label,
            self.kids
                .try_into()
                .unwrap_or_else(|_| panic!("impossible")),
        )
    }

    pub fn forget_ann(self) -> PlainExpr {
        let Self { label, kids, .. } = self;
        Expr {
            label: label.clone(),
            annotation: (),
            kids: kids.into_iter().map(|x| x.forget_ann()).collect(),
        }
    }
}

macro_rules! ex {
    ( $label: expr ) => {
        Expr {
            label: $label.to_owned(),
            annotation: (),
            kids: vec![]
        }
    };
    ( $label: expr, [ $( $kid: expr ),* ] ) => {
        Expr {
            label: $label.to_owned(),
            annotation: (),
            kids: vec![ $( $kid ),* ]
        }
    };
    ( $label: expr, [ $( $kid: expr ),* ], $ann: expr ) => {
        Expr {
            label: $label.to_owned(),
            annotation: $ann,
            kids: vec![ $( $kid ),* ]
        }
    };
}

macro_rules! ex_ann {
    ( $label: expr ) => {
        Expr {
            label: $label.to_owned(),
            annotation: Cell::new(vec![]),
            kids: vec![],
        }
    };
    ( $label: expr, [ $( $kid: expr ),* ] ) => {
        Expr {
            label: $label.to_owned(),
            annotation: Cell::new(vec![]),
            kids: vec![$( $kid ),*],
        }
    };
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
// this is like Type but with less information
enum Sort {
    Proof,
    Prop,
    Num,
    Var,
}

impl Sort {
    pub fn to_type(&self) -> Type<'_> {
        match self {
            Proof => {
                panic!("this shouldn't be able to happen, can't recover details of sort to type")
            }
            Prop => Type::Prop,
            Num => Type::Num,
            Var => Type::Var,
        }
    }
}

use Sort::{Num, Proof, Prop, Var};

impl Display for Sort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

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
    pub kids: Vec<Sort>,
}

macro_rules! rule {
    ( $sort: expr ) => {
        Rule { sort: $sort, kids: vec![] }
    };
    ( $sort: expr, [ $( $kid: expr ),* ] ) => {
        Rule { sort: $sort, kids: vec![$( $kid ),*] }
    }
}

impl Rule {
    pub fn new(sort: Sort, kids: Vec<Sort>) -> Self {
        Self { sort, kids }
    }
}

/// This macro is a wrapper for the [`hash_map`] macro which lets you omit using
/// `to_owned` on each key when creating a [`HashMap`] with [`String`] keys.
macro_rules! string_hash_map {
    ( $( $key: literal => $val: expr ),* $(,)? ) => {
        hash_map!( $( $key.to_owned() => $val ),* )
    };
}

const and: &str = "and";
const top: &str = "top";
const bot: &str = "bot";
const intro_and: &str = "intro_and";
const intro_top: &str = "intro_top";

lazy_static! {
    static ref GRAMMAR: HashMap<String, Rule> = string_hash_map! {
        // Prop
        "top" => rule!(Prop),
        "bot" => rule!(Prop),
        "<" => rule!(Prop, [Num, Num]),
        ">" => rule!(Prop, [Num, Num]),
        ">=" => rule!(Prop, [Num, Num]),
        "<=" => rule!(Prop, [Num, Num]),
        "=" => rule!(Prop, [Num, Num]),
        "and" => rule!(Prop, [Prop, Prop]),
        "or" => rule!(Prop, [Prop, Prop]),
        "=>" => rule!(Prop, [Prop, Prop]),
        "not" => rule!(Prop, [Prop]),
        "forall" => rule!(Prop, [Var, Prop]),
        "exists" => rule!(Prop, [Var, Prop]),
        // Num
        "+" => rule!(Num, [Num, Num]),
        "-" => rule!(Num, [Num, Num]),
        "*" => rule!(Num, [Num, Num]),
        "/" => rule!(Num, [Num, Num]),
        // TODO: but how can we add number literals if the labels are strings?
        // maybe we'll add these too:
        "abs" => rule!(Num, [Num]),
        "ceil" => rule!(Num, [Num]),
        "floor" => rule!(Num, [Num]),
        // proof forms
        "intro_and" => rule!(Proof, [Proof, Proof]),
        "intro_top" => rule!(Proof),
        "elim_bot" => rule!(Proof, [Proof]),
    };
}

// proof forms:
//   - [x] intro_top
//   - [x] elim_bot
//   - [x] intro_and
//   - [ ] elim_and
//   - [ ] intro_or
//   - [ ] elim_or
//   - [ ] intro_var
//   - [ ] exists
//   - [ ] apply
//   - [ ] rewrite

fn from_expr_kids_to_array<T: Debug, const N: usize>(xs: Vec<T>) -> [T; N] {
    xs.try_into()
        .expect("impossible since we already checked the arity of expr")
}

fn check_type<'a>(
    ctx: HashMap<String, Type<'a>>,
    expected_type: &Type<'a>,
    expr: &AnnExpr,
) -> bool {
    let mut success = true;
    check_type_helper(&mut success, ctx, expected_type, expr);
    success
}

fn check_type_helper<'a>(
    success: &mut bool,
    ctx: HashMap<String, Type<'a>>,
    expected_type: &Type<'a>,
    expr: &AnnExpr,
) {
    let expected_sort = expected_type.get_sort();
    // clean up the old annotation
    {
        let mut annotation = expr.annotation.take();
        annotation.clear();
        expr.annotation.set(annotation);
    }

    // call this function to add error annotation, also sets the success flag
    let mut add_error = |msg: String| {
        let mut annotation = expr.annotation.take();
        annotation.push(msg);
        expr.annotation.set(annotation);
        *success = false;
    };

    // get the corresponding rule
    let Rule {
        sort: rule_sort,
        kids: rule_kids,
    } = match GRAMMAR.get(&expr.label) {
        None => {
            add_error("unknown expr label".to_owned());
            return;
        }
        Some(rule) => rule,
    };

    // check sort
    if rule_sort != &expected_sort {
        add_error(format!(
            "this expr was expected to have sort {expected_sort} but actually has sort {rule_sort}"
        ));
    }

    // check arity
    if expr.kids.len() != rule_kids.len() {
        let expr_kids_len = expr.kids.len();
        add_error(format!(
            "this expr was expected to have {} kids but actually has {} kids",
            rule_kids.len(),
            expr_kids_len
        ));
    }

    // TODO: write a function (instead of from_expr_kids_to_array) that takes the number of expected kids, the actual kids, and returns an array of exprs of the expected number of kids len, buffered by some extra dummy kids, and all extra kids are just given an annotation error that indicates they're out of place

    // check kid sorts
    match ((expr.label.as_str(), expr.kids.as_slice()), expected_type) {
        (("var", [x]), _) => {
            if !ctx.contains_key(&x.label) {
                add_error("this var expr is out-of-scope".to_owned());
            }
        }
        (("forall" | "exists", [x, p]), _) => {
            let ctx = {
                let mut ctx = ctx;
                ctx.insert(x.label.clone(), Type::Num);
                ctx
            };
            check_type_helper(success, ctx, &rule_kids[1].to_type(), p);
        }
        // The proof step cases are handled here
        ((label, _), Type::Proof(prop)) => match ((label, expr.kids.as_slice()), prop.pat()) {
            (("intro_and", [a, b]), ("and", [p, q])) => {
                // let [a, b] = from_expr_kids_to_array(expr.kids);
                check_type_helper(success, ctx.clone(), &Type::Proof(p), a);
                check_type_helper(success, ctx.clone(), &Type::Proof(q), b);
            }
            (("intro_top", []), ("top", [])) => {}
            (("elim_bot", [a]), _) => {
                check_type_helper(success, ctx, &Type::Proof(&ex!(bot)), a);
            }
            _ => {
                add_error("invalid proof".to_owned());
            }
        },
        // default case that doesn't interact with environment, and uses simple sorts
        _ => {
            for (kid, expected_kid_sort) in expr.kids.iter().zip(rule_kids.iter()) {
                check_type_helper(success, ctx.clone(), &expected_kid_sort.to_type(), kid);
            }
        }
    }
}

fn test<'a>(ctx: &HashMap<String, Type<'a>>, expected_prop: AnnExpr, proof: &AnnExpr) {
    let prop_well_typed = check_type(ctx.clone(), &Type::Prop, &expected_prop);
    let expected_prop = expected_prop.forget_ann();
    let term_well_typed = check_type(ctx.clone(), &Type::Proof(&expected_prop), proof);
    println!("");
    println!("{ctx:?} |-");
    println!("     {proof}");
    println!("        : {expected_prop}");
    println!(" ==> {}", prop_well_typed && term_well_typed);
}

pub fn main() {
    test(
        &hash_map! {},
        ex_ann!(and, [ex_ann!(top), ex_ann!(top)]),
        &ex_ann!(intro_and, [ex_ann!(intro_top), ex_ann!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex_ann!(and, [ex_ann!(top), ex_ann!(bot)]),
        &ex_ann!(intro_and, [ex_ann!(intro_top), ex_ann!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex_ann!(and, [ex_ann!(bot), ex_ann!(top)]),
        &ex_ann!(intro_and, [ex_ann!(intro_top), ex_ann!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex_ann!(and, [ex_ann!(bot), ex_ann!(bot)]),
        &ex_ann!(intro_and, [ex_ann!(intro_top), ex_ann!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex_ann!("and", [ex_ann!(top), ex_ann!(top)]),
        &ex_ann!(
            intro_and,
            [
                ex_ann!(intro_and, [ex_ann!(intro_top), ex_ann!(intro_top)]),
                ex_ann!(intro_top)
            ]
        ),
    );
}
