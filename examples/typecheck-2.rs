#![allow(dead_code, clippy::println_empty_string, non_upper_case_globals)]

use lazy_static::lazy_static;
use map_macro::hash_map;
use std::fmt::Debug;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug)]
struct Expr<Ann> {
    label: String,
    annotation: Ann,
    kids: Vec<Expr<Ann>>,
}

/// annotated expr
type AnnExpr<'a> = Expr<&'a mut Vec<String>>;

impl<Ann> Display for Expr<Ann> {
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
        // if let Some(ann) = &self.annotation {
        //     write!(f, ", {ann}")?;
        // }
        write!(f, ")")
    }
}

impl<Ann: Debug> Expr<Ann> {
    pub fn pat(&self) -> (&str, &[Self]) {
        (self.label.as_str(), self.kids.as_slice())
    }

    pub fn pat_owned<const N: usize>(self) -> (String, [Self; N]) {
        (self.label, self.kids.try_into().expect("impossible"))
    }
}

// impl<'a> AnnExpr<'a> {
//     pub fn annotate<'a>(self, msg: String) -> Self {
//         let mut expr = self;
//         expr.annotation = &mut vec![msg];
//         expr
//     }
// }

macro_rules! ex {
    ( $label: expr ) => {
        Expr {
            label: $label.to_owned(),
            annotation: None,
            kids: vec![]
        }
    };
    ( $label: expr, [ $( $kid: expr ),* ] ) => {
        Expr {
            label: $label.to_owned(),
            annotation: None,
            kids: vec![ $( $kid ),* ]
        }
    };
    ( $label: expr, [ $( $kid: expr ),* ], $ann: expr ) => {
        Expr {
            label: $label.to_owned(),
            annotation: $ann.to_owned(),
            kids: vec![ $( $kid ),* ]
        }
    };
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
enum Sort {
    Proof,
    Prop,
    Num,
    Var,
}

use Sort::{Num, Proof, Prop, Var};

impl Display for Sort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
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

fn check_sort<'a>(ctx: HashMap<String, Type>, expected_sort: &Sort, expr: AnnExpr<'a>) -> bool {
    let mut success = true;
    check_sort_helper(&mut success, ctx, expected_sort, expr);
    success
}

fn check_sort_helper<'a>(
    success: &mut bool,
    ctx: HashMap<String, Type>,
    expected_sort: &Sort,
    expr: AnnExpr<'a>,
) {
    // clean up the old annotation
    expr.annotation.drain(..);

    // call this function to add error annotation, also sets the success flag
    let mut add_error = |msg: String| {
        expr.annotation.push(msg);
        *success = false
    };

    // get the corresponding rule
    let Rule { sort, kids } = match GRAMMAR.get(&expr.label) {
        None => {
            add_error("unknown expr label".to_owned());
            return ();
        }
        Some(rule) => rule,
    };

    // check sort
    if sort != expected_sort {
        add_error(format!(
            "this expr was expected to have sort {expected_sort} but actually has sort {sort}"
        ));
    }

    // check arity
    if expr.kids.len() != kids.len() {
        let expr_kids_len = expr.kids.len();
        add_error(format!(
            "this expr was expected to have {} kids but actually has {} kids",
            kids.len(),
            expr_kids_len
        ));
    }

    // TODO: write a function (instead of from_expr_kids_to_array) that takes the number of expected kids, the actual kids, and returns an array of exprs of the expected number of kids len, buffered by some extra dummy kids, and all extra kids are just given an annotation error that indicates they're out of place

    // check kid sorts
    match expr.label.as_str() {
        "var" => {
            let [x] = from_expr_kids_to_array(expr.kids);
            if !ctx.contains_key(&x.label) {
                add_error("this var expr is out-of-scope".to_owned());
            }
        }
        "forall" | "exists" => {
            let [x, p] = from_expr_kids_to_array(expr.kids);
            let ctx = {
                let mut ctx = ctx;
                ctx.insert(x.label.clone(), Type::Num);
                ctx
            };
            check_sort_helper(success, ctx, &kids[1], p);
        }
        // default case that doesn't interact with environment
        _ => {
            for (kid, expected_kid_sort) in expr.kids.into_iter().zip(kids.iter()) {
                check_sort_helper(success, ctx.clone(), expected_kid_sort, kid);
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Type {
    Num,
    Proof(Expr),
}

/// Checks that a proof is valid with respect to an expected [`Prop`].
/// - Assumes that all the [`Type`]s in the context have been sort-checked.
/// - Assumes that [`expected_prop`] has been sort-checked.
fn check_proof(ctx: HashMap<String, Type>, expected_prop: &Expr, proof: Expr) -> Expr {
    // get the corresponding rule
    let Rule { sort, kids } = match GRAMMAR.get(&proof.label) {
        None => return proof.annotate("unknown expr label".to_owned()),
        Some(rule) => rule,
    };
    // check sort
    if sort != &Proof {
        return proof.annotate(format!(
            "expected expr to have sort Proof, but actually has sort {sort}"
        ));
    }

    // check arity
    if proof.kids.len() != kids.len() {
        let proof_kids_len = proof.kids.len();
        return proof.annotate(format!(
            "expected expr to have {} kids, but actually has {} kids",
            kids.len(),
            proof_kids_len
        ));
    }

    // check validity
    match (proof.label.as_str(), expected_prop.pat()) {
        ("intro_and", ("and", [p, q])) => {
            let [a, b] = from_expr_kids_to_array(proof.kids)
                .expect("impossible since we already checked the arity of proof");
            let a = check_proof(ctx.clone(), p, a);
            let b = check_proof(ctx.clone(), q, b);
            Expr {
                label: proof.label,
                annotation: None,
                kids: vec![a, b],
            }
        }
        ("intro_top", ("top", [])) => Expr {
            label: proof.label,
            annotation: None,
            kids: vec![],
        },
        ("elim_bot", _) => {
            let [a] = from_expr_kids_to_array(proof.kids)
                .expect("impossible since we already checked the arity of proof");
            let a = check_proof(ctx, &ex!["bot"], a);
            Expr {
                label: proof.label,
                annotation: None,
                kids: vec![a],
            }
        }
        _ => Expr {
            label: proof.label,
            annotation: Some(format!(
                "this proof is not a valid proof of the prop {expected_prop}"
            )),
            kids: proof.kids,
        },
    }
}

// fn test(ctx: &HashMap<String, Type>, expected_prop: Expr, proof: Expr) {
//     let expected_prop = check_sort_helper(ctx.clone(), &Prop, expected_prop);
//     let proof = check_sort_helper(ctx.clone(), &Proof, proof);
//     let result = check_proof(ctx.clone(), &expected_prop, proof.clone());
//     println!("");
//     println!("{ctx:?} |-");
//     println!("     {proof}");
//     println!("        : {expected_prop}");
//     println!(" ==> {result}");
// }

// pub fn main() {
//     test(
//         &hash_map! {},
//         ex!(and, [ex!(top), ex!(top)]),
//         ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
//     );
//     test(
//         &hash_map! {},
//         ex!(and, [ex!(top), ex!(bot)]),
//         ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
//     );
//     test(
//         &hash_map! {},
//         ex!(and, [ex!(bot), ex!(top)]),
//         ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
//     );
//     test(
//         &hash_map! {},
//         ex!(and, [ex!(bot), ex!(bot)]),
//         ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
//     );
//     test(
//         &hash_map! {},
//         ex!("and", [ex!(top), ex!(top)]),
//         ex!(
//             intro_and,
//             [
//                 ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
//                 ex!(intro_top)
//             ]
//         ),
//     );
// }
