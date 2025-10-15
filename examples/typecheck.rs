#![allow(dead_code, clippy::println_empty_string, non_upper_case_globals)]

use lazy_static::lazy_static;
use map_macro::hash_map;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
struct Expr {
    label: String,
    annotation: Option<String>,
    kids: Vec<Expr>,
}

impl Display for Expr {
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
        if let Some(ann) = &self.annotation {
            write!(f, ", {ann}")?;
        }
        write!(f, ")")
    }
}

impl Expr {
    pub fn pat(&self) -> (&str, &[Self]) {
        (self.label.as_str(), self.kids.as_slice())
    }

    pub fn pat_owned<const N: usize>(self) -> (String, [Self; N]) {
        (self.label, self.kids.try_into().expect("impossible"))
    }

    pub fn annotate(&self, msg : String) -> Self {
        Self {
            annotation: Some(msg),
            ..self.clone()
        }
    }
}

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

fn from_vec_to_array<T, const N: usize>(xs: Vec<T>) -> Result<[T; N], Vec<T>> {
    xs.try_into()
}

fn check_sort(ctx: HashMap<String, Type>, expected_sort: &Sort, expr: Expr) -> Expr {
    // check sort
    let Rule { sort, kids } = match GRAMMAR.get(&expr.label) {
        None => {
            return Expr {
                label: expr.label,
                annotation: Some("unknown expr label".to_owned()),
                kids: expr.kids,
            };
        }
        Some(rule) => rule,
    };
    if sort != expected_sort {
        return expr.annotate( format!(
            "this expr was expected to have sort {expected_sort} but actually has sort {sort}"
        ));
    }

    // check arity
    if expr.kids.len() != kids.len() {
        return expr.annotate(format!(
                "this expr was expected to have {} kids but actually has {} kids",
                kids.len(),
                expr.kids.len()
            ));
    }

    // check kid sorts
    match expr.label.as_str() {
        "var" => {
            let [x] = from_vec_to_array(expr.kids).expect("impossible since we already checked the arity of expr");
            if !ctx.contains_key(&x.label) {
                return Expr {
                    label: expr.label,
                    annotation: Some("this var expr is out-of-scope".to_owned()),
                    kids: vec![x],
                };
            }
            Expr {
                label: expr.label,
                annotation: None,
                kids: vec![x],
            }
        }
        "forall" | "exists" => {
            let [x, p] = from_vec_to_array(expr.kids).expect("impossible since we already checked the arity of expr");
            let ctx = {
                let mut ctx = ctx;
                ctx.insert(x.label.clone(), Type::Num);
                ctx
            };
            let p = check_sort(ctx, &kids[1], p);
            Expr {
                label: expr.label,
                annotation: None,
                kids: vec![x, p],
            }
        }
        // default case that doesn't interact with environment
        _ => {
            let expr_label = expr.label;
            let expr_kids: Vec<Expr> = expr
                .kids
                .into_iter()
                .zip(kids.iter())
                .map(|(kid, expected_kid_sort)| check_sort(ctx.clone(), expected_kid_sort, kid))
                .collect();
            Expr {
                label: expr_label,
                annotation: None,
                kids: expr_kids,
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
    // check sort
    let Rule { sort, kids } = match GRAMMAR.get(&proof.label) {
        None => {
            return Expr {
                label: proof.label,
                annotation: Some("unknown expr label".to_owned()),
                kids: proof.kids,
            };
        }
        Some(rule) => rule,
    };
    if sort != &Proof {
        return proof.annotate(format!(
            "expected expr to have sort Proof, but actually has sort {sort}"
        ));
    }

    // check arity
    if proof.kids.len() != kids.len() {
        return proof.annotate(format!(
            "expected expr to have {} kids, but actually has {} kids",
            kids.len(),
            proof.kids.len()
        ));
    }

    // check validity
    match (proof.label.as_str(), expected_prop.pat()) {
        ("intro_and", ("and", [p, q])) => {
            let [a, b] = from_vec_to_array(proof.kids)
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
            let [a] = from_vec_to_array(proof.kids)
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

fn test(ctx: &HashMap<String, Type>, expected_prop: Expr, proof: Expr) {
    let expected_prop = check_sort(ctx.clone(), &Prop, expected_prop);
    let proof = check_sort(ctx.clone(), &Proof, proof);
    let result = check_proof(ctx.clone(), &expected_prop, proof.clone());
    println!("");
    println!("{ctx:?} |-");
    println!("     {proof}");
    println!("        : {expected_prop}");
    println!(" ==> {result}");
}

pub fn main() {
    test(
        &hash_map! {},
        ex!(and, [ex!(top), ex!(top)]),
        ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex!(and, [ex!(top), ex!(bot)]),
        ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex!(and, [ex!(bot), ex!(top)]),
        ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex!(and, [ex!(bot), ex!(bot)]),
        ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
    );
    test(
        &hash_map! {},
        ex!("and", [ex!(top), ex!(top)]),
        ex!(
            intro_and,
            [
                ex!(intro_and, [ex!(intro_top), ex!(intro_top)]),
                ex!(intro_top)
            ]
        ),
    );
}
