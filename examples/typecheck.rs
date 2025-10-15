#![allow(dead_code)]

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
        write!(f, "ex!(\"{}\"", self.label)?;
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
            write!(f, ", \"{ann}\"")?;
        }
        write!(f, ")")
    }
}

impl Expr {
    pub fn pat(&self) -> (&str, &[Self]) {
        (self.label.as_str(), self.kids.as_slice())
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
    ( $label:expr, [ $( $kid: expr ),* ] ) => {
        Expr {
            label: $label.to_owned(),
            annotation: None,
            kids: vec![ $( $kid ),* ]
        }
    };
    ( $label:expr, [ $( $kid: expr ),* ], $ann: literal ) => {
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

lazy_static! {
    static ref GRAMMAR: HashMap<String, Rule> = string_hash_map! {
        // Prop
        "true" => rule!(Prop),
        "false" => rule!(Prop),
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
        "and_intro" => rule!(Proof, [Proof, Proof]),
        "true_intro" => rule!(Proof),
    };
}

// proof forms:
//   - and_intro
//   - and_elim
//   - or_intro
//   - or_elim
//   - intro
//   - exists
//   - apply
//   - rewrite

fn from_vec_to_array<T, const N: usize>(xs: Vec<T>) -> Result<[T; N], Vec<T>> {
    xs.try_into()
}

fn check_sort(ctx: HashMap<String, Type>, expected_sort: &Sort, expr: Expr) -> Expr {
    let Rule { sort, kids } = match GRAMMAR.get(&expr.label) {
        None => return expr,
        Some(rule) => rule,
    };
    if sort != expected_sort {
        return Expr {
            label: expr.label,
            annotation: Some(format!(
                "this expr was expected to have sort {expected_sort} but actually has sort {sort}"
            )),
            kids: expr.kids,
        };
    }
    if expr.kids.len() != kids.len() {
        return Expr {
            label: expr.label,
            annotation: Some(format!(
                "this expr was expected to have {} kids but actually has {} kids",
                kids.len(),
                expr.kids.len()
            )),
            kids: expr.kids,
        };
    }

    match expr.label.as_str() {
        "var" => {
            let [x] = from_vec_to_array::<_, 1>(expr.kids).expect("impossible");
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
            let [x, p] = from_vec_to_array::<_, 2>(expr.kids).expect("impossible");
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
fn check_proof(ctx: HashMap<String, Type>, expected_prop: &Expr, proof: &Expr) -> bool {
    // sort-check
    let Rule { sort, kids } = match GRAMMAR.get(&proof.label) {
        None => return false,
        Some(rule) => rule,
    };
    if sort != &Proof {
        return false;
    }
    if proof.kids.len() != kids.len() {
        return false;
    }

    // proof-check
    match (proof.pat(), expected_prop.pat()) {
        (("and_intro", [a, b]), ("and", [p, q])) => {
            check_proof(ctx.clone(), p, a) && check_proof(ctx, q, b)
        }
        (("true_intro", []), ("true", [])) => true,
        _ => false,
    }
}

fn test(ctx: &HashMap<String, Type>, expected_prop: Expr, proof: Expr) {
    let expected_prop = check_sort(ctx.clone(), &Prop, expected_prop);
    let proof = check_sort(ctx.clone(), &Proof, proof);
    let result = check_proof(ctx.clone(), &expected_prop, &proof);
    println!("{ctx:?} |-");
    println!("     {proof} : {expected_prop}");
    println!(" ==> {result}");
}

pub fn main() {
    test(
        &hash_map! {},
        ex!("and", [ex!("true"), ex!("true")]),
        ex!("and_intro", [ex!("true_intro"), ex!("true_intro")]),
    );
    test(
        &hash_map! {},
        ex!("and", [ex!("true"), ex!("false")]),
        ex!("and_intro", [ex!("true_intro"), ex!("true_intro")]),
    );
    test(
        &hash_map! {},
        ex!("and", [ex!("false"), ex!("true")]),
        ex!("and_intro", [ex!("true_intro"), ex!("true_intro")]),
    );
    test(
        &hash_map! {},
        ex!("and", [ex!("false"), ex!("false")]),
        ex!("and_intro", [ex!("true_intro"), ex!("true_intro")]),
    );
}
