#![allow(dead_code)]

use lazy_static::lazy_static;
use map_macro::hash_map;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Expr {
    label: String,
    kids: Vec<Expr>,
}

impl Expr {
    pub fn pat(&self) -> (&str, &[Self]) {
        (self.label.as_str(), self.kids.as_slice())
    }
}

macro_rules! ex {
    ( $label: expr ) => {
        Expr { label: $label.to_owned(), kids: vec![] }
    };
    ( $label:expr, [ $( $kid: expr ),* ] ) => {
        Expr { label: $label.to_owned(), kids: vec![ $( $kid ),* ] }
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

struct Rule {
    pub sort: Sort,
    pub kids: Vec<Sort>,
}

impl Rule {
    pub fn new(sort: Sort, kids: Vec<Sort>) -> Self {
        Self { sort, kids }
    }
}

lazy_static! {
    static ref GRAMMAR: HashMap<String, Rule> = hash_map! {
        // Prop
        "true".to_owned() => Rule::new(Prop, vec![]),
        "false".to_owned() => Rule::new(Prop, vec![]),
        "<".to_owned() => Rule::new(Prop, vec![Num, Num]),
        ">".to_owned() => Rule::new(Prop, vec![Num, Num]),
        ">=".to_owned() => Rule::new(Prop, vec![Num, Num]),
        "<=".to_owned() => Rule::new(Prop, vec![Num, Num]),
        "=".to_owned() => Rule::new(Prop, vec![Num, Num]),
        "and".to_owned() => Rule::new(Prop, vec![Prop, Prop]),
        "or".to_owned() => Rule::new(Prop, vec![Prop, Prop]),
        "=>".to_owned() => Rule::new(Prop, vec![Prop, Prop]),
        "not".to_owned() => Rule::new(Prop, vec![Prop]),
        "forall".to_owned() => Rule::new(Prop, vec![Var, Prop]),
        "exists".to_owned() => Rule::new(Prop, vec![Var, Prop]),
        // Num
        "+".to_owned() => Rule::new(Num, vec![Num, Num]),
        "-".to_owned() => Rule::new(Num, vec![Num, Num]),
        "*".to_owned() => Rule::new(Num, vec![Num, Num]),
        "/".to_owned() => Rule::new(Num, vec![Num, Num]),
        // TODO: but how can we add number literals if the labels are strings?
        // maybe we'll add these too:
        "abs".to_owned() => Rule::new(Num, vec![Num]),
        "ceil".to_owned() => Rule::new(Num, vec![Num]),
        "floor".to_owned() => Rule::new(Num, vec![Num]),
        // proof forms
        "and_intro".to_owned() => Rule::new(Proof, vec![Proof, Proof]),
        "true_intro".to_owned() => Rule::new(Proof, vec![]),
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

fn check_sort(ctx: HashMap<String, Type>, expected_sort: &Sort, expr: &Expr) -> bool {
    let Rule { sort, kids } = match GRAMMAR.get(&expr.label) {
        None => return false,
        Some(rule) => rule,
    };
    if sort != expected_sort {
        return false;
    }
    if expr.kids.len() != kids.len() {
        return false;
    }

    match expr.label.as_str() {
        "var" => {
            let x = &expr.kids[0].label;
            ctx.contains_key(x)
        }
        "forall" | "exists" => {
            let x = &expr.kids[0].label;
            let p = &expr.kids[1];
            let mut ctx = ctx;
            ctx.insert(x.clone(), Type::Num);
            check_sort(ctx, &kids[1], p)
        }
        _ => {
            for (kid, expected_kid_sort) in expr.kids.iter().zip(kids.iter()) {
                if !check_sort(ctx.clone(), expected_kid_sort, kid) {
                    return false;
                }
            }
            true
        }
    }
}

#[derive(Debug, Clone)]
enum Type {
    Num,
    Proof(Expr),
}

/// Checks that a proof is valid with respect to an expected [`Prop`].
/// - Assuems that all the [`Type`]s in the context have been sort-checked.
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

pub fn main() {
    println!(
        "{}",
        check_proof(
            HashMap::new(),
            &ex!("and", [ex!("true"), ex!("true")]),
            &ex!("and_intro", [ex!("true_intro"), ex!("true_intro")]),
        )
    );
}
