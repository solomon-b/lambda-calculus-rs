pub mod ast {
    use ustr::Ustr;

    #[derive(Clone, Debug)]
    pub enum Expr {
        Var(Ustr, i64),
        Abs(Ustr, Box<Expr>),
        App(Box<Expr>, Box<Expr>),
    }
}

mod substitution {
    use super::ast::Expr;
    use super::ast::Expr::*;

    fn shift(d: i64, c: i64, term: Expr) -> Expr {
        match term {
            Var(bndr, k) if k < c => Var(bndr, k),
            Var(bndr, k) => Var(bndr, k + d),
            Abs(bndr, t1) => Abs(bndr, Box::new(shift(d, c + 1, *t1))),
            App(t1, t2) => App(Box::new(shift(d, c, *t1)), Box::new(shift(d, c, *t2))),
        }
    }

    fn subst(j: i64, s: Expr, term: Expr) -> Expr {
        match term {
            Var(_, k) if k == j => s,
            Var(bndr, k) => Var(bndr, k),
            Abs(bndr, t1) => Abs(bndr, Box::new(subst(j + 1, shift(1, 0, s), *t1))),
            App(t1, t2) => App(
                Box::new(subst(j, s.clone(), *t1)),
                Box::new(subst(j, s, *t2)),
            ),
        }
    }

    pub fn subst_top(s: Expr, t: Expr) -> Expr {
        shift(-1, 0, subst(0, shift(1, 0, s), t))
    }
}

mod evaluator {
    use super::{ast::Expr, ast::Expr::*, substitution::subst_top};

    fn is_val(term: &Expr) -> bool {
        match term {
            Var(_, _) => false,
            Abs(_, _) => true,
            App(_, _) => false,
        }
    }

    fn single_eval(term: Expr) -> Option<Expr> {
        match term {
            App(t1, t2) => match *t1 {
                Abs(_, t12) if is_val(t2.as_ref()) => Some(subst_top(*t2.clone(), *t12)),

                v1 @ Abs(_, _) => single_eval(*t2).map(|t2| App(Box::new(v1), Box::new(t2))),

                _ => single_eval(*t1).map(|t1| App(Box::new(t1), t2)),
            },
            _ => None,
        }
    }

    pub fn multi_step_eval(mut term: Expr) -> Expr {
        loop {
            match single_eval(term.clone()) {
                Some(val) => {
                    term = val;
                    continue;
                }
                None => break,
            }
        }
        term
    }
}

use ast::Expr::*;
use ustr::ustr;

pub fn true_t() -> ast::Expr {
    Abs(
        ustr("p"),
        Box::new(Abs(ustr("q"), Box::new(Var(ustr("p"), 1)))),
    )
}

pub fn false_t() -> ast::Expr {
    Abs(
        ustr("p"),
        Box::new(Abs(ustr("q"), Box::new(Var(ustr("q"), 0)))),
    )
}

pub fn not_t() -> ast::Expr {
    Abs(
        ustr("z"),
        Box::new(App(
            Box::new(App(Box::new(Var(ustr("z"), 0)), Box::new(false_t()))),
            Box::new(true_t()),
        )),
    )
}

pub fn eval(term: ast::Expr) -> ast::Expr {
    evaluator::multi_step_eval(term)
}
