pub mod ast {
    use std::fmt;
    use ustr::Ustr;

    #[derive(Clone, Debug)]
    pub enum Expr {
        Var(Ustr, i64),
        Abs(Ustr, Box<Expr>),
        App(Box<Expr>, Box<Expr>),
    }

    impl fmt::Display for Expr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Expr::Var(bndr, _) => write!(f, "{}", bndr),
                Expr::Abs(bndr, t1) => write!(f, "λ{}. {}", bndr, t1),
                Expr::App(t1, t2) => write!(f, "{} {}", t1, t2),
            }
        }
    }

    pub mod macros {
      macro_rules! var {
          ($var: expr, $i: expr) => {
              Var(ustr($var), $i)
          };
      }

      pub(crate) use var;

      macro_rules! abs {
          ($var: expr, $expr: expr) => {
              Abs(ustr($var), Box::new($expr))
          };
      }

      pub(crate) use abs;

      macro_rules! app {
          ($t1: expr, $t2: expr) => {
              App(Box::new($t1), Box::new($t2))
          };
      }

      pub(crate) use app;
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

use ast::Expr;
use ast::Expr::*;
use ast::macros::{abs, app, var};
use ustr::ustr;

pub fn true_t() -> Expr {
    abs!("p", abs!("q", var!("p", 1)))
}

pub fn false_t() -> Expr {
    abs!("p", abs!("q", var!("q", 0)))
}

pub fn not_t() -> Expr {
    abs!("z", app!(app!(var!("z", 0), false_t()), true_t()))
}

pub fn eval(term: Expr) -> Expr {
    evaluator::multi_step_eval(term)
}
