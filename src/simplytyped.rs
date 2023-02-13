pub mod ast {
    #[derive(Clone, Debug)]
    pub enum Expr {
        Var(ustr::Ustr, i64),
        Abs(ustr::Ustr, Box<Expr>),
        App(Box<Expr>, Box<Expr>),
    }

    pub fn var(var: ustr::Ustr, n: i64) -> Expr {
        Expr::Var(var, n)
    }

    pub fn abs(bndr: ustr::Ustr, body: Expr) -> Expr {
        Expr::Abs(bndr, Box::new(body))
    }

    pub fn app(t1: Expr, t2: Expr) -> Expr {
        Expr::App(Box::new(t1), Box::new(t2))
    }
}

mod substitution {
    use super::ast::Expr;

    fn shift(d: i64, c: i64, term: Expr) -> Expr {
        match term {
            Expr::Var(bndr, k) if k < c => Expr::Var(bndr, k),
            Expr::Var(bndr, k) => Expr::Var(bndr, k + d),
            Expr::Abs(bndr, t1) => Expr::Abs(bndr, Box::new(shift(d, c + 1, *t1))),
            Expr::App(t1, t2) => Expr::App(
                Box::new(shift(d, c, *t1)),
                Box::new(shift(d, c, *t2)),
            ),
        }
    }

    fn subst(j: i64, s: Expr, term: Expr) -> Expr {
        match term {
            Expr::Var(_, k) if k == j => s,
            Expr::Var(bndr, k) => Expr::Var(bndr, k),
            Expr::Abs(bndr, t1) => Expr::Abs(bndr, Box::new(subst(j + 1, shift(1, 0, s), *t1))),
            Expr::App(t1, t2) => Expr::App(
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
    use super::{ast::Expr, substitution};

    fn is_val(term: &Expr) -> bool {
        match term {
            Expr::Var(_, _) => false,
            Expr::Abs(_, _) => true,
            Expr::App(_, _) => false,
        }
    }

    fn single_eval(term: Expr) -> Option<Expr> {
        match term {
            Expr::App(t1, t2) => match *t1 {
                Expr::Abs(_, t12) if is_val(t2.as_ref()) => Some(substitution::subst_top(
                    *t2.clone(),
                    *t12,
                )),

                v1 @ Expr::Abs(_, _) => {
                    single_eval(*t2).map(|t2| Expr::App(Box::new(v1), Box::new(t2)))
                }

                _ => single_eval(*t1).map(|t1| Expr::App(Box::new(t1), t2)),
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

pub fn true_t() -> ast::Expr {
    ast::abs(ustr::ustr("p"), ast::abs(ustr::ustr("q"), ast::var(ustr::ustr("p"), 1)))
}

pub fn false_t() -> ast::Expr {
    ast::abs(ustr::ustr("p"), ast::abs(ustr::ustr("q"), ast::var(ustr::ustr("q"), 0)))
}

pub fn not_t() -> ast::Expr {
    ast::abs(
        ustr::ustr("z"),
        ast::app(ast::app(ast::var(ustr::ustr("z"), 0), false_t()), true_t()),
    )
}

pub fn app<'a>(t1: ast::Expr, t2: ast::Expr) -> ast::Expr {
    ast::app(t1, t2)
}

pub fn eval(term: ast::Expr) -> ast::Expr {
    evaluator::multi_step_eval(term)
}
