pub mod ast {
    #[derive(Clone, Debug)]
    pub enum Expr<'a> {
        Var(&'a str, i64),
        Abs(&'a str, Box<Expr<'a>>),
        App(Box<Expr<'a>>, Box<Expr<'a>>),
    }

    pub fn var<'a>(var: &'a str, n: i64) -> Expr<'a> {
        Expr::Var(var, n)
    }

    pub fn abs<'a>(bndr: &'a str, body: Expr<'a>) -> Expr<'a> {
        Expr::Abs(bndr, Box::new(body))
    }

    pub fn app<'a>(t1: Expr<'a>, t2: Expr<'a>) -> Expr<'a> {
        Expr::App(Box::new(t1), Box::new(t2))
    }
}

mod substitution {
    use super::ast::Expr;

    fn shift<'a>(d: i64, c: i64, term: &Expr<'a>) -> Expr<'a> {
        match term {
            Expr::Var(bndr, k) if k < &c => Expr::Var(bndr, *k),
            Expr::Var(bndr, k) => Expr::Var(&bndr, k + d),
            Expr::Abs(bndr, t1) => Expr::Abs(bndr, Box::new(shift(d, c + 1, t1.as_ref()))),
            Expr::App(t1, t2) => Expr::App(
                Box::new(shift(d, c, t1.as_ref())),
                Box::new(shift(d, c, t2.as_ref())),
            ),
        }
    }

    fn subst<'a>(j: i64, s: Expr<'a>, term: &Expr<'a>) -> Expr<'a> {
        match term {
            Expr::Var(_, k) if *k == j => s,
            Expr::Var(bndr, k) => Expr::Var(bndr, *k),
            Expr::Abs(bndr, t1) => Expr::Abs(bndr, Box::new(subst(j + 1, shift(1, 0, &s), t1))),
            Expr::App(t1, t2) => Expr::App(
                Box::new(subst(j, s.clone(), t1.as_ref())),
                Box::new(subst(j, s, t2.as_ref())),
            ),
        }
    }

    pub fn subst_top<'a>(s: Expr<'a>, t: Expr<'a>) -> Expr<'a> {
        shift(-1, 0, &subst(0, shift(1, 0, &s), &t))
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
                    t2.as_ref().clone(),
                    t12.as_ref().clone(),
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

pub fn true_t<'a>() -> ast::Expr<'a> {
    ast::abs("p", ast::abs("q", ast::var("p", 1)))
}

pub fn false_t<'a>() -> ast::Expr<'a> {
    ast::abs("p", ast::abs("q", ast::var("q", 0)))
}

pub fn not_t<'a>() -> ast::Expr<'a> {
    ast::abs(
        "z",
        ast::app(ast::app(ast::var("z", 0), false_t()), true_t()),
    )
}

pub fn app<'a>(t1: ast::Expr<'a>, t2: ast::Expr<'a>) -> ast::Expr<'a> {
    ast::app(t1, t2)
}

pub fn eval(term: ast::Expr) -> ast::Expr {
    evaluator::multi_step_eval(term)
}
