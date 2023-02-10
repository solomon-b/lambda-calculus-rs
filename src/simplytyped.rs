pub mod ast {
    #[derive(Clone, Debug)]
    pub enum Expr {
        Var(String),
        Abs(String, Box<Expr>),
        App(Box<Expr>, Box<Expr>),
    }

    pub fn var(var: &str) -> Expr {
        Expr::Var(var.to_string())
    }
    pub fn abs(bndr: &str, body: Expr) -> Expr {
        Expr::Abs(bndr.to_string(), Box::new(body))
    }

    pub fn app(t1: Expr, t2: Expr) -> Expr {
        Expr::App(Box::new(t1), Box::new(t2))
    }
}

mod substitution {
    use std::collections::HashSet;

    use super::ast::Expr;

    fn free_vars(term: &Expr) -> HashSet<String> {
        match term {
            Expr::Var(x) => {
                let mut var: HashSet<String> = HashSet::new();
                var.insert(x.clone());
                var
            }
            Expr::Abs(bndr, t1) => {
                let mut vars = free_vars(t1.as_ref());
                vars.remove(bndr);
                vars
            }
            Expr::App(t1, t2) => {
                let mut v1 = free_vars(t1.as_ref());
                let v2 = free_vars(t2.as_ref());
                v1.extend(v2);
                v1
            }
        }
    }

    pub fn subst(x: &String, s: &Expr, term: &Expr) -> Expr {
        match term {
            Expr::Var(y) if x == y => s.clone(),
            Expr::Var(y) => Expr::Var(y.clone()),
            Expr::Abs(y, t1) if x != y && !free_vars(s).contains(y) => {
                Expr::Abs(y.clone(), Box::new(subst(x, s, t1.as_ref())))
            }
            Expr::Abs(_, _) => {
                panic!("oops name collision!")
            }
            Expr::App(t1, t2) => Expr::App(
                Box::new(subst(x, s, t1.as_ref())),
                Box::new(subst(x, s, t2.as_ref())),
            ),
        }
    }
}

mod evaluator {
    use super::{ast::Expr, substitution};

    fn is_val(term: &Expr) -> bool {
        match term {
            Expr::Var(_) => false,
            Expr::Abs(_, _) => true,
            Expr::App(_, _) => false,
        }
    }

    fn single_eval(term: Expr) -> Option<Expr> {
        match term {
            Expr::App(t1, t2) => match *t1 {
                Expr::Abs(x, t12) if is_val(t2.as_ref()) => {
                    Some(substitution::subst(&x, t2.as_ref(), t12.as_ref()))
                }

                v1 @ Expr::Abs(_, _) => {
                    single_eval(*t2).map(|t2| Expr::App(Box::new(v1), Box::new(t2)))
                }

                _ => single_eval(*t1).map(|t1| Expr::App(Box::new(t1), t2)),
            },
            _ => None,
        }
    }

    pub fn multi_step_eval(term: Expr) -> Expr {
        match single_eval(term.clone()) {
            Some(val) => multi_step_eval(val),
            None => term,
        }
    }
}

pub fn true_t() -> ast::Expr {
    ast::abs("p", ast::abs("q", ast::var("p")))
}

pub fn false_t() -> ast::Expr {
    ast::abs("p", ast::abs("q", ast::var("q")))
}

pub fn not_t() -> ast::Expr {
    ast::abs(
        "z",
        ast::app(
            ast::app(ast::var("z"), ast::abs("a", ast::abs("b", ast::var("b")))),
            ast::abs("c", ast::abs("d", ast::var("c"))),
        ),
    )
}

pub fn app(t1: ast::Expr, t2: ast::Expr) -> ast::Expr {
    ast::app(t1, t2)
}

pub fn eval(term: ast::Expr) -> ast::Expr {
    evaluator::multi_step_eval(term)
}
