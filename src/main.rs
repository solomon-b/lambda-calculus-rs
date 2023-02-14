use simplytyped::ast::Expr::*;
use simplytyped::ast::macros::app;
use simplytyped::{not_t, true_t, eval};

pub mod simplytyped;

fn main() {
    let expr = app!(not_t(), true_t());

    println!("{}", eval(expr))
}
