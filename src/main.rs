use simplytyped::ast::Expr::*;
use simplytyped::{not_t, true_t, eval};

pub mod simplytyped;

fn main() {
    let expr = App(Box::new(not_t()), Box::new(true_t()));

    println!("{:?}", eval(expr))
}
