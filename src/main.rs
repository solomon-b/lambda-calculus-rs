pub mod simplytyped;

fn main() {
    let expr = simplytyped::app(simplytyped::not_t(), simplytyped::true_t());
    
    println!("{:?}", simplytyped::eval(expr))
}

