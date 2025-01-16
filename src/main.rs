#![feature(once_wait)]

use cek::CEK;
use dynamics::*;

mod cek;
mod dynamics;

fn main() {
    //  let a =
    //      let box c = box 2 in (\x -> c * 4)
    //  in
    //  let b =
    //      let c = 4 in (\x -> c)
    //  in
    //  a (b 6)

    rayon::ThreadPoolBuilder::new().build_global().unwrap();

    let mut state = CEK::new(Term::LetBinding(LetBinding {
        binding: String::from("a"),
        expr: Term::LetBoxBinding(LetBinding {
            binding: String::from("c"),
            expr: Term::Box(Bx {
                body: Term::IntLiteral(2).into(),
            })
            .into(),
            body: Term::Function(Func {
                binding: String::from("x"),
                body: Term::BinaryPrimitive(BinaryPrimitive {
                    op: BinaryOp::Multiply,
                    lhs: Term::GlobalVariable(String::from("c")).into(),
                    rhs: Term::IntLiteral(4).into(),
                }).into(),
            })
            .into(),
        })
        .into(),
        body: Term::LetBinding(LetBinding {
            binding: String::from("b"),
            expr: Term::LetBinding(LetBinding {
                binding: String::from("c"),
                expr: Term::IntLiteral(4).into(),
                body: Term::Function(Func {
                    binding: String::from("x"),
                    body: Term::LocalVariable(String::from("c")).into(),
                })
                .into(),
            })
            .into(),
            body: Term::Application(Application {
                func: Term::LocalVariable(String::from("a")).into(),
                arg: Term::Application(Application {
                    func: Term::LocalVariable(String::from("b")).into(),
                    arg: Term::IntLiteral(6).into(),
                })
                .into(),
            })
            .into(),
        })
        .into(),
    }));
    println!("{state:#?}");
    while state.finish().is_none() {
        state = state.step();
        println!("{state:#?}");
    }
}
