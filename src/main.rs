use cek::State;
use dynamics::*;

mod cek;
mod dynamics;

fn main() {
    //  let a =
    //      let c = 2 in (\x -> c)
    //  in
    //  let b =
    //      let c = 4 in (\x -> c)
    //  in
    //  a (b 6)
    let mut state = State::new(Term::LetBinding(LetBinding {
        binding: String::from("a"),
        expr: Term::LetBinding(LetBinding {
            binding: String::from("c"),
            expr: Term::IntLiteral(2).into(),
            body: Term::Function(Func {
                binding: String::from("x"),
                body: Term::LocalVariable(String::from("c")).into(),
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
    while !state.is_done() {
        state = state.step();
        println!("{state:#?}");
    }
}
