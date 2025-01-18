#![feature(once_wait)]
#![feature(exitcode_exit_method)]

use ariadne::*;

use cek::Cek;
use dynamics::*;

mod cek;
mod dynamics;
mod parser;
mod types;

#[derive(Default)]
pub struct StaticError {
    pub span: std::ops::Range<usize>,
    pub error: String,
    pub help: Option<String>,
    pub note: Option<String>,
}

fn unwrap_static<T>(result: Result<T, Vec<StaticError>>, file: &str, src: &str) -> T {
    result.unwrap_or_else(|errs| {
        let mut colours = ColorGenerator::default();
        let mut display = Report::build(ReportKind::Error, (&file, 0..0));
        for err in errs {
            display.add_label(
                Label::new((&file, err.span))
                    .with_message(err.error)
                    .with_color(colours.next()),
            );
            if let Some(help) = err.help {
                display.set_help(help);
            }
            if let Some(note) = err.note {
                display.set_note(note);
            }
        }
        display
            .finish()
            .eprint((&file, Source::from(&src)))
            .unwrap();
        std::process::ExitCode::from(1).exit_process()
    })
}

fn main() {
    let file = std::env::args().nth(1).expect("file expected as first arg");
    let src = std::fs::read_to_string(&file).unwrap();

    let ast_static = unwrap_static(parser::generate_static_ast(&src), &file, &src);
    let ast_dynamic = unwrap_static(types::type_check(ast_static), &file, &src);

    rayon::ThreadPoolBuilder::new().build_global().unwrap();

    let mut state = Cek::new(ast_dynamic);
    while state.finish().is_none() {
        state = state.step();
    }
    println!("{}", state.finish().unwrap());
    println!("{}", state.env);
}
