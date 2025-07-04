#![feature(exitcode_exit_method)]
#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(lazy_get)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::{path::PathBuf, time::Instant};

use ariadne::*;

use cek::Cek;
use dynamics::*;

mod cek;
mod dynamics;
mod parser;
mod types;

#[derive(Default, Debug)]
pub struct StaticError {
    pub span: std::ops::Range<usize>,
    pub error: String,
    pub help: Option<String>,
    pub note: Option<String>,
}

impl StaticError {
    fn new(span: impl Into<std::ops::Range<usize>>, error: impl ToString) -> Self {
        Self {
            span: span.into(),
            error: error.to_string(),
            help: None,
            note: None,
        }
    }

    #[allow(dead_code)]
    fn with_help(mut self, msg: String) -> Self {
        self.help = Some(msg);
        self
    }

    #[allow(dead_code)]
    fn with_note(mut self, msg: String) -> Self {
        self.note = Some(msg);
        self
    }
}

impl<T, I: FromIterator<StaticError>> From<StaticError> for Result<T, I> {
    fn from(value: StaticError) -> Self {
        Err(I::from_iter(std::iter::once(value)))
    }
}

fn unwrap_static<T>(
    result: Result<T, Vec<StaticError>>,
    file: impl AsRef<std::path::Path>,
    src: &str,
) -> T {
    let file = file.as_ref().to_string_lossy();
    result.unwrap_or_else(|errs| {
        let mut colours = ColorGenerator::default();
        let mut display = Report::build(
            ReportKind::Error,
            (&file, errs.first().unwrap().span.clone()),
        );
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
    // println!("{:?}", thread::available_parallelism());

    let config = bincode::config::standard()
        .with_big_endian()
        .with_variable_int_encoding();

    let mut args = std::env::args().skip(1);
    let ast_dynamic = match &*args.next().unwrap() {
        "--file" => {
            let file = PathBuf::from(args.next().expect("file expected as first arg"));
            let src = std::fs::read_to_string(&file).unwrap();

            let c1 = Instant::now();

            let ast_static = unwrap_static(parser::generate_static_ast(&src), &file, &src);

            let c2 = Instant::now();
            println!("Parsing done in {:?}", c2 - c1);

            let ast_dynamic = unwrap_static(types::type_check(ast_static), &file, &src);

            let c3 = Instant::now();
            println!("Static pass done in {:?}", c3 - c2);

            bincode::serde::encode_into_std_write(
                &ast_dynamic,
                &mut std::fs::File::create(args.next().map(PathBuf::from).unwrap_or_else(|| {
                    let mut file = file;
                    file.set_extension("ir");
                    file
                }))
                .unwrap(),
                config,
            )
            .unwrap();

            ast_dynamic
        }
        "--ir" => {
            let file = args.next().expect("file expected as first arg");

            bincode::serde::decode_from_std_read(&mut std::fs::File::open(file).unwrap(), config)
                .unwrap()
        }
        _ => todo!(),
    };

    let c3 = Instant::now();

    let mut state = Cek::new(ast_dynamic);
    rayon::scope(|_| {
        while state.finish().is_none() {
            state = state.step();
        }

        println!("{}", state.finish().unwrap());
        // println!("{}", state.env);
    });

    let c4 = Instant::now();

    println!("Execution done in {:?}", c4 - c3);
}
