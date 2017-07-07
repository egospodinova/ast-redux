mod transform;

use std::io;
use std::thread;

use syntax::codemap::FilePathMapping;
use syntax::parse::ParseSess;
use syntax::parse::parser::Parser;
use syntax::parse::lexer::StringReader;

use parser::transform::ASTTransformer;
use types::Crate;

/*
use rustc::session::Session;
use rustc::session::config::{self, Input, ErrorOutputType};
use rustc_driver::{driver, CompilerCalls, Compilation, RustcDefaultCalls};

struct ParserCalls {
    default_calls: RustcDefaultCalls
}

impl ParserCalls {
    pub fn new() -> ParserCalls {
        ParserCalls {
            default_calls: RustcDefaultCalls
        }
    }
}

impl<'a> CompilerCalls<'a> for ParserCalls {
    fn build_controller(&mut self, _: &Session,  _: &getopts::Matches) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();
        control.after_parse.stop = Compilation::Stop;
        control.after_parse.callback = box |state: &mut driver::CompileState| {
            let krate = state.krate.as_ref();
            let mut transformer = ASTTransformer::new();
            transformer.transform_crate(krate);
        };

        control
    }
}
*/

macro_rules! maybe {
    ($e:expr) => (match $e { Ok(value) => value, Err(_) => return None })
}

fn run_parser<F, R>(f: F) -> Option<R>
    where F: FnOnce() -> R + Send + 'static,
          R: Send + 'static {
    let parse_thread = thread::Builder::new().name("parse".to_owned());
    let join_handle = parse_thread.spawn(move || {
        io::set_panic(None);
        f()
    });

    join_handle.unwrap().join().ok()
}

pub fn parse_source(name: String, source: String) -> Option<Crate> {
    run_parser(move || -> Option<Crate> {
        let parse_sess = ParseSess::new(FilePathMapping::empty());
        let filemap = parse_sess.codemap().new_filemap(name, source);
        let mut lexer = StringReader::new(&parse_sess, filemap);
        lexer.real_token();
        let stream = maybe!(lexer.parse_all_token_trees());
        let krate = maybe!(Parser::new(&parse_sess, stream, None, false, false).parse_crate_mod());

        let mut transformer = ASTTransformer::new(false, parse_sess.codemap());
        Some(transformer.transform_crate(&krate))
    }).and_then(|k| k)
}

