extern crate syntax;

use self::syntax::ast;
use self::syntax::codemap::{CodeMap, Span, Loc, FilePathMapping};
use self::syntax::parse::ParseSess;

#[repr(C)]
pub enum RSNodeKind {
    Crate,
    StructDecl,
    EnumDecl,
    TraitDecl,
    ImplDecl,
    TypeAliasDecl,
    FieldDecl,
    EnumVariantDecl,
    FunctionDecl,
    ParmDecl,
    VarDecl,
    Unexposed
}

#[repr(C)]
pub enum RSVisitResult {
    Break,
    Continue,
    Recurse
}

#[repr(C)]
pub struct RSLocation {
    line: i32,
    column: i32
}

#[repr(C)]
pub struct RSRange {
    start: RSLocation,
    end: RSLocation
}

impl RSLocation {
    pub fn invalid() -> RSLocation {
        RSLocation {
            line: -1,
            column: -1
        }
    }

    pub fn from_loc(loc: &Loc) -> RSLocation {
        RSLocation {
            line: loc.line as i32,
            column: loc.col.0 as i32
        }
    }
}

impl RSRange {
    pub fn invalid() -> RSRange {
        RSRange {
            start: RSLocation::invalid(),
            end: RSLocation::invalid()
        }
    }

    pub fn from_span(span: &Span, codemap: &CodeMap) -> RSRange {
        RSRange {
            start: RSLocation::from_loc(&codemap.lookup_char_pos(span.lo)),
            end: RSLocation::from_loc(&codemap.lookup_char_pos(span.hi))
        }
    }

    pub fn at_span_start(span: &Span, codemap: &CodeMap) -> RSRange {
        let start = codemap.lookup_char_pos(span.lo);
        RSRange {
            start: RSLocation::from_loc(&start),
            end: RSLocation::from_loc(&start)
        }
    }
}


pub struct RSNode<'a> {
    data: RSASTItem<'a>,
    krate: &'a RSCrate,
}

impl<'a> RSNode<'a> {
    pub fn from_crate(krate: &'a RSCrate) -> RSNode<'a> {
        RSNode {
            data: RSASTItem::Crate(krate.get_ast()),
            krate: krate
        }
    }
    pub fn new(data: RSASTItem<'a>, krate: &'a RSCrate) -> RSNode<'a> {
        RSNode {
            data: data,
            krate: krate
        }
    }

    pub fn get_crate(&self) -> &RSCrate {
        self.krate
    }

    pub fn get_ast_item(&self) -> &RSASTItem<'a> {
        &self.data
    }
}

pub enum RSASTItem<'ast> {
    Crate(&'ast ast::Crate),
    Item(&'ast ast::Item),
    Stmt(&'ast ast::Stmt),
    Expr(&'ast ast::Expr),
    Variant(&'ast ast::Variant, &'ast ast::Generics, ast::NodeId),
    Field(&'ast ast::StructField),
}

pub struct RSCrate {
    ast: ast::Crate,
}

impl RSCrate {
    pub fn new(ast: ast::Crate) -> RSCrate {
        RSCrate {
            ast: ast,
        }
    }

    pub fn get_ast(&self) -> &ast::Crate {
        &self.ast
    }
}

pub struct RSIndex {
    parse_sess: ParseSess
}

impl RSIndex {
    pub fn new(parse_sess: ParseSess) -> RSIndex {
        RSIndex {
            parse_sess: parse_sess
        }
    }

    pub fn get_parse_sess(&self) -> &ParseSess {
        &self.parse_sess
    }
}

