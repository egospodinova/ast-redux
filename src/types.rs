extern crate syntax;

use self::syntax::ast;
use self::syntax::codemap::{CodeMap, FilePathMapping};
use self::syntax::parse::ParseSess;

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

