// Copyright 2017  Emma Gospodinova <emma.gospodinova@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use types::{self, Named};
use diagnostics::Diagnostic;

#[repr(C)]
pub enum RSNodeKind {
    Crate,
    Module,
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
    Path,
    PathSegment,
    Block,
    Arm,
    Unexposed
}

#[repr(C)]
pub enum RSTypeKind {
    Never,
    Tuple,
    Array,
    Slice,
    Ptr,
    Ref,
    Function,
    Path,
    Err
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

    pub fn from_loc(loc: &types::Location) -> RSLocation {
        RSLocation {
            line: loc.line,
            column: loc.column
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

    pub fn from_span(span: &types::Span) -> RSRange {
        RSRange {
            start: RSLocation::from_loc(&span.start),
            end: RSLocation::from_loc(&span.end)
        }
    }

    pub fn at_span_start(span: &types::Span) -> RSRange {
        let start = &span.start;
        RSRange {
            start: RSLocation::from_loc(start),
            end: RSLocation::from_loc(start)
        }
    }
}


pub struct RSNode<'a> {
    data: RSASTItem<'a>,
    krate: &'a RSCrate,
    id: u32,
    last_child_id: u32
}

impl<'a> RSNode<'a> {
    pub fn new(data: RSASTItem<'a>, krate: &'a RSCrate, id: u32) -> RSNode<'a> {
        RSNode {
            data: data,
            krate: krate,
            id: id,
            last_child_id: id
        }
    }

    pub fn get_id(&self) -> u32 {
        self.id
    }

    pub fn get_crate(&self) -> &RSCrate {
        self.krate
    }

    pub fn get_ast_item(&self) -> &RSASTItem<'a> {
        &self.data
    }

    pub fn get_last_child_id(&self) -> u32 {
        self.last_child_id
    }

    pub fn set_last_child_id(&mut self, id: u32) {
        self.last_child_id = id;
    }
}

pub enum RSASTItem<'ast> {
    Crate(&'ast types::Crate),
    Item(&'ast types::Item),
    Variant(&'ast types::EnumVariant, &'ast types::Generics),
    Field(&'ast types::StructField),
    Pat(&'ast types::Pat),
    TraitItem(&'ast types::TraitItem),
    ImplItem(&'ast types::ImplItem),
    Path(&'ast types::Path),
    PathSegment(&'ast types::PathSegment),
    Block(&'ast types::Block),
    Arm(&'ast types::Arm),
}

impl<'ast> RSASTItem<'ast> {
    pub fn name(&self) -> Option<types::Identifier> {
        use types::{Pat_, Item_, Type_};
        match *self {
            RSASTItem::Item(i) => match i.node {
                Item_::Impl(_, _, _, ref ty, _) => match ty.node {
                    Type_::Path(ref p)  => Some(p.name()),
                    _                   => Some(i.name())
                },
                _                       => Some(i.name()),
            },
            RSASTItem::Variant(v, _)    => Some(v.name()),
            RSASTItem::Field(f) => match f.ident {
                Some(ref id)            => Some(id.clone()),
                _                       => None
            },
            RSASTItem::Pat(p) => match p.node {
                Pat_::Ident(ref id, ..) => Some(id.node.clone()),
                _                       => None
            },
            RSASTItem::TraitItem(t)     => Some(t.name()),
            RSASTItem::ImplItem(i)      => Some(i.name()),
            RSASTItem::Path(p)          => Some(p.name()),
            RSASTItem::PathSegment(s)   => Some(s.name()),
            _ => None
        }
    }
    pub fn span(&self) -> Option<types::Span> {
        match *self {
            RSASTItem::Crate(ref c)         => Some(c.span),
            RSASTItem::Item(ref i)          => Some(i.span),
            RSASTItem::Variant(ref v, _)    => Some(v.span),
            RSASTItem::Field(ref f)         => Some(f.span),
            // FIXME move this out of here:
            RSASTItem::Pat(ref p)           => match p.node {
                types::Pat_::Ident(ref id, ..) => Some(id.span),
                _ => None
            },
            RSASTItem::TraitItem(ref ti)    => Some(ti.span),
            RSASTItem::ImplItem(ref ii)     => Some(ii.span),
            RSASTItem::Path(ref p)          => Some(p.span),
            RSASTItem::PathSegment(ref s)   => Some(s.span),
            RSASTItem::Block(ref b)         => Some(b.span),
            RSASTItem::Arm(ref a)           => Some(a.span),
        }
    }
}

pub struct RSCrate {
    ast: Option<types::Crate>,
    diagnostics: Vec<Diagnostic>
}

impl RSCrate {
    pub fn new(ast: Option<types::Crate>, diagnostics: Vec<Diagnostic>) -> RSCrate {
        RSCrate {
            ast: ast,
            diagnostics: diagnostics
        }
    }

    pub fn get_ast(&self) -> &Option<types::Crate> {
        &self.ast
    }

    pub fn get_diagnostics(&self) -> &Vec<Diagnostic> {
        &self.diagnostics
    }
}

