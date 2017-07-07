use types;

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
    Crate(&'ast types::Crate),
    Item(&'ast types::Item),
    Stmt(&'ast types::Stmt),
    Expr(&'ast types::Expr),
    //Variant(&'ast types::Variant, &'ast types::Generics),
    Field(&'ast types::StructField),
    Pat(&'ast types::Pat),
    TraitItem(&'ast types::TraitItem),
    ImplItem(&'ast types::ImplItem),
}

pub struct RSCrate {
    ast: types::Crate,
}

impl RSCrate {
    pub fn new(ast: types::Crate) -> RSCrate {
        RSCrate {
            ast: ast,
        }
    }

    pub fn get_ast(&self) -> &types::Crate {
        &self.ast
    }
}

