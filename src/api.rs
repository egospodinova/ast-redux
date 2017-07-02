extern crate libc;
extern crate syntax;

use std::ptr;
use std::ffi::{CStr, CString};
use self::syntax::ast::*;
use self::syntax::codemap::{CodeMap, FilePathMapping, Loc, Span};
use self::syntax::errors::DiagnosticBuilder;
use self::syntax::parse::{self, ParseSess};
use self::syntax::parse::parser::Parser;
use self::syntax::parse::lexer;
use self::syntax::print::pprust;
use self::syntax::visit::{self, Visitor, FnKind};

use types::*;

type CallbackFn = extern fn(*const RSNode, *const RSNode, *mut libc::c_void) -> RSVisitResult;
type ClientData = *mut libc::c_void;

#[no_mangle]
pub unsafe extern fn visit_children(node: *mut RSNode, callback: CallbackFn, data: ClientData) {
    if node.is_null() {
        return;
    }

    let mut visitor = ApiVisitor::new((*node).get_crate(), callback, data);
    visitor.walk(node);
}

#[no_mangle]
pub unsafe extern fn node_get_crate(node: *const RSNode) -> *const RSCrate {
    (*node).get_crate()
}

#[no_mangle]
pub unsafe extern fn create_index() -> *mut RSIndex {
    let parse_sess = ParseSess::new(FilePathMapping::empty());
    Box::into_raw(Box::new(RSIndex::new(parse_sess)))
}

#[no_mangle]
pub unsafe extern fn destroy_index(index: *mut RSIndex) {
    if !index.is_null() {
        let index = Box::from_raw(index);
    }
}

fn parse_source(name: String, source: String, parse_sess: &ParseSess) -> Result<Crate, DiagnosticBuilder> {
    let filemap = parse_sess.codemap().new_filemap(name, source);
    let mut lexer = lexer::StringReader::new(&parse_sess, filemap);
    lexer.real_token();
    let stream = try!(lexer.parse_all_token_trees());
    Parser::new(parse_sess, stream, None, false, false).parse_crate_mod()
}

#[no_mangle]
pub unsafe extern fn parse_crate(name: *mut libc::c_char, src: *mut libc::c_char,
                                 index: *mut RSIndex) -> *const RSCrate {
    if index.is_null() {
        return ptr::null();
    }

    let index = &*index;
    let name = if !name.is_null() { CStr::from_ptr(name).to_str().unwrap_or("") } else { "" };

    if let Ok(source_str) = CStr::from_ptr(src).to_str() {
        match parse_source(name.to_owned(), source_str.to_owned(), index.get_parse_sess()) {
            Ok(krate) => {
                if index.get_parse_sess().span_diagnostic.has_errors() {
                    // TODO: expose errors
                }
                let rs_crate = Box::new(RSCrate::new(krate));
                return Box::into_raw(rs_crate);
            },
            Err(mut diag) => {
                // TODO: expose errors
                diag.emit();
            }
        }
    }

    ptr::null()
}

#[no_mangle]
pub unsafe extern fn destroy_crate(krate: *mut RSCrate) {
    if !krate.is_null() {
        let krate = Box::from_raw(krate);
    }
}

#[no_mangle]
pub unsafe extern fn node_from_crate<'a>(krate: *const RSCrate) -> *const RSNode<'a> {
    if krate.is_null() { return ptr::null(); }
    Box::into_raw(Box::new(RSNode::from_crate(&*krate)))
}

#[no_mangle]
pub unsafe extern fn destroy_node(node: *mut RSNode) {
    if !node.is_null() {
        let node = Box::from_raw(node);
    }
}

#[no_mangle]
pub unsafe extern fn node_get_spelling_name(node: *const RSNode) -> *const libc::c_char {
    match *(*node).get_ast_item() {
        RSASTItem::Item(i) => match i.node {
            ItemKind::Impl(_, _, _, _, _, ref ty, _) => CString::new(pprust::ty_to_string(&*ty)).unwrap().into_raw(),
            _ => CString::new(format!("{}", i.ident.name)).unwrap().into_raw(),
        },
        RSASTItem::Variant(v, _, _) => CString::new(format!("{}", v.node.name)).unwrap().into_raw(),
        RSASTItem::Field(f) => if let Some(id) = f.ident {
            CString::new(format!("{}", id)).unwrap().into_raw()
        } else { ptr::null() },
        RSASTItem::Pat(p) => match p.node {
            PatKind::Ident(_, id, _) => CString::new(format!("{}", id.node.name)).unwrap().into_raw(),
            _ => ptr::null()
        },
        RSASTItem::TraitItem(t) => CString::new(format!("{}", t.ident.name)).unwrap().into_raw(),
        RSASTItem::ImplItem(i) => CString::new(format!("{}", i.ident.name)).unwrap().into_raw(),
        _ => ptr::null()
    }
}

#[no_mangle]
pub unsafe extern fn node_get_spelling_range(node: *const RSNode, index: *const RSIndex) -> RSRange {
    if index.is_null() {
        return RSRange::invalid();
    }

    let codemap = (*index).get_parse_sess().codemap();
    match *(*node).get_ast_item() {
        RSASTItem::Item(i) => RSRange::at_span_start(&i.span, codemap),
        RSASTItem::Variant(v, _, _) => RSRange::at_span_start(&v.span, codemap),
        RSASTItem::Field(f) => RSRange::at_span_start(&f.span, codemap),
        RSASTItem::Pat(p) => match p.node {
            PatKind::Ident(_, id, _) => RSRange::from_span(&id.span, codemap),
            _ => RSRange::invalid()
        },
        RSASTItem::TraitItem(t) => RSRange::at_span_start(&t.span, codemap),
        RSASTItem::ImplItem(i) => RSRange::at_span_start(&i.span, codemap),
        _ => RSRange::invalid()
    }
}

#[no_mangle]
pub unsafe extern fn node_get_extent(node: *const RSNode, index: *const RSIndex) -> RSRange {
    if index.is_null() {
        return RSRange::invalid();
    }

    let codemap = (*index).get_parse_sess().codemap();
    match *(*node).get_ast_item() {
        RSASTItem::Item(i) => RSRange::from_span(&i.span, codemap),
        RSASTItem::Variant(v, _, _) => RSRange::from_span(&v.span, codemap),
        RSASTItem::Field(f) => RSRange::from_span(&f.span, codemap),
        RSASTItem::Pat(p) => match p.node {
            PatKind::Ident(_, id, _) => RSRange::from_span(&id.span, codemap),
            _ => RSRange::invalid()
        },
        RSASTItem::TraitItem(t) => RSRange::from_span(&t.span, codemap),
        RSASTItem::ImplItem(i) => RSRange::from_span(&i.span, codemap),
        _ => RSRange::invalid()
    }
}

#[no_mangle]
pub unsafe extern fn node_get_kind(node: *const RSNode) -> RSNodeKind {
    match *(*node).get_ast_item() {
        RSASTItem::Crate(_) => RSNodeKind::Crate,
        RSASTItem::Item(i) => match i.node {
            ItemKind::Struct(..)        => RSNodeKind::StructDecl,
            ItemKind::Enum(..)          => RSNodeKind::EnumDecl,
            ItemKind::Trait(..)         => RSNodeKind::TraitDecl,
            ItemKind::Impl(..) |
            ItemKind::DefaultImpl(..)   => RSNodeKind::ImplDecl,
            ItemKind::Fn(..)            => RSNodeKind::FunctionDecl,
            ItemKind::Ty(..)            => RSNodeKind::TypeAliasDecl,
            _   => RSNodeKind::Unexposed
        },
        RSASTItem::Pat(p) => match p.node {
            PatKind::Ident(..) => RSNodeKind::VarDecl,
            _ => RSNodeKind::Unexposed
        },
        RSASTItem::Stmt(_) => RSNodeKind::Unexposed,
        RSASTItem::Expr(_) => RSNodeKind::Unexposed,
        RSASTItem::Variant(v, _, _) => RSNodeKind::EnumVariantDecl,
        RSASTItem::Field(f) => RSNodeKind::FieldDecl,
        RSASTItem::TraitItem(t) => match t.node {
            TraitItemKind::Method(..) => RSNodeKind::FunctionDecl,
            _ => RSNodeKind::Unexposed
        },
        RSASTItem::ImplItem(i) => match i.node {
            ImplItemKind::Method(..) => RSNodeKind::FunctionDecl,
            _ => RSNodeKind::Unexposed
        },
        _       => RSNodeKind::Unexposed
    }
}

struct ApiVisitor<'ast> {
    callback: CallbackFn,
    data: ClientData,
    parents: Vec<*const RSNode<'ast>>,
    krate: &'ast RSCrate,
    should_stop: bool
}

impl<'ast> ApiVisitor<'ast> {
    fn new(krate: &'ast RSCrate, callback: CallbackFn, data: ClientData) -> ApiVisitor {
        ApiVisitor {
            callback: callback,
            data: data,
            parents: vec![],
            krate: krate,
            should_stop: false
        }
    }

    fn walk(&mut self, node: *const RSNode<'ast>) {
        let result = {
            if let Some(parent) = self.parents.last() {
                (self.callback)(node, *parent, self.data)
            } else if let &RSASTItem::Crate(c) = unsafe { (*node).get_ast_item() } {
                (self.callback)(node, ptr::null(), self.data)
            } else {
                RSVisitResult::Recurse
            }
        };

        match result {
            RSVisitResult::Continue => (),
            RSVisitResult::Break => { self.should_stop = true },
            RSVisitResult::Recurse => {
                self.parents.push(node);
                match unsafe { (*node).get_ast_item() } {
                    &RSASTItem::Crate(c) => visit::walk_crate(self, c),
                    &RSASTItem::Item(i) => visit::walk_item(self, i),
                    &RSASTItem::Stmt(s) => visit::walk_stmt(self, s),
                    &RSASTItem::Expr(e) => visit::walk_expr(self, e),
                    &RSASTItem::Variant(v, g, id) => visit::walk_variant(self, v, g, id),
                    &RSASTItem::Field(f) => visit::walk_struct_field(self, f),
                    &RSASTItem::Pat(p) => visit::walk_pat(self, p),
                    &RSASTItem::TraitItem(t) => visit::walk_trait_item(self, t),
                    &RSASTItem::ImplItem(i) => visit::walk_impl_item(self, i),
                }
                self.parents.pop().unwrap();
            }
        }
    }
}

impl<'ast> Visitor<'ast> for ApiVisitor<'ast> {
    fn visit_name(&mut self, _span: Span, _name: Name) {
        // Nothing to do.
    }
    fn visit_ident(&mut self, span: Span, ident: Ident) {
        visit::walk_ident(self, span, ident);
    }
    fn visit_mod(&mut self, m: &'ast Mod, _s: Span, _attrs: &[Attribute], _n: NodeId) {
        visit::walk_mod(self, m);
    }
    fn visit_foreign_item(&mut self, i: &'ast ForeignItem) { visit::walk_foreign_item(self, i) }
    fn visit_global_asm(&mut self, ga: &'ast GlobalAsm) { visit::walk_global_asm(self, ga) }
    fn visit_item(&mut self, i: &'ast Item) {
        let item = Box::new(RSNode::new(RSASTItem::Item(i), self.krate));
        self.walk(item.as_ref());
    }
    fn visit_local(&mut self, l: &'ast Local) { visit::walk_local(self, l) }
    fn visit_block(&mut self, b: &'ast Block) { visit::walk_block(self, b) }
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        let stmt = Box::new(RSNode::new(RSASTItem::Stmt(s), self.krate));
        self.walk(stmt.as_ref());
    }
    fn visit_arm(&mut self, a: &'ast Arm) { visit::walk_arm(self, a) }
    fn visit_pat(&mut self, p: &'ast Pat) {
        let pat = Box::new(RSNode::new(RSASTItem::Pat(p), self.krate));
        self.walk(pat.as_ref());
    }
    fn visit_expr(&mut self, ex: &'ast Expr) {
        let expr = Box::new(RSNode::new(RSASTItem::Expr(ex), self.krate));
        self.walk(expr.as_ref());
    }
    fn visit_expr_post(&mut self, _ex: &'ast Expr) {
    }
    fn visit_ty(&mut self, t: &'ast Ty) {
        visit::walk_ty(self, t)
    }
    fn visit_generics(&mut self, g: &'ast Generics) { visit::walk_generics(self, g) }
    fn visit_where_predicate(&mut self, p: &'ast WherePredicate) {
        visit::walk_where_predicate(self, p)
    }
    fn visit_fn(&mut self, fk: FnKind<'ast>, fd: &'ast FnDecl, s: Span, _: NodeId) {
        visit::walk_fn(self, fk, fd, s)
    }
    fn visit_trait_item(&mut self, ti: &'ast TraitItem) {
        let trait_item = Box::new(RSNode::new(RSASTItem::TraitItem(ti), self.krate));
        self.walk(trait_item.as_ref());
    }
    fn visit_impl_item(&mut self, ii: &'ast ImplItem) {
        let impl_item = Box::new(RSNode::new(RSASTItem::ImplItem(ii), self.krate));
        self.walk(impl_item.as_ref());
    }
    fn visit_trait_ref(&mut self, t: &'ast TraitRef) { visit::walk_trait_ref(self, t) }
    fn visit_ty_param_bound(&mut self, bounds: &'ast TyParamBound) {
        visit::walk_ty_param_bound(self, bounds)
    }
    fn visit_poly_trait_ref(&mut self, t: &'ast PolyTraitRef, m: &'ast TraitBoundModifier) {
        visit::walk_poly_trait_ref(self, t, m)
    }
    fn visit_variant_data(&mut self, s: &'ast VariantData, _: Ident,
                          _: &'ast Generics, _: NodeId, _: Span) {
        visit::walk_struct_def(self, s)
    }
    fn visit_struct_field(&mut self, s: &'ast StructField) {
        let field = Box::new(RSNode::new(RSASTItem::Field(s), self.krate));
        self.walk(field.as_ref());
    }
    fn visit_enum_def(&mut self, enum_definition: &'ast EnumDef,
                      generics: &'ast Generics, item_id: NodeId, _: Span) {
        visit::walk_enum_def(self, enum_definition, generics, item_id)
    }
    fn visit_variant(&mut self, v: &'ast Variant, g: &'ast Generics, id: NodeId) {
        let var = Box::new(RSNode::new(RSASTItem::Variant(v, g, id), self.krate));
        self.walk(var.as_ref());
    }
    fn visit_lifetime(&mut self, lifetime: &'ast Lifetime) {
        visit::walk_lifetime(self, lifetime)
    }
    fn visit_lifetime_def(&mut self, lifetime: &'ast LifetimeDef) {
        visit::walk_lifetime_def(self, lifetime)
    }
    fn visit_mac(&mut self, mac: &'ast Mac) {
        visit::walk_mac(self, mac)
    }
    fn visit_mac_def(&mut self, mac: &'ast MacroDef, _id: NodeId) {
        // Nothing to do
    }
    fn visit_path(&mut self, path: &'ast Path, _id: NodeId) {
        visit::walk_path(self, path)
    }
    fn visit_path_list_item(&mut self, prefix: &'ast Path, item: &'ast PathListItem) {
        visit::walk_path_list_item(self, prefix, item)
    }
    fn visit_path_segment(&mut self, path_span: Span, path_segment: &'ast PathSegment) {
        visit::walk_path_segment(self, path_span, path_segment)
    }
    fn visit_path_parameters(&mut self, path_span: Span, path_parameters: &'ast PathParameters) {
        visit::walk_path_parameters(self, path_span, path_parameters)
    }
    fn visit_assoc_type_binding(&mut self, type_binding: &'ast TypeBinding) {
        visit::walk_assoc_type_binding(self, type_binding)
    }
    fn visit_attribute(&mut self, _attr: &'ast Attribute) {}
    fn visit_vis(&mut self, vis: &'ast Visibility) {
        visit::walk_vis(self, vis)
    }
    fn visit_fn_ret_ty(&mut self, ret_ty: &'ast FunctionRetTy) {
        visit::walk_fn_ret_ty(self, ret_ty)
    }
}

