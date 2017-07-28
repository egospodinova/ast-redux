mod types;

use std::ptr;
use std::ffi::{CStr, CString};

use api::types::*;
use types::*;
use visit::{self, Visitor};
use parser::parse_source;

type CallbackFn = extern fn(*const RSNode, *const RSNode, *mut ::libc::c_void) -> RSVisitResult;
type ClientData = *mut ::libc::c_void;

#[no_mangle]
pub unsafe extern fn visit_children(node: *mut RSNode, callback: CallbackFn, data: ClientData) {
    if node.is_null() {
        return;
    }

    let mut visitor = ApiVisitor::new((*node).get_crate(), callback, data, (*node).get_id());
    visitor.walk(node, RSVisitResult::Recurse);
}

#[no_mangle]
pub unsafe extern fn node_get_crate(node: *const RSNode) -> *const RSCrate {
    (*node).get_crate()
}

#[no_mangle]
pub unsafe extern fn parse_crate(name: *mut ::libc::c_char, src: *mut ::libc::c_char) -> *const RSCrate {
    let name = if !name.is_null() { CStr::from_ptr(name).to_str().unwrap_or("") } else { "" };

    if let Ok(source_str) = CStr::from_ptr(src).to_str() {
        let (krate, diagnostics) = parse_source(name.to_owned(), source_str.to_owned());
        let rs_crate = Box::new(RSCrate::new(krate, diagnostics));
        return Box::into_raw(rs_crate);
    }

    ptr::null()
}

#[no_mangle]
pub unsafe extern fn node_from_crate<'a>(krate: *const RSCrate) -> *const RSNode<'a> {
    if krate.is_null() { return ptr::null(); }

    if let Some(ref ast) = *(*krate).get_ast() {
        Box::into_raw(Box::new(RSNode::new(RSASTItem::Crate(ast), &*krate, 0)))
    } else {
        ptr::null()
    }
}

#[no_mangle]
pub unsafe extern fn node_get_id(node: *const RSNode) -> u32 {
    (*node).get_id()
}

#[no_mangle]
pub unsafe extern fn node_get_spelling_name(node: *const RSNode) -> *const ::libc::c_char {
    if let Some(name) = (*node).get_ast_item().name() {
        CString::new(name).unwrap().into_raw()
    } else {
        ptr::null()
    }
}

#[no_mangle]
pub unsafe extern fn node_get_spelling_range(node: *const RSNode) -> RSRange {
    if let Some(ref span) = (*node).get_ast_item().span() {
        RSRange::at_span_start(span)
    } else {
        RSRange::invalid()
    }
}

#[no_mangle]
pub unsafe extern fn node_get_extent(node: *const RSNode) -> RSRange {
    if let Some(ref span) = (*node).get_ast_item().span() {
        RSRange::from_span(span)
    } else {
        RSRange::invalid()
    }
}

#[no_mangle]
pub unsafe extern fn node_get_kind(node: *const RSNode) -> RSNodeKind {
    // FIXME clean up
    match *(*node).get_ast_item() {
        RSASTItem::Crate(..) => RSNodeKind::Crate,
        RSASTItem::Item(i) => match i.node {
            Item_::Mod(..)           => RSNodeKind::Module,
            Item_::Struct(..)        => RSNodeKind::StructDecl,
            Item_::Enum(..)          => RSNodeKind::EnumDecl,
            Item_::Trait(..)         => RSNodeKind::TraitDecl,
            Item_::Impl(..)          => RSNodeKind::ImplDecl,
            Item_::Fn(..)            => RSNodeKind::FunctionDecl,
            Item_::TypeAlias(..)     => RSNodeKind::TypeAliasDecl,
            _   => RSNodeKind::Unexposed
        },
        RSASTItem::Pat(p) => match p.node {
            Pat_::Ident(..) => RSNodeKind::VarDecl,
            _ => RSNodeKind::Unexposed
        },
        RSASTItem::Variant(..) => RSNodeKind::EnumVariantDecl,
        RSASTItem::Field(..) => RSNodeKind::FieldDecl,
        RSASTItem::TraitItem(t) => match t.node {
            ItemMember::Method(..) => RSNodeKind::FunctionDecl,
            _ => RSNodeKind::Unexposed
        },
        RSASTItem::ImplItem(i) => match i.node {
            ItemMember::Method(..) => RSNodeKind::FunctionDecl,
            _ => RSNodeKind::Unexposed
        },
        RSASTItem::Path(..) => RSNodeKind::Path,
        RSASTItem::PathSegment(..) => RSNodeKind::PathSegment,
        _=> RSNodeKind::Unexposed
    }
}

#[no_mangle]
pub unsafe extern fn destroy_crate(krate: *mut RSCrate) {
    if !krate.is_null() {
        let krate = Box::from_raw(krate);
    }
}

#[no_mangle]
pub unsafe extern fn destroy_node(node: *mut RSNode) {
    if !node.is_null() {
        let node = Box::from_raw(node);
    }
}

#[no_mangle]
pub unsafe extern fn destroy_string(string: *mut ::libc::c_char) {
    if !string.is_null() {
        CString::from_raw(string);
    }
}

struct ApiVisitor<'ast> {
    callback: CallbackFn,
    data: ClientData,
    parents: Vec<*const RSNode<'ast>>,
    krate: &'ast RSCrate,
    should_stop: bool,
    last_id: u32
}

impl<'ast> ApiVisitor<'ast> {
    fn new(krate: &'ast RSCrate, callback: CallbackFn, data: ClientData, last_id: u32) -> ApiVisitor {
        ApiVisitor {
            callback: callback,
            data: data,
            parents: vec![],
            krate: krate,
            should_stop: false,
            last_id: last_id
        }
    }

    fn visit(&mut self, node: *const RSNode<'ast>) {
        let result = {
            if let Some(parent) = self.parents.last() {
                (self.callback)(node, *parent, self.data)
            } else if let &RSASTItem::Crate(c) = unsafe { (*node).get_ast_item() } {
                (self.callback)(node, ptr::null(), self.data)
            } else {
                RSVisitResult::Recurse
            }
        };

        self.walk(node, result);
    }

    fn walk(&mut self, node: *const RSNode<'ast>, result: RSVisitResult) {
        match result {
            RSVisitResult::Continue => (),
            RSVisitResult::Break => { self.should_stop = true },
            RSVisitResult::Recurse => {
                self.parents.push(node);
                match unsafe { (*node).get_ast_item() } {
                    &RSASTItem::Crate(c) => visit::walk_crate(self, c),
                    &RSASTItem::Item(i) => visit::walk_item(self, i),
                    &RSASTItem::Variant(v, g)  => visit::walk_variant(self, v, g),
                    &RSASTItem::Field(f) => visit::walk_struct_field(self, f),
                    &RSASTItem::Pat(p) => visit::walk_pat(self, p),
                    &RSASTItem::TraitItem(t) => visit::walk_trait_item(self, t),
                    &RSASTItem::ImplItem(i) => visit::walk_impl_item(self, i),
                    &RSASTItem::Path(p) => visit::walk_path(self, p),
                    &RSASTItem::PathSegment(s) => visit::walk_path_segment(self, s),
                }
                self.parents.pop().unwrap();
            }
        }
    }

    fn create_node(&mut self, ast_item: RSASTItem<'ast>) -> Box<RSNode<'ast>> {
        self.last_id += 1;
        Box::new(RSNode::new(ast_item, self.krate, self.last_id))
    }
}

macro_rules! walk_node {
    ($visitor:expr, $e:expr) => {
        let node = $visitor.create_node($e);
        $visitor.visit(node.as_ref());
    }
}

impl<'ast> Visitor<'ast> for ApiVisitor<'ast> {
    fn visit_item(&mut self, i: &'ast Item) {
        walk_node!(self, RSASTItem::Item(i));
    }
    fn visit_pat(&mut self, p: &'ast Pat) {
        walk_node!(self, RSASTItem::Pat(p));
    }
    fn visit_trait_item(&mut self, ti: &'ast TraitItem) {
        walk_node!(self, RSASTItem::TraitItem(ti));
    }
    fn visit_impl_item(&mut self, ii: &'ast ImplItem) {
        walk_node!(self, RSASTItem::ImplItem(ii));
    }
    fn visit_struct_field(&mut self, s: &'ast StructField) {
        walk_node!(self, RSASTItem::Field(s));
    }
    fn visit_variant(&mut self, v: &'ast EnumVariant, g: &'ast Generics) {
        walk_node!(self, RSASTItem::Variant(v, g));
    }
    fn visit_path(&mut self, p: &'ast Path) {
        walk_node!(self, RSASTItem::Path(p));
    }
    fn visit_path_segment(&mut self, s: &'ast PathSegment) {
        walk_node!(self, RSASTItem::PathSegment(s));
    }
}
