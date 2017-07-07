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

    let mut visitor = ApiVisitor::new((*node).get_crate(), callback, data);
    visitor.walk(node);
}

#[no_mangle]
pub unsafe extern fn node_get_crate(node: *const RSNode) -> *const RSCrate {
    (*node).get_crate()
}

#[no_mangle]
pub unsafe extern fn parse_crate(name: *mut ::libc::c_char, src: *mut ::libc::c_char) -> *const RSCrate {
    let name = if !name.is_null() { CStr::from_ptr(name).to_str().unwrap_or("") } else { "" };

    if let Ok(source_str) = CStr::from_ptr(src).to_str() {
        if let Some(krate) = parse_source(name.to_owned(), source_str.to_owned()) {
            let rs_crate = Box::new(RSCrate::new(krate));
            return Box::into_raw(rs_crate);
        }
    }

    ptr::null()
}

#[no_mangle]
pub unsafe extern fn node_from_crate<'a>(krate: *const RSCrate) -> *const RSNode<'a> {
    if krate.is_null() { return ptr::null(); }
    Box::into_raw(Box::new(RSNode::from_crate(&*krate)))
}

#[no_mangle]
pub unsafe extern fn node_get_spelling_name(node: *const RSNode) -> *const ::libc::c_char {
    // FIXME move this out of here
    match *(*node).get_ast_item() {
        RSASTItem::Item(i) => match i.node {
            //Item_::Impl(_, _, _, _, _, ref ty, _) => CString::new(pprust::ty_to_string(&*ty)).unwrap().into_raw(),
            _ => CString::new(i.name()).unwrap().into_raw(),
        },
        RSASTItem::Variant(v, _) => CString::new(v.name()).unwrap().into_raw(),
        RSASTItem::Field(f) => if let Some(ref id) = f.ident {
            CString::new(id.clone()).unwrap().into_raw()
        } else { ptr::null() },
        RSASTItem::Pat(p) => match p.node {
            Pat_::Ident(ref id, _) => CString::new(id.node.clone()).unwrap().into_raw(),
            _ => ptr::null()
        },
        RSASTItem::TraitItem(t) => CString::new(t.name()).unwrap().into_raw(),
        RSASTItem::ImplItem(i) => CString::new(i.name()).unwrap().into_raw(),
        _ => ptr::null()
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
        RSRange::at_span_start(span)
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
                    &RSASTItem::Variant(v, g)  => visit::walk_variant(self, v, g),
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
    fn visit_item(&mut self, i: &'ast Item) {
        let item = Box::new(RSNode::new(RSASTItem::Item(i), self.krate));
        self.walk(item.as_ref());
    }
    fn visit_pat(&mut self, p: &'ast Pat) {
        let pat = Box::new(RSNode::new(RSASTItem::Pat(p), self.krate));
        self.walk(pat.as_ref());
    }
    fn visit_trait_item(&mut self, ti: &'ast TraitItem) {
        let trait_item = Box::new(RSNode::new(RSASTItem::TraitItem(ti), self.krate));
        self.walk(trait_item.as_ref());
    }
    fn visit_impl_item(&mut self, ii: &'ast ImplItem) {
        let impl_item = Box::new(RSNode::new(RSASTItem::ImplItem(ii), self.krate));
        self.walk(impl_item.as_ref());
    }
    fn visit_struct_field(&mut self, s: &'ast StructField) {
        let field = Box::new(RSNode::new(RSASTItem::Field(s), self.krate));
        self.walk(field.as_ref());
    }
    fn visit_variant(&mut self, v: &'ast EnumVariant, g: &'ast Generics) {
        let var = Box::new(RSNode::new(RSASTItem::Variant(v, g), self.krate));
        self.walk(var.as_ref());
    }
}
