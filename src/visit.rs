// Copyright 2017  Emma Gospodinova <emma.gospodinova@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use types::*;

pub trait Visitor<'ast> : Sized {
    fn visit_ident(&mut self, span: &'ast Span, ident: &'ast Identifier) {
    }
    fn visit_mod(&mut self, m: &'ast Mod, s: &'ast Span, attrs: &[Attribute]) {
        walk_mod(self, m);
    }
    fn visit_item(&mut self, i: &'ast Item) { walk_item(self, i) }
    fn visit_block(&mut self, b: &'ast Block) { walk_block(self, b) }
    fn visit_stmt(&mut self, s: &'ast Stmt) { walk_stmt(self, s) }
    fn visit_arm(&mut self, a: &'ast Arm) { walk_arm(self, a) }
    fn visit_pat(&mut self, p: &'ast Pat) { walk_pat(self, p) }
    fn visit_expr(&mut self, ex: &'ast Expr) { walk_expr(self, ex) }
    fn visit_type(&mut self, t: &'ast Type) { walk_type(self, t) }
    fn visit_generics(&mut self, g: &'ast Generics) { walk_generics(self, g) }
    /*fn visit_where_predicate(&mut self, p: &'ast WherePredicate) {
        walk_where_predicate(self, p)
    }*/
    fn visit_fn(&mut self, func: Function<'ast>, s: &'ast Span) {
        walk_fn(self, func, s)
    }
    fn visit_trait_item(&mut self, ti: &'ast TraitItem) { walk_trait_item(self, ti) }
    fn visit_impl_item(&mut self, ii: &'ast ImplItem) { walk_impl_item(self, ii) }
    fn visit_type_param_bound(&mut self, bound: &'ast TypeParamBound) {
        walk_type_param_bound(self, bound)
    }
    fn visit_variant_data(&mut self, s: &'ast VariantData) {
        walk_variant_data(self, s)
    }
    fn visit_struct_field(&mut self, s: &'ast StructField) { walk_struct_field(self, s) }
    fn visit_variant(&mut self, v: &'ast EnumVariant, g: &'ast Generics) {
        walk_variant(self, v, g)
    }

    fn visit_lifetime(&mut self, lifetime: &'ast Lifetime) {
        walk_lifetime(self, lifetime)
    }
    /*
    fn visit_lifetime_def(&mut self, lifetime: &'ast LifetimeDef) {
        walk_lifetime_def(self, lifetime)
    }
    */
    fn visit_macro(&mut self, mac: &'ast Macro) {
    }
    /*
    fn visit_macro_def(&mut self, mac: &'ast MacroDef) {
    }
    */
    fn visit_path(&mut self, path: &'ast Path) {
        walk_path(self, path)
    }
    fn visit_path_segment(&mut self, path_segment: &'ast PathSegment) {
        walk_path_segment(self, path_segment)
    }
    /*
    fn visit_path_parameters(&mut self, path_span: Span, path_parameters: &'ast PathParameters) {
        walk_path_parameters(self, path_span, path_parameters)
    }
    fn visit_path_list_item(&mut self, prefix: &'ast Path, item: &'ast PathListItem) {
        walk_path_list_item(self, prefix, item)
    }
    fn visit_assoc_type_binding(&mut self, type_binding: &'ast TypeBinding) {
        walk_assoc_type_binding(self, type_binding)
    }*/
    fn visit_attribute(&mut self, attr: &'ast Attribute) {}
    fn visit_vis(&mut self, vis: &'ast Visibility) {
        walk_vis(self, vis)
    }

}

pub enum Function<'a> {
    Function(&'a FunctionSig, &'a Block),
    Closure(&'a FnDecl, &'a Expr),
}

#[macro_export]
macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method(elem)
        }
    };
    ($visitor: expr, $method: ident, $list: expr, $($extra_args: expr),*) => {
        for elem in $list {
            $visitor.$method(elem, $($extra_args,)*)
        }
    }
}

pub fn walk_opt_ident<'a, V: Visitor<'a>>(visitor: &mut V, span: &'a Span, opt_ident: &'a Option<Identifier>) {
    if let Some(ref ident) = *opt_ident {
        visitor.visit_ident(span, ident);
    }
}

pub fn walk_opt_sp_ident<'a, V: Visitor<'a>>(visitor: &mut V, opt_ident: &'a Option<Spanned<Identifier>>) {
    if let Some(ref ident) = *opt_ident {
        visitor.visit_ident(&ident.span, &ident.node);
    }
}

pub fn walk_ident<'a, V: Visitor<'a>>(visitor: &mut V, span: &'a Span, ident: &'a Identifier) {
    visitor.visit_ident(span, ident);
}

pub fn walk_crate<'a, V: Visitor<'a>>(visitor: &mut V, krate: &'a Crate) {
    visitor.visit_mod(&krate.module, &krate.span, &krate.attrs);
    walk_list!(visitor, visit_attribute, &krate.attrs);
}

pub fn walk_mod<'a, V: Visitor<'a>>(visitor: &mut V, module: &'a Mod) {
    walk_list!(visitor, visit_item, &module.items);
}

pub fn walk_lifetime<'a, V: Visitor<'a>>(visitor: &mut V, lifetime: &'a Lifetime) {
    //visitor.visit_ident(lifetime);
}
/*
pub fn walk_lifetime_def<'a, V: Visitor<'a>>(visitor: &mut V, lifetime_def: &'a LifetimeDef) {
    visitor.visit_lifetime(&lifetime_def.lifetime);
    walk_list!(visitor, visit_lifetime, &lifetime_def.bounds);
    walk_list!(visitor, visit_attribute, &*lifetime_def.attrs);
}
*/
pub fn walk_item<'a, V: Visitor<'a>>(visitor: &mut V, item: &'a Item) {
    visitor.visit_vis(&item.vis);
    walk_opt_ident(visitor, &item.span, &item.ident);
    match item.node {
        Item_::ExternCrate(ref ident) => {
            walk_opt_ident(visitor, &item.span, &ident)
        }
        Item_::Use(ref vp) => {
            /*
            match vp.node {
                ViewPathSimple(ref ident, ref path) => {
                    visitor.visit_ident(&item.span, &ident);
                    visitor.visit_path(path);
                }
                ViewPathGlob(ref path) => {
                    visitor.visit_path(path);
                }
                ViewPathList(ref prefix, ref list) => {
                    visitor.visit_path(prefix);
                    /*for item in list {
                        visitor.visit_path_list_item(prefix, item)
                    }*/
                }
            }
            */
        }
        Item_::Static(ref ty, _, ref expr) |
        Item_::Const(ref ty, ref expr) => {
            visitor.visit_type(ty);
            visitor.visit_expr(expr);
        }
        Item_::Fn(ref sig, ref body) => {
            visitor.visit_fn(Function::Function(sig, body), &item.span)
        }
        Item_::Mod(ref module) => {
            visitor.visit_mod(module, &item.span, &item.attrs)
        }
        /*
        Item_::ForeignMod(ref foreign_module) => {
            walk_list!(visitor, visit_foreign_item, &foreign_module.items);
        }
        Item_::GlobalAsm(ref ga) => visitor.visit_global_asm(ga),
        */
        Item_::TypeAlias(ref ty, ref generics) => {
            visitor.visit_type(ty);
            visitor.visit_generics(generics)
        }
        Item_::Enum(ref variants, ref generics) => {
            visitor.visit_generics(generics);
            walk_list!(visitor, visit_variant, variants, generics);
        }
        /*Item_::DefaultImpl(_, ref trait_ref) => {
            visitor.visit_trait_ref(trait_ref)
        }*/
        Item_::Impl(_,
                 ref generics,
                 ref trait_ref,
                 ref typ,
                 ref impl_items) => {
            visitor.visit_generics(generics);
            walk_list!(visitor, visit_path, trait_ref);
            visitor.visit_type(typ);
            walk_list!(visitor, visit_impl_item, impl_items);
        }
        Item_::Struct(ref variant_data, ref generics) /*|
        Item_::Union(ref variant_data, ref generics)*/ => {
            visitor.visit_generics(generics);
            visitor.visit_variant_data(variant_data);
        }
        Item_::Trait(_, ref generics, ref bounds, ref methods) => {
            visitor.visit_generics(generics);
            walk_list!(visitor, visit_type_param_bound, bounds);
            walk_list!(visitor, visit_trait_item, methods);
        }
        Item_::Macro(ref mac) => visitor.visit_macro(mac),
        //Item_::MacroDef(ref ts) => visitor.visit_macro_def(ts, item.id),
        _ => ()
    }
    walk_list!(visitor, visit_attribute, &item.attrs);
}

pub fn walk_variant<'a, V>(visitor: &mut V,
                           variant: &'a EnumVariant,
                           generics: &'a Generics)
    where V: Visitor<'a>,
{
    walk_opt_ident(visitor, &variant.span, &variant.ident);
    visitor.visit_variant_data(&variant.node);
    //walk_list!(visitor, visit_expr, &variant.node.disr_expr);
    walk_list!(visitor, visit_attribute, &variant.attrs);
}

pub fn walk_type<'a, V: Visitor<'a>>(visitor: &mut V, typ: &'a Type) {
    match typ.node {
        Type_::Slice(ref ty) => {
            visitor.visit_type(ty)
        }
        Type_::Ptr(ref ty, _) => {
            visitor.visit_type(&ty)
        }
        Type_::Ref(ref ty, _, ref opt_lifetime) => {
            walk_list!(visitor, visit_lifetime, opt_lifetime);
            visitor.visit_type(&ty)
        }
        Type_::Never => {},
        Type_::Tuple(ref elem_tys) => {
            walk_list!(visitor, visit_type, elem_tys);
        }
        Type_::Fun(ref func) => {
            walk_fn_decl(visitor, &func.decl);
            //walk_list!(visitor, visit_lifetime_def, &function_declaration.lifetimes);
        }
        Type_::Path(ref path) => {
            /*if let Some(ref qself) = *maybe_qself {
                visitor.visit_type(&qself.ty);
            }*/
            visitor.visit_path(path);
        }
        Type_::Array(ref ty, ref expression) => {
            visitor.visit_type(ty);
            visitor.visit_expr(expression)
        }
        /*
        Type_::TraitObject(ref bounds) |
        Type_::ImplTrait(ref bounds) => {
            walk_list!(visitor, visit_type_param_bound, bounds);
        }
        Type_::Typeof(ref expression) => {
            visitor.visit_expr(expression)
        }
        */
        Type_::Var(_) | Type_::Err => (),
        Type_::Macro(ref mac) => visitor.visit_macro(mac),
        _ => ()
    }
}

pub fn walk_path<'a, V: Visitor<'a>>(visitor: &mut V, path: &'a Path) {
    for segment in &path.node {
        visitor.visit_path_segment(&segment);
    }
}

/*
pub fn walk_path_list_item<'a, V: Visitor<'a>>(visitor: &mut V,
                                               _prefix: &Path,
                                               item: &'a PathListItem) {
    visitor.visit_ident(item.span, item.node.name);
    walk_ident(visitor, item.span, item.node.rename);
}
*/
pub fn walk_path_segment<'a, V: Visitor<'a>>(visitor: &mut V,
                                             segment: &'a PathSegment) {
    visitor.visit_ident(&segment.span, &segment.node.ident);
    /*if let Some(ref parameters) = segment.parameters {
        visitor.visit_path_parameters(path_span, parameters);
    }*/
}
/*
pub fn walk_path_parameters<'a, V>(visitor: &mut V,
                                   span: Span,
                                   path_parameters: &'a PathParameters)
    where V: Visitor<'a>,
{
    match *path_parameters {
        PathParameters::AngleBracketed(ref data) => {
            walk_list!(visitor, visit_type, &data.types);
            walk_list!(visitor, visit_lifetime, &data.lifetimes);
            walk_list!(visitor, visit_assoc_type_binding, &data.bindings);
        }
        PathParameters::Parenthesized(ref data) => {
            walk_list!(visitor, visit_type, &data.inputs);
            walk_list!(visitor, visit_type, &data.output);
        }
    }
}

pub fn walk_assoc_type_binding<'a, V: Visitor<'a>>(visitor: &mut V,
                                                   type_binding: &'a TypeBinding) {
    visitor.visit_ident(type_binding.span, type_binding.ident);
    visitor.visit_type(&type_binding.ty);
}
*/
pub fn walk_pat<'a, V: Visitor<'a>>(visitor: &mut V, pat: &'a Pat) {
    match pat.node {
        Pat_::Wildcard => (),
        Pat_::Ident(ref ident, _, _, ref sub) => {
            visitor.visit_ident(&ident.span, &ident.node);
            walk_list!(visitor, visit_pat, sub);
        }
        Pat_::Lit(ref expression) => visitor.visit_expr(expression),
        Pat_::Struct(ref path, ref fields, _) => {
            visitor.visit_path(path);
            for field in fields {
                walk_list!(visitor, visit_attribute, field.attrs.iter());
                // the span here isn't strictly correct, as we discard it during transformation
                // however, given that there are a lot of places where a Spanned<Identifier> is preferred
                // over a simple Identifier, requiring correction of the span on the user side, we
                // ignore this
                walk_opt_ident(visitor, &field.span, &field.ident);
                visitor.visit_pat(&field.node)
            }
        }
        Pat_::TupleStruct(ref path, ref fields, _) => {
            visitor.visit_path(path);
            walk_list!(visitor, visit_pat, fields);
        }
        Pat_::Path(/*ref opt_qself,*/ ref path) => {
            /*if let Some(ref qself) = *opt_qself {
                visitor.visit_type(&qself.ty);
            }*/
            visitor.visit_path(path)
        }
        Pat_::Tuple(ref tuple_elements, _) => {
            walk_list!(visitor, visit_pat, tuple_elements);
        }
        Pat_::Box(ref sub) |
        Pat_::Ref(ref sub, _) => {
            visitor.visit_pat(sub)
        }
        Pat_::Range(ref begin, ref end, _) => {
            visitor.visit_expr(begin);
            visitor.visit_expr(end);
        }
        Pat_::Slice(ref begin, ref mid, ref end) => {
            walk_list!(visitor, visit_pat, begin);
            walk_list!(visitor, visit_pat, mid);
            walk_list!(visitor, visit_pat, end);
        }
        Pat_::Macro(ref mac) => visitor.visit_macro(mac),
    }
}
/*
pub fn walk_foreign_item<'a, V: Visitor<'a>>(visitor: &mut V, foreign_item: &'a ForeignItem) {
    visitor.visit_vis(&foreign_item.vis);
    visitor.visit_ident(foreign_item.span, foreign_item.ident);

    match foreign_item.node {
        ForeignItem_::Fn(ref function_declaration, ref generics) => {
            walk_fn_decl(visitor, function_declaration);
            visitor.visit_generics(generics)
        }
        ForeignItem_::Static(ref typ, _) => visitor.visit_type(typ),
    }

    walk_list!(visitor, visit_attribute, &foreign_item.attrs);
}

pub fn walk_global_asm<'a, V: Visitor<'a>>(_: &mut V, _: &'a GlobalAsm) {
    // Empty!
}
*/
pub fn walk_type_param_bound<'a, V: Visitor<'a>>(visitor: &mut V, bound: &'a TypeParamBound) {
    /*match *bound {
        TraitTyParamBound(ref typ, ref modifier) => {
            visitor.visit_poly_trait_ref(typ, modifier);
        }
        RegionTyParamBound(ref lifetime) => {
            visitor.visit_lifetime(lifetime);
        }
    }*/
}

pub fn walk_generics<'a, V: Visitor<'a>>(visitor: &mut V, generics: &'a Generics) {
    /*for param in &generics.ty_params {
        visitor.visit_ident(param.span, param.ident);
        walk_list!(visitor, visit_type_param_bound, &param.bounds);
        walk_list!(visitor, visit_type, &param.default);
        walk_list!(visitor, visit_attribute, &*param.attrs);
    }
    walk_list!(visitor, visit_lifetime_def, &generics.lifetimes);
    walk_list!(visitor, visit_where_predicate, &generics.where_clause.predicates);*/
}
/*
pub fn walk_where_predicate<'a, V: Visitor<'a>>(visitor: &mut V, predicate: &'a WherePredicate) {
    match *predicate {
        WherePredicate::BoundPredicate(WhereBoundPredicate{ref bounded_ty,
                                                           ref bounds,
                                                           ref bound_lifetimes,
                                                           ..}) => {
            visitor.visit_type(bounded_ty);
            walk_list!(visitor, visit_type_param_bound, bounds);
            walk_list!(visitor, visit_lifetime_def, bound_lifetimes);
        }
        WherePredicate::RegionPredicate(WhereRegionPredicate{ref lifetime,
                                                             ref bounds,
                                                             ..}) => {
            visitor.visit_lifetime(lifetime);
            walk_list!(visitor, visit_lifetime, bounds);
        }
        WherePredicate::EqPredicate(WhereEqPredicate{ref lhs_ty,
                                                     ref rhs_ty,
                                                     ..}) => {
            visitor.visit_type(lhs_ty);
            visitor.visit_type(rhs_ty);
        }
    }
}
*/
pub fn walk_fn_decl<'a, V: Visitor<'a>>(visitor: &mut V, decl: &'a FnDecl) {
    for arg in &decl.args {
        visitor.visit_pat(&arg.pat);
        visitor.visit_type(&arg.ty)
    }
    visitor.visit_type(&decl.return_type)
}

pub fn walk_fn<'a, V: Visitor<'a>>(visitor: &mut V, func: Function<'a>, span: &'a Span) {
    match func {
        Function::Function(ref sig, ref body) => {
            visitor.visit_generics(&sig.generics);
            walk_fn_decl(visitor, &sig.decl);
            visitor.visit_block(body);
        }
        Function::Closure(ref decl, ref body) => {
            walk_fn_decl(visitor, decl);
            visitor.visit_expr(body);
        }
    }
}

pub fn walk_trait_item<'a, V: Visitor<'a>>(visitor: &mut V, trait_item: &'a TraitItem) {
    walk_opt_ident(visitor, &trait_item.span, &trait_item.ident);
    walk_list!(visitor, visit_attribute, &trait_item.attrs);
    walk_item_member(visitor, &trait_item.node, &trait_item.span);
}

pub fn walk_impl_item<'a, V: Visitor<'a>>(visitor: &mut V, impl_item: &'a ImplItem) {
    visitor.visit_vis(&impl_item.vis);
    walk_opt_ident(visitor, &impl_item.span, &impl_item.ident);
    walk_list!(visitor, visit_attribute, &impl_item.attrs);
    walk_item_member(visitor, &impl_item.node, &impl_item.span);
}

pub fn walk_item_member<'a, V: Visitor<'a>>(visitor: &mut V, item_member: &'a ItemMember, span: &'a Span) {
    match *item_member {
        ItemMember::Const(ref ty, ref default) => {
            visitor.visit_type(ty);
            walk_list!(visitor, visit_expr, default);
        }
        ItemMember::Method(ref sig, None) => {
            visitor.visit_generics(&sig.generics);
            walk_fn_decl(visitor, &sig.decl);
        }
        ItemMember::Method(ref sig, Some(ref body)) => {
            visitor.visit_fn(Function::Function(sig, body), span);
        }
        ItemMember::Type(ref bounds, ref default) => {
            walk_list!(visitor, visit_type_param_bound, bounds);
            walk_list!(visitor, visit_type, default);
        }
        ItemMember::Macro(ref mac) => {
            visitor.visit_macro(mac);
        }
        _ => ()
    }
}

pub fn walk_variant_data<'a, V: Visitor<'a>>(visitor: &mut V, variant_data: &'a VariantData) {
    match *variant_data {
        VariantData::Unit => (),
        VariantData::Tuple(ref fields) |
        VariantData::Struct(ref fields) => walk_list!(visitor, visit_struct_field, fields)
    }
}

pub fn walk_struct_field<'a, V: Visitor<'a>>(visitor: &mut V, field: &'a StructField) {
    visitor.visit_vis(&field.vis);
    walk_opt_ident(visitor, &field.span, &field.ident);
    visitor.visit_type(&field.node);
    walk_list!(visitor, visit_attribute, &field.attrs);
}

pub fn walk_block<'a, V: Visitor<'a>>(visitor: &mut V, block: &'a Block) {
    walk_list!(visitor, visit_stmt, &block.node);
}

pub fn walk_stmt<'a, V: Visitor<'a>>(visitor: &mut V, statement: &'a Stmt) {
    match statement.node {
        Stmt_::Local(ref pat, ref ty, ref expr) => {
            visitor.visit_pat(&*pat);
            walk_list!(visitor, visit_type, ty);
            walk_list!(visitor, visit_expr, expr);
        },
        Stmt_::Item(ref item) => visitor.visit_item(item),
        Stmt_::ExpStmt(ref expr) | Stmt_::UnitStmt(ref expr) => {
            visitor.visit_expr(expr)
        }
        Stmt_::Macro(ref mac) => {
            //let (ref mac, _, ref attrs) = **mac;
            visitor.visit_macro(mac);
            /*for attr in attrs.iter() {
                visitor.visit_attribute(attr);
            }*/
        }
        _ => ()
    }
}

pub fn walk_expr<'a, V: Visitor<'a>>(visitor: &mut V, expression: &'a Expr) {
    /*for attr in expression.attrs.iter() {
        visitor.visit_attribute(attr);
    }*/
    match expression.node {
        Expr_::Box(ref expr) => {
            visitor.visit_expr(expr)
        }
        /*Expr_::InPlace(ref place, ref expr) => {
            visitor.visit_expr(place);
            visitor.visit_expr(expr)
        }*/
        Expr_::Array(ref exprs) => {
            walk_list!(visitor, visit_expr, exprs);
        }
        Expr_::Repeat(ref element, ref count) => {
            visitor.visit_expr(element);
            visitor.visit_expr(count)
        }
        Expr_::Struct(ref path, ref fields, ref base) => {
            visitor.visit_path(path);
            for field in fields {
                walk_list!(visitor, visit_attribute, field.attrs.iter());
                walk_opt_ident(visitor, &field.span, &field.ident);
                visitor.visit_expr(&field.node)
            }
            walk_list!(visitor, visit_expr, base);
        }
        Expr_::Tuple(ref exprs) => {
            walk_list!(visitor, visit_expr, exprs);
        }
        Expr_::Call(ref callee, ref arguments) => {
            visitor.visit_expr(callee);
            walk_list!(visitor, visit_expr, arguments);
        }
        Expr_::MethodCall(ref path_segment, ref arguments) => {
            visitor.visit_path_segment(path_segment);
            walk_list!(visitor, visit_expr, arguments);
        }
        Expr_::BinApp(_, ref lhs, ref rhs) => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs)
        }
        Expr_::AddrOf(_, ref expr) | Expr_::UnApp(_, ref expr) => {
            visitor.visit_expr(expr)
        }
        Expr_::Lit(_) => {}
        Expr_::Cast(ref expr, ref typ) /*| Expr_::Type(ref expr, ref typ)*/ => {
            visitor.visit_expr(expr);
            visitor.visit_type(typ)
        }
        Expr_::If(ref cond, ref if_block, ref optional_else) => {
            visitor.visit_expr(cond);
            visitor.visit_block(if_block);
            walk_list!(visitor, visit_expr, optional_else);
        }
        Expr_::While(ref cond, ref block, ref ident) => {
            visitor.visit_expr(cond);
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, ident);
        }
        Expr_::IfLet(ref pat, ref cond, ref if_block, ref optional_else) => {
            visitor.visit_pat(pat);
            visitor.visit_expr(cond);
            visitor.visit_block(if_block);
            walk_list!(visitor, visit_expr, optional_else);
        }
        Expr_::WhileLet(ref pattern, ref cond, ref block, ref ident) => {
            visitor.visit_pat(pattern);
            visitor.visit_expr(cond);
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, ident);
        }
        Expr_::For(ref pattern, ref expr, ref block, ref ident) => {
            visitor.visit_pat(pattern);
            visitor.visit_expr(expr);
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, ident);
        }
        Expr_::Loop(ref block, ref ident) => {
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, ident);
        }
        Expr_::Match(ref expr, ref arms) => {
            visitor.visit_expr(expr);
            walk_list!(visitor, visit_arm, arms);
        }
        Expr_::Closure(_, ref decl, ref body) => {
            visitor.visit_fn(Function::Closure(decl, body), &expression.span)
        }
        Expr_::Block(ref block) => visitor.visit_block(block),
        Expr_::Assign(ref lhs, ref rhs) => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        Expr_::BinAppAssign(_, ref lhs, ref rhs) => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        Expr_::Field(ref expr, ref ident) => {
            visitor.visit_expr(expr);
            visitor.visit_ident(&ident.span, &ident.node);
        }
        Expr_::TupleElem(ref expr, _) => {
            visitor.visit_expr(expr);
        }
        Expr_::Index(ref expr, ref idx) => {
            visitor.visit_expr(expr);
            visitor.visit_expr(idx)
        }
        /*
        Expr_::Range(ref start, ref end, _) => {
            walk_list!(visitor, visit_expr, start);
            walk_list!(visitor, visit_expr, end);
        }
        */
        Expr_::Path(/*ref maybe_qself,*/ ref path) => {
            /*if let Some(ref qself) = *maybe_qself {
                visitor.visit_type(&qself.ty);
            }*/
            visitor.visit_path(path)
        }
        Expr_::Break(ref ident, ref opt_expr) => {
            walk_opt_sp_ident(visitor, ident);
            walk_list!(visitor, visit_expr, opt_expr);
        }
        Expr_::Continue(ref ident) => {
            walk_opt_sp_ident(visitor, ident);
        }
        Expr_::Return(ref expr) => {
            walk_list!(visitor, visit_expr, expr);
        }
        Expr_::Macro(ref mac) => {
            visitor.visit_macro(mac);
        }
        /*Expr_::Paren(ref expr) => {
            visitor.visit_expr(expr)
        }
        Expr_::InlineAsm(ref ia) => {
            for &(_, ref input) in &ia.inputs {
                visitor.visit_expr(input)
            }
            for output in &ia.outputs {
                visitor.visit_expr(&output.expr)
            }
        }
        Expr_::Try(ref expr) => {
            visitor.visit_expr(expr)
        }
        Expr_::Catch(ref body) => {
            visitor.visit_block(body)
        }
        */
        _ => ()
    }
}

pub fn walk_arm<'a, V: Visitor<'a>>(visitor: &mut V, arm: &'a Arm) {
    walk_list!(visitor, visit_pat, &arm.node.pats);
    walk_list!(visitor, visit_expr, &arm.node.guard);
    visitor.visit_expr(&arm.node.expr);
    walk_list!(visitor, visit_attribute, &arm.node.attrs);
}

pub fn walk_vis<'a, V: Visitor<'a>>(visitor: &mut V, vis: &'a Visibility) {
    /*if let Visibility::Restricted { ref path, id } = *vis {
        visitor.visit_path(path, id);
    }*/
}
