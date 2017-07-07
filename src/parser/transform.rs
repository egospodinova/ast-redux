use syntax::ast;
use syntax::abi;
use syntax::ptr as syntax_ptr;
use syntax::codemap::{self, CodeMap, FilePathMapping};
use syntax::errors;

use types::*;

macro_rules! opt_map {
    ($value:expr, $fn:expr) => ($value.iter().map($fn).next())
}

/// The reason we can't use .and_then() is because it consumes the optional
macro_rules! vec_map {
    ($value:expr, $fn:expr) => ($value.iter().map($fn).collect())
}

pub struct ASTTransformer<'a> {
    should_skip_function_bodies: bool,
    codemap: &'a CodeMap
}

impl<'a> ASTTransformer<'a> {
    pub fn new(should_skip_function_bodies: bool, codemap: &'a CodeMap) -> ASTTransformer<'a> {
        ASTTransformer {
            should_skip_function_bodies: should_skip_function_bodies,
            codemap: codemap
        }
    }

    pub fn transform_crate(&mut self, krate: &ast::Crate) -> Crate {
        Crate {
            module: self.transform_mod(&krate.module),
            attrs: self.transform_attrs(&krate.attrs),
            span: self.transform_span(&krate.span)
        }
    }

    fn transform_mod(&mut self, module: &ast::Mod) -> Mod {
        Mod {
            span: self.transform_span(&module.inner),
            items: vec_map!(module.items, |i| P::new(self.transform_item(i)))
        }
    }

    fn transform_item(&mut self, item: &ast::Item) -> Item {
        Item {
            ident: Some(self.transform_symbol(&item.ident.name)),
            attrs: self.transform_attrs(&item.attrs),
            span: self.transform_span(&item.span),
            node: self.transform_item_kind(&item.node),
            vis: self.transform_visibility(&item.vis)
        }
    }

    fn transform_item_kind(&mut self, item_kind: &ast::ItemKind) -> Item_ {
        match *item_kind {
            ast::ItemKind::ExternCrate(ref name)
                => Item_::ExternCrate(name.map(|n| self.transform_symbol(&n))),
            ast::ItemKind::Static(ref ty, ref mu, ref init)
                => Item_::Static(P::new(self.transform_type(&*ty)),
                                 self.transform_mutability(&mu),
                                 P::new(self.transform_expr(&*init))),
            ast::ItemKind::Const(ref ty, ref init)
                => Item_::Const(P::new(self.transform_type(&*ty)),
                                P::new(self.transform_expr(&*init))),
            ast::ItemKind::Mod(ref module)
                => Item_::Mod(self.transform_mod(&module)),
            ast::ItemKind::Ty(ref ty, ref generics)
                => Item_::TypeAlias(P::new(self.transform_type(&*ty)),
                                    self.transform_generics(&generics)),
            ast::ItemKind::Enum(ref def, ref generics)
                => Item_::Enum(vec_map!(def.variants, |v| self.transform_variant(v)),
                               self.transform_generics(&generics)),
            ast::ItemKind::Struct(ref variant_data, ref generics)
                => Item_::Struct(self.transform_variant_data(&variant_data),
                                 self.transform_generics(&generics)),
            ast::ItemKind::Trait(ref unsafety, ref generics, ref bounds, ref items)
                => Item_::Trait(self.transform_unsafety(unsafety),
                                self.transform_generics(generics),
                                vec_map!(bounds, |b| self.transform_bound(b)),
                                vec_map!(items, |i| self.transform_trait_item(i))),
            ast::ItemKind::Impl(ref unsafety, _, _, ref generics, ref trait_ref, ref ty, ref items)
                => Item_::Impl(self.transform_unsafety(unsafety),
                              self.transform_generics(generics),
                               opt_map!(trait_ref, |t| self.transform_path(&t.path)),
                               P::new(self.transform_type(&*ty)),
                               vec_map!(items, |i| self.transform_impl_item(i))),
            ast::ItemKind::Fn(ref decl, ref unsafety, ref constness, ref abi, ref generics, ref block)
                => Item_::Fn(self.transform_function_sig(&*decl, unsafety, constness, abi, generics),
                             P::new(self.transform_block(block))),
            _ => unimplemented!()
        }
    }

    fn transform_trait_item(&mut self, item: &ast::TraitItem) -> TraitItem {
        let item_member = match &item.node {
            &ast::TraitItemKind::Const(ref ty, ref expr)
                => ItemMember::Const(P::new(self.transform_type(&*ty)),
                                      opt_map!(expr, |e| P::new(self.transform_expr(e)))),
            &ast::TraitItemKind::Method(ref sig, ref block)
                => ItemMember::Method(self.transform_function_sig(&*sig.decl, &sig.unsafety, &sig.constness,
                                                                   &sig.abi, &sig.generics),
                                       opt_map!(block, |b| P::new(self.transform_block(b)))),
            &ast::TraitItemKind::Type(ref bounds, ref ty)
                => ItemMember::Type(vec_map!(bounds, |b| self.transform_bound(b)),
                                     opt_map!(ty, |t| P::new(self.transform_type(t)))),
            &ast::TraitItemKind::Macro(ref m)
                => ItemMember::Macro(self.transform_macro(m)),
            _   => unimplemented!()
        };

        TraitItem {
            ident: Some(self.transform_symbol(&item.ident.name)),
            attrs: self.transform_attrs(&item.attrs),
            span: self.transform_span(&item.span),
            node: item_member
        }
    }

    fn transform_impl_item(&mut self, item: &ast::ImplItem) -> ImplItem {
        // TODO: possibly could reduce duplication with method above
        let item_member = match &item.node {
            &ast::ImplItemKind::Const(ref ty, ref expr)
                => ItemMember::Const(P::new(self.transform_type(&*ty)),
                                      Some(P::new(self.transform_expr(expr)))),
            &ast::ImplItemKind::Method(ref sig, ref block)
                => ItemMember::Method(self.transform_function_sig(&*sig.decl, &sig.unsafety, &sig.constness,
                                                                   &sig.abi, &sig.generics),
                                       Some(P::new(self.transform_block(block)))),
            &ast::ImplItemKind::Type(ref ty)
                => ItemMember::Type(vec![], Some(P::new(self.transform_type(ty)))),
            &ast::ImplItemKind::Macro(ref m)
                => ItemMember::Macro(self.transform_macro(m)),
            _   => unimplemented!()
        };

        ImplItem {
            ident: Some(self.transform_symbol(&item.ident.name)),
            attrs: self.transform_attrs(&item.attrs),
            span: self.transform_span(&item.span),
            vis: self.transform_visibility(&item.vis),
            node: item_member
        }
    }

    fn transform_function_sig(&mut self, decl: &ast::FnDecl, unsafety: &ast::Unsafety,
                              constness: &codemap::Spanned<ast::Constness>,
                              abi: &abi::Abi, generics: &ast::Generics) -> FunctionSig {
        FunctionSig {
            decl: P::new(self.transform_fn_decl(decl)),
            unsafety: self.transform_unsafety(unsafety),
            generics: self.transform_generics(generics)
        }
    }

    fn transform_fn_decl(&mut self, decl: &ast::FnDecl) -> FnDecl {
        let ret_ty = match &decl.output {
            &ast::FunctionRetTy::Default(ref span)   => Type { span: self.transform_span(&span), node: Type_::Tuple(vec![]) },
            &ast::FunctionRetTy::Ty(ref ty)          => self.transform_type(&*ty)
        };

        FnDecl {
            args: vec_map!(decl.inputs, |a| self.transform_arg(a)),
            return_type: P::new(ret_ty),
            variadic: decl.variadic
        }
    }

    fn transform_block(&mut self, block: &ast::Block) -> Block {
        Block {
            span: self.transform_span(&block.span),
            node: vec_map!(block.stmts, |s| self.transform_stmt(s))
        }
    }

    fn transform_stmt(&mut self, stmt: &ast::Stmt) -> Stmt {
        let stmt_kind = match stmt.node {
            ast::StmtKind::Local(ref local)
                => Stmt_::Local(P::new(self.transform_pat(&local.pat)),
                                opt_map!(local.ty, |t| P::new(self.transform_type(t))),
                                opt_map!(local.init, |e| P::new(self.transform_expr(e)))),
            ast::StmtKind::Item(ref item)
                => Stmt_::Item(P::new(self.transform_item(item))),
            ast::StmtKind::Expr(ref expr)
                => Stmt_::ExpStmt(P::new(self.transform_expr(expr))),
            ast::StmtKind::Semi(ref expr)
                => Stmt_::UnitStmt(P::new(self.transform_expr(expr))),
            ast::StmtKind::Mac(ref mac)
                => Stmt_::Macro(P::new(self.transform_macro(&(*mac).0))), // FIXME ew.
            _   => unimplemented!()
        };
        Stmt {
            span: self.transform_span(&stmt.span),
            node: stmt_kind
        }
    }

    fn transform_pat(&mut self, pat: &ast::Pat) -> Pat {
        let pat_kind = Pat_::Wildcard;
        Pat {
            span: self.transform_span(&pat.span),
            node: pat_kind
        }
    }

    fn transform_arg(&mut self, arg: &ast::Arg) -> Arg {
        Arg {
            pat: P::new(self.transform_pat(&*arg.pat)),
            ty: P::new(self.transform_type(&*arg.ty))
        }
    }

    fn transform_variant(&mut self, variant: &ast::Variant) -> EnumVariant {
        EnumVariant {
            ident: Some(self.transform_symbol(&variant.node.name.name)),
            attrs: self.transform_attrs(&variant.node.attrs),
            node: self.transform_variant_data(&variant.node.data),
            span: self.transform_span(&variant.span)
        }
    }

    fn transform_variant_data(&mut self, variant_data: &ast::VariantData) -> VariantData {
        match *variant_data {
            ast::VariantData::Unit(_)               => VariantData::Unit,
            ast::VariantData::Struct(ref fields, _) => VariantData::Struct(vec![]),
            ast::VariantData::Tuple(ref fields, _)  => VariantData::Tuple(vec![])
        }
    }

    fn transform_generics(&mut self, generics: &ast::Generics) -> Generics {
        Generics {}
    }

    fn transform_bound(&mut self, bound: &ast::TyParamBound) -> TypeParamBound {
        TypeParamBound {}
    }

    fn transform_visibility(&mut self, visibility: &ast::Visibility) -> Visibility {
        Visibility {}
    }

    fn transform_type(&mut self, ty: &ast::Ty) -> Type {
        let transformed_ty = match ty {
            _ => Type_::Never
        };
        Type {
            span: self.transform_span(&ty.span),
            node: transformed_ty
        }
    }

    fn transform_expr(&mut self, expr: &ast::Expr) -> Expr {
        macro_rules! trans_spanned_ident {
            ($i:expr) => (Spanned {
                span: self.transform_span(&$i.span),
                node: self.transform_symbol(&$i.node.name)
            })
        }
        macro_rules! trans_exp {
            ($e:expr) => (P::new(self.transform_expr(&*$e)))
        }
        let expr_kind = match expr.node {
            ast::ExprKind::Box(ref e)
                => Expr_::Box(trans_exp!(e)),
            ast::ExprKind::Array(ref es)
                => Expr_::Array(vec_map!(es, |e| trans_exp!(e))),
            ast::ExprKind::Call(ref e, ref args)
                => Expr_::Call(trans_exp!(e), vec_map!(args, |e| trans_exp!(e))),
            ast::ExprKind::MethodCall(ref ident, ref tys, ref args)
                => unimplemented!(),
            ast::ExprKind::Tup(ref es)
                => Expr_::Tuple(vec_map!(es, |e| trans_exp!(e))),
            ast::ExprKind::Binary(ref op, ref lhs, ref rhs)
                => Expr_::BinApp(self.transform_bin_op(op), trans_exp!(lhs), trans_exp!(rhs)),
            ast::ExprKind::Unary(ref op, ref e)
                => Expr_::UnApp(self.transform_un_op(op), trans_exp!(e)),
            ast::ExprKind::Lit(ref lit)
                => Expr_::Lit(self.transform_lit(&*lit)),
            ast::ExprKind::Cast(ref e, ref ty)
                => Expr_::Cast(trans_exp!(e), P::new(self.transform_type(ty))),
            ast::ExprKind::If(ref c, ref t, ref f)
                => Expr_::If(trans_exp!(c), P::new(self.transform_block(&*t)), opt_map!(f, |e| trans_exp!(e))),
            ast::ExprKind::IfLet(ref p, ref e, ref b, ref f)
                => Expr_::IfLet(P::new(self.transform_pat(&*p)), trans_exp!(e), P::new(self.transform_block(&*b)),
                                opt_map!(f, |e| trans_exp!(e))),
            ast::ExprKind::While(ref c, ref b, ref i)
                => Expr_::While(trans_exp!(c), P::new(self.transform_block(&*b)),
                                opt_map!(i, |ident| trans_spanned_ident!(ident))),
            ast::ExprKind::WhileLet(ref p, ref e, ref b, ref i)
                => Expr_::WhileLet(P::new(self.transform_pat(&*p)), trans_exp!(e), P::new(self.transform_block(&*b)),
                                   opt_map!(i, |ident| trans_spanned_ident!(ident))),
            ast::ExprKind::ForLoop(ref p, ref e, ref b, ref i)
                => Expr_::For(P::new(self.transform_pat(&*p)), trans_exp!(e), P::new(self.transform_block(&*b)),
                              opt_map!(i, |ident| trans_spanned_ident!(ident))),
            ast::ExprKind::Loop(ref b, ref i)
                => Expr_::Loop(P::new(self.transform_block(&*b)), opt_map!(i, |ident| trans_spanned_ident!(ident))),
            ast::ExprKind::Match(ref e, ref arms)
                => Expr_::Match(trans_exp!(e), vec_map!(arms, |a| self.transform_arm(a))),
            ast::ExprKind::Closure(ref cap, ref decl, ref e, ref s)
                => Expr_::Closure(self.transform_capture_kind(cap), P::new(self.transform_fn_decl(&*decl)),
                                  trans_exp!(e)),
            ast::ExprKind::Block(ref b)
                => Expr_::Block(P::new(self.transform_block(&*b))),
            ast::ExprKind::Assign(ref lhs, ref rhs)
                => Expr_::Assign(trans_exp!(lhs), trans_exp!(rhs)),
            ast::ExprKind::AssignOp(ref op, ref lhs, ref rhs)
                => Expr_::BinAppAssign(self.transform_bin_op(&op), trans_exp!(lhs), trans_exp!(rhs)),
            ast::ExprKind::Field(ref e, ref i)
                => Expr_::Field(trans_exp!(e), trans_spanned_ident!(i)),
            ast::ExprKind::TupField(ref e, ref i)
                => Expr_::TupleElem(trans_exp!(e), Spanned { span: self.transform_span(&i.span), node: i.node }),
            ast::ExprKind::Index(ref e, ref idx)
                => Expr_::Index(trans_exp!(e), trans_exp!(idx)),
            ast::ExprKind::Path(ref qs, ref p)
                => Expr_::Path(self.transform_path(&*p)),
            ast::ExprKind::AddrOf(ref m, ref e)
                => Expr_::AddrOf(self.transform_mutability(m), trans_exp!(e)),
            ast::ExprKind::Break(ref i, ref e)
                => Expr_::Break(opt_map!(i, |ident| trans_spanned_ident!(ident)), 
                                opt_map!(e, |expr| trans_exp!(expr))),
            ast::ExprKind::Continue(ref i)
                => Expr_::Continue(opt_map!(i, |ident| trans_spanned_ident!(ident))),
            ast::ExprKind::Ret(ref v)
                => Expr_::Return(opt_map!(v, |e| trans_exp!(e))),
            ast::ExprKind::Repeat(ref e, ref n)
                => Expr_::Repeat(trans_exp!(e), trans_exp!(n)),
            ast::ExprKind::Struct(ref p, ref fs, ref e)
                => Expr_::Struct(self.transform_path(p), vec_map!(fs, |f| self.transform_field(f)),
                                 opt_map!(e, |expr| trans_exp!(expr))),
            ast::ExprKind::Mac(ref m)
                => Expr_::Macro(self.transform_macro(m)),
            ast::ExprKind::Paren(ref e)
                => Expr_::Paren(trans_exp!(e)),
            // not implemented:
            //ast::ExprKind::InlineAsm(P<InlineAsm>),
            //ast::ExprKind::Range(Option<P<Expr>>, Option<P<Expr>>, RangeLimits),
            //ast::ExprKind::Catch(P<Block>),
            //ast::ExprKind::Type(P<Expr>, P<Ty>),
            //ast::ExprKind::InPlace(P<Expr>, P<Expr>),
            //ast::ExprKind::Try(P<Expr>),
            _ => unimplemented!()
        };
        Expr {
            span: self.transform_span(&expr.span),
            node: expr_kind
        }
    }

    fn transform_bin_op(&mut self, op: &ast::BinOp) -> BinOp {
        match op.node {
            ast::BinOpKind::Add     => BinOp::Add,
            ast::BinOpKind::Sub     => BinOp::Sub,
            ast::BinOpKind::Mul     => BinOp::Mul,
            ast::BinOpKind::Div     => BinOp::Div,
            ast::BinOpKind::Rem     => BinOp::Rem,
            ast::BinOpKind::And     => BinOp::And,
            ast::BinOpKind::Or      => BinOp::Or,
            ast::BinOpKind::BitXor  => BinOp::BitXor,
            ast::BinOpKind::BitAnd  => BinOp::BitAnd,
            ast::BinOpKind::BitOr   => BinOp::BitOr,
            ast::BinOpKind::Shl     => BinOp::Shl,
            ast::BinOpKind::Shr     => BinOp::Shr,
            ast::BinOpKind::Eq      => BinOp::Eq,
            ast::BinOpKind::Ne      => BinOp::Ne,
            ast::BinOpKind::Lt      => BinOp::Lt,
            ast::BinOpKind::Le      => BinOp::Le,
            ast::BinOpKind::Gt      => BinOp::Gt,
            ast::BinOpKind::Ge      => BinOp::Ge
        }
    }

    fn transform_un_op(&mut self, op: &ast::UnOp) -> UnOp {
        match *op {
            ast::UnOp::Deref    => UnOp::Deref,
            ast::UnOp::Neg      => UnOp::Neg,
            ast::UnOp::Not      => UnOp::Not
        }
    }

    fn transform_arm(&mut self, arm: &ast::Arm) -> Arm {
        Arm {
            attrs: self.transform_attrs(&arm.attrs),
            pats: vec_map!(arm.pats, |p| P::new(self.transform_pat(p))),
            guard: opt_map!(arm.guard, |g| P::new(self.transform_expr(&*g))),
            expr: P::new(self.transform_expr(&*arm.body))
        }
    }

    fn transform_lit(&mut self, lit: &ast::Lit) -> Literal {
        Literal::Int(1337)
    }

    fn transform_capture_kind(&mut self, capture_kind: &ast::CaptureBy) -> CaptureKind {
        match *capture_kind {
            ast::CaptureBy::Value => CaptureKind::ByValue,
            ast::CaptureBy::Ref => CaptureKind::ByRef
        }
    }

    fn transform_field(&mut self, field: &ast::Field) -> Field {
        Field {}
    }

    fn transform_path(&mut self, path: &ast::Path) -> Path {
        Path {
            span: self.transform_span(&path.span),
            node: vec_map!(path.segments, |s| self.transform_path_segment(s))
        }
    }

    fn transform_path_segment(&mut self, segment: &ast::PathSegment) -> Spanned<PathSegment> {
        Spanned {
            span: self.transform_span(&segment.span),
            node: PathSegment {
                ident: self.transform_symbol(&segment.identifier.name)
            }
        }
    }

    fn transform_attrs(&mut self, attrs: &[ast::Attribute]) -> Vec<Attribute> {
        vec_map!(attrs, |a| self.transform_attr(a))
    }

    fn transform_attr(&mut self, attr: &ast::Attribute) -> Attribute {
        Attribute {}
    }

    fn transform_macro(&mut self, mac: &ast::Mac) -> Macro {
        Macro {}
    }

    fn transform_unsafety(&mut self, unsafety: &ast::Unsafety) -> Unsafety {
        match *unsafety {
            ast::Unsafety::Unsafe => Unsafety::Unsafe,
            ast::Unsafety::Normal => Unsafety::Safe
        }
    }

    fn transform_mutability(&mut self, mutability: &ast::Mutability) -> Mutability {
        match *mutability {
            ast::Mutability::Mutable    => Mutability::Mutable,
            ast::Mutability::Immutable  => Mutability::Immutable
        }
    }

    fn transform_symbol(&mut self, symbol: &ast::Name) -> Identifier {
        (*symbol.as_str()).to_owned()
    }

    fn transform_span(&mut self, span: &codemap::Span) -> Span {
        let lo = self.codemap.lookup_char_pos(span.lo);
        let hi = self.codemap.lookup_char_pos(span.hi);
        Span {
            start: self.transform_loc(&lo),
            end: self.transform_loc(&hi)
        }
    }

    fn transform_loc(&mut self, loc: &codemap::Loc) -> Location {
        Location {
            line: loc.line as i32,
            column: loc.col.0 as i32
        }
    }
}
