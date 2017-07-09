use std::boxed::Box;

pub type Identifier = String;
pub type P<T> = Box<T>;
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

pub trait Named {
    fn name(&self) -> String;
}

pub struct ItemLike<T> {
    pub ident: Option<Identifier>,
    pub attrs: Vec<Attribute>,
    pub span: Span,
    pub node: T,
}

impl<T> Named for ItemLike<T> {
    fn name(&self) -> String {
        if let Some(ref name) = self.ident {
            name.to_owned()
        } else {
            "<unnamed>".to_owned()
        }
    }
}

pub struct VisibleItemLike<T> {
    pub ident: Option<Identifier>,
    pub attrs: Vec<Attribute>,
    pub span: Span,
    pub vis: Visibility,
    pub node: T
}

impl<T> Named for VisibleItemLike<T> {
    fn name(&self) -> String {
        if let Some(ref name) = self.ident {
            name.to_owned()
        } else {
            "<unnamed>".to_owned()
        }
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Debug)]
pub struct Location {
    pub line: i32,
    pub column: i32
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Debug)]
pub struct Span {
    pub start: Location,
    pub end: Location
}

pub enum Literal {
    Int(i64),
    UInt(u64),
    Byte(u8),
    Char(char),
    Str(String),
    Boolean(bool),
    Float(f64)
}

pub struct Generics;
pub struct Visibility;
pub struct Lifetime;

pub enum Mutability {
    Mutable,
    Immutable
}

pub enum Unsafety {
    Unsafe,
    Safe
}

pub enum UnOp {
    Deref,
    Not,
    Neg
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge
}

pub enum RangeLimits {
    HalfOpen,
    Closed
}

pub type Expr = Spanned<Expr_>;
pub enum Expr_ {
    Lit(Literal),
    BinApp(BinOp, P<Expr>, P<Expr>),
    UnApp(UnOp, P<Expr>),
    Block(P<Block>),
    Match(P<Expr>, Vec<Arm>),
    IfLet(P<Pat>, P<Expr>, P<Block>, Option<P<Expr>>),
    WhileLet(P<Pat>, P<Expr>, P<Block>, Option<Spanned<Identifier>>),
    If(P<Expr>, P<Block>, Option<P<Expr>>),
    Loop(P<Block>, Option<Spanned<Identifier>>),
    While(P<Expr>, P<Block>, Option<Spanned<Identifier>>),
    For(P<Pat>, P<Expr>, P<Block>, Option<Spanned<Identifier>>),
    Range(Option<P<Expr>>, Option<P<Expr>>, RangeLimits),
    Array(Vec<P<Expr>>),
    Repeat(P<Expr>, P<Expr>),
    Struct(Path, Vec<Field>, Option<P<Expr>>),
    Tuple(Vec<P<Expr>>),
    Cast(P<Expr>, P<Type>),
    Call(P<Expr>, Vec<P<Expr>>),
    Assign(P<Expr>, P<Expr>),
    BinAppAssign(BinOp, P<Expr>, P<Expr>),
    Closure(CaptureKind, P<FnDecl>, P<Expr>),
    Field(P<Expr>, Spanned<Identifier>),
    TupleElem(P<Expr>, Spanned<usize>),
    Index(P<Expr>, P<Expr>),
    Path(Path), // missing QSelf
    AddrOf(Mutability, P<Expr>),
    Break(Option<Spanned<Identifier>>, Option<P<Expr>>),
    Continue(Option<Spanned<Identifier>>),
    Return(Option<P<Expr>>),
    Macro(Macro),
    Box(P<Expr>)
}

pub struct Arm {
    pub attrs: Vec<Attribute>,
    pub pats: Vec<P<Pat>>,
    pub guard: Option<P<Expr>>,
    pub expr: P<Expr>
}

pub enum CaptureKind {
    ByValue,
    ByRef
}

pub struct Field {

}

pub type Pat = Spanned<Pat_>;
pub enum Pat_ {
    Wildcard,
    Ident(Spanned<Identifier>, CaptureKind, Mutability, Option<P<Pat>>),
    Lit(P<Expr>),
    Struct(Path, Vec<FieldPat>, bool),
    TupleStruct(Path, Vec<P<Pat>>, Option<usize>),
    Path(Path),
    Tuple(Vec<P<Pat>>, Option<usize>),
    Box(P<Pat>),
    Ref(P<Pat>, Mutability),
    Range(P<Expr>, P<Expr>, RangeLimits),
    Slice(Vec<P<Pat>>, Option<P<Pat>>, Vec<P<Pat>>),
    Macro(Macro)
}

pub type FieldPat = ItemLike<P<Pat>>;

pub type Block = Spanned<Vec<Stmt>>;

pub type Stmt = Spanned<Stmt_>;
pub enum Stmt_ {
    Local(P<Pat>, Option<P<Type>>, Option<P<Expr>>),
    ExpStmt(P<Expr>),
    UnitStmt(P<Expr>),
    Item(P<Item>),
    Macro(P<Macro>)
}

pub enum IntType {
    ISize,
    I8,
    I16,
    I32,
    I64,
    USize,
    U8,
    U16,
    U32,
    U64,
    Unspecified
}

pub enum FloatType {
    F32,
    F64
}

pub type TypeVar = String;
pub type Type = Spanned<Type_>;
pub enum Type_ {
    Never,
    Tuple(Vec<P<Type>>),
    Array(P<Type>, P<Expr>),
    Slice(P<Type>),
    Ptr(P<Type>, Mutability),
    Ref(P<Type>, Mutability, Option<Lifetime>),
    Fun(FunctionSig),
    Path(Path),
    Var(TypeVar),
    Macro(Macro),
    Err
}

pub struct TypeParamBound;

pub struct Arg {
    pub pat: P<Pat>,
    pub ty: P<Type>
}

pub struct FnDecl {
    pub args: Vec<Arg>,
    pub return_type: P<Type>,
    pub variadic: bool
}

pub type Path = Spanned<Vec<PathSegment>>;

impl Named for Path {
    fn name(&self) -> String {
        self.node.iter()
            .map(|segment| segment.name())
            .collect::<Vec<Identifier>>()
            .join("::")
    }
}

pub type PathSegment = Spanned<PathSegment_>;
pub struct PathSegment_ {
    pub ident: Identifier,
    // pub parameters: PathParameters
}

impl Named for PathSegment {
    fn name(&self) -> String {
        self.node.ident.to_owned()
    }
}

pub struct PathUseItem {
    pub orig_ident: Identifier,
    pub renamed_ident: Identifier
}

pub enum PathUse {
    Simple(Path, Identifier),
    Glob(Path),
    List(Path, Vec<PathUseItem>)
}

pub type StructField = VisibleItemLike<P<Type>>;
pub type EnumVariant = ItemLike<VariantData>;

pub enum VariantData {
    Unit,
    Tuple(Vec<StructField>),
    Struct(Vec<StructField>)
}

pub struct FunctionSig {
    pub decl: P<FnDecl>,
    pub unsafety: Unsafety,
    pub generics: Generics,
    // pub abi: Abi
    // pub constness: Constness
}

pub type Item = VisibleItemLike<Item_>;
pub enum Item_ {
    ExternCrate(Option<Identifier>),
    Use(P<PathUse>),
    Static(P<Type>, Mutability, P<Expr>),
    Const(P<Type>, P<Expr>),
    Mod(Mod),
    TypeAlias(P<Type>, Generics),
    Enum(Vec<EnumVariant>, Generics),
    Struct(VariantData, Generics),
    Trait(Unsafety, Generics, Vec<TypeParamBound>, Vec<TraitItem>),
    Impl(Unsafety, Generics, Option<Path>, P<Type>, Vec<ImplItem>),
    Fn(FunctionSig, P<Block>),
    Macro(Macro)
}

pub type TraitItem = ItemLike<ItemMember>;
pub type ImplItem = VisibleItemLike<ItemMember>;
pub enum ItemMember {
    Const(P<Type>, Option<P<Expr>>),
    Method(FunctionSig, Option<P<Block>>),
    Type(Vec<TypeParamBound>, Option<P<Type>>),
    Macro(Macro)
}

pub struct Macro;

pub struct Attribute;

pub struct Mod {
    pub span: Span,
    pub items: Vec<P<Item>>
}

pub struct Crate {
    pub module: Mod,
    pub attrs: Vec<Attribute>,
    pub span: Span
}
