//! A lightweight GLSL implementation. Based on section the GLSL ES Spec docs:
//! https://www.khronos.org/registry/OpenGL/specs/es/3.0/GLSL_ES_Specification_3.00.pdf

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;

use crate::data::color;
use crate::system::gpu::data::buffer::item::MatrixCtx;

use code_builder::CodeBuilder;
use code_builder::HasCodeRepr;
use enso_shapely::derive_clone_plus;
use nalgebra::OMatrix;



// ===============
// === Version ===
// ===============

/// Version of the GLSL to be used. Please note that WebGL 2.0 supports only GLSL 3.00 ES, however,
/// GLSL optimizers require GLSL 3.10+. Therefore, we need to generate GLSL 3.10+ code for the
/// optimizer and GLSL 3.00 ES for WebGL.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Version {
    #[default]
    V300 = 300,
    V310 = 310,
}

impl Version {
    /// The GLSL code header.
    pub fn code(self) -> String {
        format!("#version {} es", self as usize)
    }

    /// Check whether the GLSL version requires explicit layout qualifiers on all shader's
    /// parameters.
    pub fn requires_layout(&self) -> bool {
        *self > Version::V300
    }
}



// =================================================================================================
// === Glsl ========================================================================================
// =================================================================================================

/// A GLSL code representation.
#[derive(Clone, Debug, Deref, DerefMut)]
pub struct Glsl {
    /// Raw, textual code representation.
    pub str: String,
}

impl Display for Glsl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.str, f)
    }
}


// === Conversions from Glsl ===

impls! { From  <Glsl> for String    { |t| t.str } }
impls! { From  <Glsl> for CowString { |t| t.str.into() } }
impls! { From <&Glsl> for String    { |t| t.str.clone() } }
impls! { From <&Glsl> for CowString { |t| (&t.str).into() } }


// === Self Conversions ===

impls! { From<&Glsl> for Glsl { |t| t.clone() } }


// === From String to Glsl ===

impls! { From + &From <String>    for Glsl { |t| Self {str:t.into()    } }}
impls! { From + &From <CowString> for Glsl { |t| Self {str:t.into()    } }}
impls! { From + &From <&str>      for Glsl { |t| Self {str:(*t).into() } }}


// === From Tuple to Glsl ===

impls! {[T1,T2] From<&(T1,T2)> for Glsl
    where [ T1:RefInto<Glsl>, T2:RefInto<Glsl> ] { |t| {
    let v1 = t.0.glsl();
    let v2 = t.1.glsl();
    format!("vec2({v1},{v2})").into()
}}}

impls! {[T1,T2,T3] From<&(T1,T2,T3)> for Glsl
where [ T1:RefInto<Glsl>, T2:RefInto<Glsl>, T3:RefInto<Glsl> ] { |t| {
    let v1 = t.0.glsl();
    let v2 = t.1.glsl();
    let v3 = t.2.glsl();
    format!("vec3({v1},{v2},{v3})").into()
}}}

impls! {[T1,T2,T3,T4] From<&(T1,T2,T3,T4)> for Glsl
where [ T1:RefInto<Glsl>, T2:RefInto<Glsl>, T3:RefInto<Glsl>, T4:RefInto<Glsl> ] { |t| {
    let v1 = t.0.glsl();
    let v2 = t.1.glsl();
    let v3 = t.2.glsl();
    let v4 = t.3.glsl();
    format!("vec4({v1},{v2},{v3},{v4})").into()
}}}

impls! {[T1,T2] From <(T1,T2)> for Glsl
    where [ T1:Into<Glsl>, T2:Into<Glsl> ] { |t| {
    let v1 = t.0.into();
    let v2 = t.1.into();
    format!("vec2({v1},{v2})").into()
}}}

impls! {[T1,T2,T3] From <(T1,T2,T3)> for Glsl
where [ T1:Into<Glsl>, T2:Into<Glsl>, T3:Into<Glsl> ] { |t| {
    let v1 = t.0.into();
    let v2 = t.1.into();
    let v3 = t.2.into();
    format!("vec3({v1},{v2},{v3})").into()
}}}

impls! {[T1,T2,T3,T4] From <(T1,T2,T3,T4)> for Glsl
where [ T1:Into<Glsl>, T2:Into<Glsl>, T3:Into<Glsl>, T4:Into<Glsl> ] { |t| {
    let v1 = t.0.into();
    let v2 = t.1.into();
    let v3 = t.2.into();
    let v4 = t.3.into();
    format!("vec4({v1},{v2},{v3},{v4})").into()
}}}


// === From Prim Types to Glsl ===

impls! { From + &From <bool> for Glsl { |t| t.to_string().into() } }
impls! { From + &From <i32>  for Glsl { |t| t.to_string().into() } }
impls! { From + &From <u32>  for Glsl { |t| t.to_string().into() } }
impls! { From + &From <f32>  for Glsl { |t| {
    let is_int = t.fract() == 0.0;
    if is_int { format!("{t}.0").into() }
    else      { format!("{t}").into() }
}}}

impls! { [T,R,C] From + &From <OMatrix<T,R,C>> for Glsl
    where [ T    : Into<Glsl>
          , Self : MatrixCtx<T,R,C>
          , PhantomData<OMatrix<T,R,C>> : Into<PrimType> ] {
    |t| {
        let type_name = PrimType::phantom_from::<OMatrix<T,R,C>>().to_code();
        let vals:Vec<String> = t.as_slice().iter().cloned().map(|t| {
            let t:Glsl = t.into();
            t.into()
        }).collect();
        format!("{}({})",type_name,vals.join(",")).into()
    }
}}


// === From Colors to Glsl ===

impls! { From + &From <color::Rgb> for Glsl {
    |t| format!("srgb({},{},{})", t.red.glsl(), t.green.glsl(), t.blue.glsl()).into()
} }

impls! { From + &From <color::Rgba> for Glsl {
    |t| format!("srgba({},{},{},{})", t.red.glsl(), t.green.glsl(), t.blue.glsl(), t.alpha.glsl()).into()
} }

impls! { From + &From <color::LinearRgb> for Glsl {
    |t| format!("rgb({},{},{})", t.red.glsl(), t.green.glsl(), t.blue.glsl()).into()
} }

impls! { From + &From <color::LinearRgba> for Glsl {
    |t| format!("rgba({},{},{},{})", t.red.glsl(), t.green.glsl(), t.blue.glsl(), t.alpha.glsl()).into()
} }


// === Units ===

impls! { From + &From <Pixels> for Glsl { |t| { t.value.into() } }}

impls! { From<PhantomData<Pixels>> for PrimType {
    |_|  { PhantomData::<f32>.into() }
}}

impls! { From<PhantomData<Vector2<Pixels>>> for PrimType {
    |_|  { PhantomData::<Vector2<f32>>.into() }
}}

impls! { From<PhantomData<Vector3<Pixels>>> for PrimType {
    |_|  { PhantomData::<Vector3<f32>>.into() }
}}

impls! { From<PhantomData<Vector4<Pixels>>> for PrimType {
    |_|  { PhantomData::<Vector4<f32>>.into() }
}}


impls! { From< Radians> for Glsl { |t| { f32_to_rad(&t.value.glsl()) } }}
impls! { From<&Radians> for Glsl { |t| { f32_to_rad(&t.value.glsl()) } }}
impls! { From< Degrees> for Glsl { |t| { deg_to_f32(&f32_to_deg(&t.value.glsl())) } }}
impls! { From<&Degrees> for Glsl { |t| { deg_to_f32(&f32_to_deg(&t.value.glsl())) } }}
impls! { From<PhantomData<Radians>> for PrimType {
    |_|  { "Radians".into() }
}}



// === Wrong Conversions ===

/// Error indicating that a value cannot be converted to Glsl.
#[derive(Clone, Copy, Debug)]
pub struct NotGlslError;

// === Glsl to Glsl helpers to convert between types ===

/// Converts a number to a `Radians` struct.
pub(crate) fn f32_to_rad(glsl: &Glsl) -> Glsl {
    format!("Radians({glsl})").into()
}

/// Extracts a number from a `Radians` struct.
pub(crate) fn rad_to_f32(glsl: &Glsl) -> Glsl {
    format!("value({glsl})").into()
}

/// Converts a number to a `Degree` struct.
pub(crate) fn f32_to_deg(glsl: &Glsl) -> Glsl {
    // We just use the radians representation of the degrees.
    format!("Degrees({glsl})").into()
}

/// Extracts a number from a `Degree` struct. The number will be in radians.
pub(crate) fn deg_to_f32(glsl: &Glsl) -> Glsl {
    format!("radians({glsl})").into()
}



// =================================================================================================
// === Expr ========================================================================================
// =================================================================================================

/// Any GLSL expression, like function call, or math operations.
#[derive(Clone, Debug, Deref)]
pub struct Expr(Box<ExprUnboxed>);

impl Expr {
    pub fn new<T: Into<ExprUnboxed>>(t: T) -> Self {
        Self(Box::new(Into::<ExprUnboxed>::into(t)))
    }
}

impl HasCodeRepr for Expr {
    fn build(&self, builder: &mut CodeBuilder) {
        self.deref().build(builder)
    }
}

impl From<&String> for Expr {
    fn from(t: &String) -> Self {
        Expr::new(t)
    }
}


// === ExprUnboxed ===

macro_rules! mk_expr_unboxed { ($($variant:ident),*) => {
    #[derive(Clone,Debug)]
    pub enum ExprUnboxed {
        $($variant($variant)),*
    }

    $(impl From<$variant> for ExprUnboxed {
        fn from(t: $variant) -> Self {
            ExprUnboxed::$variant(t)
        }
    })*

    $(impl From<$variant> for Expr {
        fn from(t: $variant) -> Self {
            Expr::new(t)
        }
    })*

    impl HasCodeRepr for ExprUnboxed {
        fn build(&self, builder:&mut CodeBuilder) {
            match self {
                $(ExprUnboxed::$variant(t) => t.build(builder)),*
            }
        }
    }
};}

mk_expr_unboxed!(RawCode, Identifier, Block, Assignment);

impl From<&String> for ExprUnboxed {
    fn from(t: &String) -> Self {
        Self::Identifier(t.into())
    }
}



// ===============
// === RawCode ===
// ===============

/// Raw, unchecked GLSL code.
#[derive(Clone, Debug)]
pub struct RawCode {
    pub str: String,
}

impl RawCode {
    pub fn new(str: String) -> Self {
        Self { str }
    }
}

impl HasCodeRepr for RawCode {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.write(&self.str)
    }
}



// ==================
// === Identifier ===
// ==================

/// Variable or type identifier.
#[derive(Clone, Debug, Deref, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Identifier(pub String);

impl HasCodeRepr for Identifier {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add(&self.0);
    }
}

impl From<String> for Identifier {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&String> for Identifier {
    fn from(s: &String) -> Self {
        Self(s.clone())
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Self(s.into())
    }
}

impl From<&Identifier> for String {
    fn from(t: &Identifier) -> Self {
        t.0.clone()
    }
}



// =============
// === Block ===
// =============

/// Block of expressions. Used e.g. as function body.
#[derive(Clone, Debug, Default)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

impl<T: Into<Expr>> AddMut<T> for Block {
    type Output = ();
    fn add(&mut self, t: T) {
        self.exprs.push(t.into());
    }
}

impl HasCodeRepr for Block {
    fn build(&self, builder: &mut CodeBuilder) {
        for line in &self.exprs {
            builder.newline();
            builder.add(line);
        }
    }
}



// ==================
// === Assignment ===
// ==================

/// Assignment expressiong (`a = b`).
#[derive(Clone, Debug)]
pub struct Assignment {
    pub left:  Expr,
    pub right: Expr,
}

impl Assignment {
    pub fn new<L: Into<Expr>, R: Into<Expr>>(left: L, right: R) -> Self {
        Self { left: left.into(), right: right.into() }
    }
}

impl HasCodeRepr for Assignment {
    fn build(&self, builder: &mut CodeBuilder) {
        self.left.build(builder);
        builder.add("=");
        builder.add(&self.right);
        builder.terminator();
    }
}



// =================================================================================================
// === Statement ===================================================================================
// =================================================================================================

/// Top-level statement, like function declaration.
#[derive(Clone, Debug)]
pub enum Statement {
    Function(Function),
    PrecisionDecl(PrecisionDecl),
    Raw(RawCode),
}

impl HasCodeRepr for Statement {
    fn build(&self, builder: &mut CodeBuilder) {
        match self {
            Self::Function(t) => builder.add(t),
            Self::PrecisionDecl(t) => builder.add(t),
            Self::Raw(t) => builder.add(t),
        };
    }
}

impl From<PrecisionDecl> for Statement {
    fn from(t: PrecisionDecl) -> Self {
        Self::PrecisionDecl(t)
    }
}



// ================
// === Function ===
// ================

/// Top-level function declaration.
#[derive(Clone, Debug)]
pub struct Function {
    pub typ:   Type,
    pub ident: Identifier,
    pub body:  Block,
}

impl HasCodeRepr for Function {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add(&self.typ).add(&self.ident).add("() {");
        builder.inc_indent();
        builder.add(&self.body);
        builder.dec_indent();
        builder.newline();
        builder.add("}");
    }
}

impl<T: Into<Expr>> AddMut<T> for Function {
    type Output = ();
    fn add(&mut self, t: T) {
        self.body.add(t)
    }
}



// =====================
// === PrecisionDecl ===
// =====================

/// Top-level type precision declaration.
#[derive(Clone, Debug)]
pub struct PrecisionDecl {
    pub prec: Precision,
    pub typ:  Type,
}

impl PrecisionDecl {
    pub fn new<P: Into<Precision>, T: Into<Type>>(prec: P, typ: T) -> Self {
        Self { prec: prec.into(), typ: typ.into() }
    }
}

impl HasCodeRepr for PrecisionDecl {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add("precision");
        builder.add(&self.prec);
        builder.add(&self.typ);
        builder.terminator();
    }
}



// =================================================================================================
// === AST Elements ================================================================================
// =================================================================================================

// ============
// === Type ===
// ============

/// Abstraction for any GLSL type, including array types.
#[derive(Clone, Debug)]
pub struct Type {
    pub prim:  PrimType,
    pub array: Option<usize>,
}

impl Type {
    /// The layout size of this type.
    pub fn layout_size(&self) -> usize {
        self.prim.layout_size() * self.array.unwrap_or(1)
    }
}

impl From<PrimType> for Type {
    fn from(prim: PrimType) -> Self {
        let array = None;
        Self { prim, array }
    }
}

impl HasCodeRepr for Type {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add(&self.prim).add(&self.array);
    }
}

derive_clone_plus!(Type);

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_code())
    }
}



// ================
// === PrimType ===
// ================

macro_rules! define_prim_type {
    ($($variant:ident { name = $name:literal, layout_size = $layout_size:literal }),* $(,)?) => {
        /// Any non-array GLSL type.
        #[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
        pub enum PrimType {
            $($variant,)*
            Struct(Identifier),
        }

        impl PrimType {
            /// The GLSL name of this type.
            pub fn glsl_name(&self) -> &str {
                match self {
                    $(Self::$variant => $name,)*
                    Self::Struct(ident) => &ident,
                }
            }

            /// The layout size of this type.
            pub fn layout_size(&self) -> usize {
                match self {
                    $(Self::$variant => $layout_size,)*
                    Self::Struct(_) => 0,
                }
            }
        }
    };
}

// If `layout_size` is `0`, then this type can't be used as attribute, and thus, it does not
// contribute to the attribute layout.
define_prim_type! {
    Float { name = "float", layout_size = 1 },
    Int { name = "int", layout_size = 1 },
    Void { name = "void", layout_size = 0 },
    Bool { name = "bool", layout_size = 1 },
    Mat2 { name = "mat2", layout_size = 2 },
    Mat3 { name = "mat3", layout_size = 3 },
    Mat4 { name = "mat4", layout_size = 4 },
    Mat2x2 { name = "mat2x2", layout_size = 2 },
    Mat2x3 { name = "mat2x3", layout_size = 2 },
    Mat2x4 { name = "mat2x4", layout_size = 2 },
    Mat3x2 { name = "mat3x2", layout_size = 3 },
    Mat3x3 { name = "mat3x3", layout_size = 3 },
    Mat3x4 { name = "mat3x4", layout_size = 3 },
    Mat4x2 { name = "mat4x2", layout_size = 4 },
    Mat4x3 { name = "mat4x3", layout_size = 4 },
    Mat4x4 { name = "mat4x4", layout_size = 4 },
    Vec2 { name = "vec2", layout_size = 1 },
    Vec3 { name = "vec3", layout_size = 1 },
    Vec4 { name = "vec4", layout_size = 1 },
    IVec2 { name = "ivec2", layout_size = 1 },
    IVec3 { name = "ivec3", layout_size = 1 },
    IVec4 { name = "ivec4", layout_size = 1 },
    BVec2 { name = "bvec2", layout_size = 1 },
    BVec3 { name = "bvec3", layout_size = 1 },
    BVec4 { name = "bvec4", layout_size = 1 },
    UInt { name = "int", layout_size = 1 },
    UVec2 { name = "uvec2", layout_size = 1 },
    UVec3 { name = "uvec3", layout_size = 1 },
    UVec4 { name = "uvec4", layout_size = 1 },
    Sampler2d { name = "sampler2D", layout_size = 0 },
    Sampler3d { name = "sampler3D", layout_size = 0 },
    SamplerCube { name = "samplerCube", layout_size = 0 },
    Sampler2dShadow { name = "sampler2DShadow", layout_size = 0 },
    SamplerCubeShadow { name = "samplerCubeShadow", layout_size = 0 },
    Sampler2dArray { name = "sampler2DArray", layout_size = 0 },
    Sampler2dArrayShadow { name = "sampler2DArrayShadow", layout_size = 0 },
    ISampler2d { name = "isampler2D", layout_size = 0 },
    ISampler3d { name = "isampler3D", layout_size = 0 },
    ISamplerCube { name = "isamplerCube", layout_size = 0 },
    ISampler2dArray { name = "isampler2DArray", layout_size = 0 },
    USampler2d { name = "usampler2D", layout_size = 0 },
    USampler3d { name = "usampler3D", layout_size = 0 },
    USamplerCube { name = "usamplerCube", layout_size = 0 },
    USampler2dArray { name = "usampler2DArray", layout_size = 0 },
}

impl HasCodeRepr for PrimType {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add(self.glsl_name());
    }
}

impl From<&str> for PrimType {
    fn from(s: &str) -> Self {
        Self::Struct(s.into())
    }
}

impl From<PrimType> for String {
    fn from(t: PrimType) -> Self {
        t.to_code()
    }
}

impl Display for PrimType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_code())
    }
}



// =================
// === GlobalVar ===
// =================

/// Global variable declaration, including attributes and uniforms.
#[derive(Clone, Debug)]
pub struct GlobalVar {
    pub layout:  Option<Layout>,
    pub storage: Option<GlobalVarStorage>,
    pub prec:    Option<Precision>,
    pub typ:     Type,
    pub ident:   Identifier,
}

/// Global variable layout definition.
#[derive(Clone, Copy, Debug, Default)]
pub struct Layout {
    pub location: usize,
}

/// Global variable storage definition.
#[derive(Clone, Copy, Debug)]
pub enum GlobalVarStorage {
    ConstStorage,
    InStorage(LinkageStorage),
    OutStorage(LinkageStorage),
    UniformStorage,
}

/// Storage definition for in- and out- attributes.
#[derive(Clone, Copy, Debug, Default)]
pub struct LinkageStorage {
    pub centroid:      bool,
    pub interpolation: Option<InterpolationStorage>,
}

/// Interpolation storage type for attributes.
#[derive(Clone, Copy, Debug)]
pub enum InterpolationStorage {
    Smooth,
    Flat,
}


// === Printers ===

impl HasCodeRepr for Layout {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add_spaced("layout(location=");
        builder.add(&self.location);
        builder.add_spaced(")");
    }
}

impl HasCodeRepr for InterpolationStorage {
    fn build(&self, builder: &mut CodeBuilder) {
        match self {
            Self::Smooth => builder.add("smooth"),
            Self::Flat => builder.add("flat"),
        };
    }
}

impl HasCodeRepr for LinkageStorage {
    fn build(&self, builder: &mut CodeBuilder) {
        if self.centroid {
            builder.add("centroid");
        };
        builder.add(&self.interpolation);
    }
}

impl HasCodeRepr for GlobalVarStorage {
    fn build(&self, builder: &mut CodeBuilder) {
        match self {
            Self::ConstStorage => builder.add("const"),
            Self::UniformStorage => builder.add("uniform"),
            Self::InStorage(qual) => builder.add(qual).add("in"),
            Self::OutStorage(qual) => builder.add(qual).add("out"),
        };
    }
}

impl HasCodeRepr for GlobalVar {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add(&self.layout).add(&self.storage).add(&self.typ).add(&self.ident);
    }
}



// ================
// === LocalVar ===
// ================

/// Local variable definition.
#[derive(Clone, Debug)]
pub struct LocalVar {
    pub constant: bool,
    pub typ:      Type,
    pub ident:    Identifier,
}

impl HasCodeRepr for LocalVar {
    fn build(&self, builder: &mut CodeBuilder) {
        if self.constant {
            builder.add("const");
        }
        builder.add(&self.typ).add(&self.ident);
    }
}



// =================
// === Precision ===
// =================

/// Type precision definition.
#[derive(Clone, Copy, Debug)]
pub enum Precision {
    Low,
    Medium,
    High,
}

impl Display for Precision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prec = match self {
            Self::Low => "lowp",
            Self::Medium => "mediump",
            Self::High => "highp",
        };
        write!(f, "{}", prec)
    }
}

impl HasCodeRepr for Precision {
    fn build(&self, builder: &mut CodeBuilder) {
        let str = match self {
            Self::Low => "lowp",
            Self::Medium => "mediump",
            Self::High => "highp",
        };
        builder.add(str);
    }
}

impl From<&Precision> for Precision {
    fn from(t: &Precision) -> Self {
        *t
    }
}



// =================================================================================================
// === Module ======================================================================================
// =================================================================================================

/// Translation unit definition. It represents the whole GLSL file.
#[derive(Clone, Debug)]
pub struct Module {
    pub version:     Version,
    pub prec_decls:  Vec<PrecisionDecl>,
    pub global_vars: Vec<GlobalVar>,
    pub statements:  Vec<Statement>,
    pub main:        Function,
}

impl Module {
    /// Constructor.
    pub fn new(version: Version) -> Self {
        let prec_decls = default();
        let global_vars = default();
        let statements = default();
        let main =
            Function { typ: PrimType::Void.into(), ident: "main".into(), body: default() };
        Self { version, prec_decls, global_vars, statements, main }
    }
}

impl AddMut<GlobalVar> for Module {
    type Output = ();
    fn add(&mut self, t: GlobalVar) {
        self.global_vars.push(t);
    }
}

impl AddMut<Statement> for Module {
    type Output = ();
    fn add(&mut self, t: Statement) {
        self.statements.push(t);
    }
}

impl AddMut<PrecisionDecl> for Module {
    type Output = ();
    fn add(&mut self, t: PrecisionDecl) {
        self.prec_decls.push(t);
    }
}

impl AddMut<Expr> for Module {
    type Output = ();
    fn add(&mut self, t: Expr) {
        self.main.add(t);
    }
}

impl HasCodeRepr for Module {
    fn build(&self, builder: &mut CodeBuilder) {
        builder.add(&self.version.code());
        builder.newline();
        builder.newline();

        for t in &self.prec_decls {
            builder.add(t);
            builder.newline();
        }
        builder.newline();

        for t in &self.global_vars {
            builder.add(t);
            builder.terminator();
            builder.newline();
        }
        builder.newline();

        for t in &self.statements {
            builder.add(t);
            builder.newline();
        }
        builder.add(&self.main);
    }
}


// ============================
// === PrimType Conversions ===
// ============================

macro_rules! define_glsl_prim_type_conversions {
    ($($ty:ty => $name:ident),* $(,)?) => {$(
        impl From<PhantomData<$ty>> for PrimType {
            fn from(_:PhantomData<$ty>) -> Self {
                Self::$name
            }
        }

        impl From<PhantomData<$ty>> for Type {
            fn from(_:PhantomData<$ty>) -> Self {
                PrimType::$name.into()
            }
        }
    )*}
}

define_glsl_prim_type_conversions! {
    bool           => Bool,
    i32            => Int,
    u32            => UInt,
    f32            => Float,

    Vector2<f32>   => Vec2,
    Vector3<f32>   => Vec3,
    Vector4<f32>   => Vec4,

    Vector2<i32>   => IVec2,
    Vector3<i32>   => IVec3,
    Vector4<i32>   => IVec4,

    Vector2<u32>   => UVec2,
    Vector3<u32>   => UVec3,
    Vector4<u32>   => UVec4,

    Vector2<bool>  => BVec2,
    Vector3<bool>  => BVec3,
    Vector4<bool>  => BVec4,

    Matrix2<f32>   => Mat2,
    Matrix3<f32>   => Mat3,
    Matrix4<f32>   => Mat4,

    Matrix2x3<f32> => Mat2x3,
    Matrix2x4<f32> => Mat2x4,
    Matrix3x2<f32> => Mat3x2,
    Matrix3x4<f32> => Mat3x4,
    Matrix4x2<f32> => Mat4x2,
    Matrix4x3<f32> => Mat4x3,
}


// === Smart accessors ===

/// Extension methods.
pub mod traits {
    use super::*;

    /// Extension methods for every type which could be converted to `PrimType`.
    pub trait PhantomIntoPrimType: Sized + PhantomInto<PrimType> {
        /// `PrimType` representation of the current type.
        fn glsl_prim_type() -> PrimType {
            Self::phantom_into()
        }
    }
    impl<T: PhantomInto<PrimType>> PhantomIntoPrimType for T {}

    /// Extension methods for every type which could be converted to `Type`.
    pub trait PhantomIntoType: Sized + PhantomInto<Type> {
        /// `PrimType` representation of the current type.
        fn glsl_type() -> Type {
            Self::phantom_into()
        }
    }
    impl<T: PhantomInto<Type>> PhantomIntoType for T {}

    pub trait IntoGlsl<'a>
    where
        Self: 'a,
        &'a Self: Into<Glsl>, {
        fn glsl(&'a self) -> Glsl {
            self.into()
        }
    }
    impl<'a, T> IntoGlsl<'a> for T
    where
        T: 'a,
        &'a T: Into<Glsl>,
    {
    }
}
pub use traits::*;
