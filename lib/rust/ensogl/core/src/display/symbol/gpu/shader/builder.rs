// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;

use crate::system::gpu::shader;
use crate::system::gpu::shader::glsl;

use code_builder::HasCodeRepr;
use std::collections::BTreeMap;



// ==============================
// === Shader Parameter Types ===
// ==============================

/// Shader attribute marker type.
#[derive(Clone, Copy, Debug, Default)]
pub struct Attribute;

/// Shader uniform marker type.
#[derive(Clone, Copy, Debug, Default)]
pub struct Uniform;



// ==========================
// === ScopeLayoutManager ===
// ==========================

/// A struct that tracks which layout slots were used per scope types. This is used to generate GLSL
/// code annotated with explicit uniforms and attributes locations.
///
/// Please note that WebGL is using GLSL 300 ES, which does not allow for explicit uniform locations
/// and requires explicit fragment output attributes locations. However, GLSL optimizer (`shaderc`)
/// supports GLSL 310+ and requires explicit layout location of all inputs. The code generated with
/// all locations is used by the optimizer only and the original body is replaced with the optimized
/// one in the end.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct LayoutManager {
    pub uniforms: ScopeLayoutManager<Uniform>,
    pub vert_in:  ScopeLayoutManager<Attribute>,
    pub vert_out: ScopeLayoutManager<Attribute>,
    pub frag_in:  ScopeLayoutManager<Attribute>,
    pub frag_out: ScopeLayoutManager<Attribute>,
}

/// Configuration of [`LayoutManager`]. Used to decide which shader parameters should be generated
/// with explicit location information.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct LayoutEnabledFor {
    pub uniforms: bool,
    pub vert_in:  bool,
    pub vert_out: bool,
    pub frag_in:  bool,
    pub frag_out: bool,
}

impl LayoutManager {
    /// Constructor. The argument defines whether the locations should be used.
    pub fn new(enabled: LayoutEnabledFor) -> Self {
        Self {
            uniforms: ScopeLayoutManager::new(enabled.uniforms),
            vert_in:  ScopeLayoutManager::new(enabled.vert_in),
            vert_out: ScopeLayoutManager::new(enabled.vert_out),
            frag_in:  ScopeLayoutManager::new(enabled.frag_in),
            frag_out: ScopeLayoutManager::new(enabled.frag_out),
        }
    }
}

/// A struct that tracks which layout slots were used per scope types. Internal representation of
/// [`LayoutManager`].
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub struct ScopeLayoutManager<T> {
    location: Option<usize>,
    tp:       ZST<T>,
}

impl<T> ScopeLayoutManager<T> {
    /// Constructor.
    pub fn new(enabled: bool) -> Self {
        Self { location: enabled.as_some(0), tp: default() }
    }
}

/// Find the first free location for the given attribute type.
#[allow(missing_docs)]
pub trait ScopeLayoutManagerAttributePlacement {
    fn place(&mut self, tp: &glsl::Type) -> Option<glsl::Layout>;
}

impl ScopeLayoutManagerAttributePlacement for ScopeLayoutManager<Attribute> {
    fn place(&mut self, tp: &glsl::Type) -> Option<glsl::Layout> {
        let location = self.location?;
        self.location = Some(location + tp.layout_size());
        Some(glsl::Layout { location })
    }
}

/// Find the first free location for a uniform. No matter the type, uniforms always take a
/// single location slot.
#[allow(missing_docs)]
pub trait ScopeLayoutManagerUniformPlacement {
    fn place(&mut self) -> Option<glsl::Layout>;
}

impl ScopeLayoutManagerUniformPlacement for ScopeLayoutManager<Uniform> {
    fn place(&mut self) -> Option<glsl::Layout> {
        let location = self.location?;
        self.location = Some(location + 1);
        Some(glsl::Layout { location })
    }
}



// ====================
// === ShaderConfig ===
// ====================

#[derive(Clone, Debug)]
pub struct ShaderConfig {
    pub glsl_version: glsl::Version,
    pub precision:    ShaderPrecision,
    pub outputs:      BTreeMap<String, AttributeQualifier>,
    pub shared:       BTreeMap<String, AttributeQualifier>,
    pub attributes:   BTreeMap<String, AttributeQualifier>,
    pub uniforms:     BTreeMap<String, UniformQualifier>,
}

impl ShaderConfig {
    pub fn new(glsl_version: glsl::Version) -> Self {
        let precision = default();
        let outputs = default();
        let shared = default();
        let attributes = default();
        let uniforms = default();
        Self { glsl_version, precision, outputs, shared, attributes, uniforms }
    }

    pub fn add_attribute<S: Str, Q: Into<AttributeQualifier>>(&mut self, name: S, qual: Q) {
        self.attributes.insert(name.as_ref().to_string(), qual.into());
    }

    pub fn add_shared_attribute<S: Str, Q: Into<AttributeQualifier>>(&mut self, name: S, qual: Q) {
        self.shared.insert(name.as_ref().to_string(), qual.into());
    }

    pub fn add_uniform<S: Str, Q: Into<UniformQualifier>>(&mut self, name: S, qual: Q) {
        self.uniforms.insert(name.as_ref().to_string(), qual.into());
    }

    pub fn add_output<S: Str, Q: Into<AttributeQualifier>>(&mut self, name: S, qual: Q) {
        self.outputs.insert(name.as_ref().to_string(), qual.into());
    }
}


// === ShaderPrecision ===

#[derive(Clone, Debug)]
pub struct ShaderPrecision {
    pub vertex:   BTreeMap<glsl::PrimType, glsl::Precision>,
    pub fragment: BTreeMap<glsl::PrimType, glsl::Precision>,
}

impl Default for ShaderPrecision {
    fn default() -> Self {
        let mut map = BTreeMap::new();
        map.insert(glsl::PrimType::Int, glsl::Precision::High);
        map.insert(glsl::PrimType::Float, glsl::Precision::High);
        map.insert(glsl::PrimType::Sampler2dArray, glsl::Precision::Medium);
        let vertex = map.clone();
        let fragment = map;
        Self { vertex, fragment }
    }
}


// === LocalVarQualifier ===

#[derive(Clone, Debug)]
pub struct LocalVarQualifier {
    pub constant: bool,
    pub typ:      glsl::Type,
}

impl LocalVarQualifier {
    pub fn to_var<Name: Into<glsl::Identifier>>(&self, name: Name) -> glsl::LocalVar {
        glsl::LocalVar {
            constant: self.constant,
            typ:      self.typ.clone(),
            ident:    name.into(),
        }
    }
}


// === AttributeQualifier ===

#[derive(Clone, Debug)]
pub struct AttributeQualifier {
    pub storage: glsl::LinkageStorage,
    pub prec:    Option<glsl::Precision>,
    pub typ:     glsl::Type,
}

impl AttributeQualifier {
    /// Convert the qualifier to input variable definition. The [`use_qual`] parameter defines if
    /// the qualified storage interpolator (e.g. `flat`) should be used. Interpolation modifiers are
    /// illegal on vertex shader input attributes, however, they are required on fragment shader
    /// ones.
    pub fn to_input_var<Name: Into<glsl::Identifier>>(
        &self,
        name: Name,
        layout_manager: &mut ScopeLayoutManager<Attribute>,
        use_qual: bool,
    ) -> glsl::GlobalVar {
        let layout = layout_manager.place(&self.typ);
        let mut storage = self.storage;
        if !use_qual {
            storage.interpolation = None;
        }
        glsl::GlobalVar {
            layout,
            storage: Some(glsl::GlobalVarStorage::InStorage(storage)),
            prec: self.prec,
            typ: self.typ.clone(),
            ident: name.into(),
        }
    }

    pub fn to_output_var<Name: Into<glsl::Identifier>>(
        &self,
        name: Name,
        layout_manager: &mut ScopeLayoutManager<Attribute>,
    ) -> glsl::GlobalVar {
        let layout = layout_manager.place(&self.typ);
        let storage = self.storage;
        glsl::GlobalVar {
            layout,
            storage: Some(glsl::GlobalVarStorage::OutStorage(storage)),
            prec: self.prec,
            typ: self.typ.clone(),
            ident: name.into(),
        }
    }
}

impl From<glsl::Type> for AttributeQualifier {
    fn from(typ: glsl::Type) -> Self {
        let prec = default();
        let prim = &typ.prim;
        let storage = match prim {
            glsl::PrimType::Int
            | glsl::PrimType::IVec2
            | glsl::PrimType::IVec3
            | glsl::PrimType::IVec4
            | glsl::PrimType::UInt
            | glsl::PrimType::UVec2
            | glsl::PrimType::UVec3
            | glsl::PrimType::UVec4 => glsl::LinkageStorage {
                interpolation: Some(glsl::InterpolationStorage::Flat),
                ..default()
            },
            _ => default(),
        };
        Self { storage, prec, typ }
    }
}

impl From<glsl::PrimType> for AttributeQualifier {
    fn from(prim_type: glsl::PrimType) -> Self {
        let typ: glsl::Type = prim_type.into();
        typ.into()
    }
}

impl From<&glsl::PrimType> for AttributeQualifier {
    fn from(prim_type: &glsl::PrimType) -> Self {
        let typ: glsl::Type = prim_type.into();
        typ.into()
    }
}


// === UniformQualifier ===

#[derive(Clone, Debug)]
pub struct UniformQualifier {
    pub prec: Option<glsl::Precision>,
    pub typ:  glsl::Type,
}

impl UniformQualifier {
    pub fn to_var<Name: Into<glsl::Identifier>>(
        &self,
        name: Name,
        layout_manager: &mut ScopeLayoutManager<Uniform>,
    ) -> glsl::GlobalVar {
        let layout = layout_manager.place();
        glsl::GlobalVar {
            layout,
            storage: Some(glsl::GlobalVarStorage::UniformStorage),
            prec: self.prec,
            typ: self.typ.clone(),
            ident: name.into(),
        }
    }
}

impl From<glsl::Type> for UniformQualifier {
    fn from(typ: glsl::Type) -> Self {
        let prec = default();
        Self { prec, typ }
    }
}

impl From<glsl::PrimType> for UniformQualifier {
    fn from(prim_type: glsl::PrimType) -> Self {
        let typ: glsl::Type = prim_type.into();
        typ.into()
    }
}

impl From<&glsl::PrimType> for UniformQualifier {
    fn from(prim_type: &glsl::PrimType) -> Self {
        let typ: glsl::Type = prim_type.into();
        typ.into()
    }
}



// ====================
// === CodeTemplate ===
// ====================

/// A GLSL code template. It is used to provide a pre-defined GLSL code chunk and insert generated
/// GLSL snippets in right places.
#[derive(Clone, Debug, Default)]
pub struct CodeTemplate {
    before_main: String,
    main:        String,
    after_main:  String,
}

impl CodeTemplate {
    /// Constructor.
    pub fn new(before_main: impl Str, main: impl Str, after_main: impl Str) -> Self {
        let before_main = before_main.into();
        let main = main.into();
        let after_main = after_main.into();
        Self { before_main, main, after_main }
    }

    /// Creates a new instance from the provided main GLSL code definition.
    pub fn from_main<S: Str>(main: S) -> Self {
        Self { main: main.as_ref().to_string(), ..default() }
    }
}


// === Getters ===

impl CodeTemplate {
    pub fn before_main(&self) -> &String {
        &self.before_main
    }

    pub fn main(&self) -> &String {
        &self.main
    }

    pub fn after_main(&self) -> &String {
        &self.after_main
    }
}


// === Setters ===

impl CodeTemplate {
    pub fn set_before_main<S: Str>(&mut self, value: S) {
        self.before_main = value.into();
    }

    pub fn set_main<S: Str>(&mut self, value: S) {
        self.main = value.into();
    }

    pub fn set_after_main<S: Str>(&mut self, value: S) {
        self.after_main = value.into();
    }
}



// =====================
// === ShaderBuilder ===
// =====================

#[derive(Clone, Debug)]
pub struct ShaderBuilder {
    pub layout:   LayoutManager,
    pub vertex:   glsl::Module,
    pub fragment: glsl::Module,
}

impl ShaderBuilder {
    pub fn new(glsl_version: glsl::Version) -> Self {
        let layout_required = glsl_version.requires_layout();
        let layout = LayoutManager::new(LayoutEnabledFor {
            uniforms: layout_required,
            vert_in:  layout_required,
            vert_out: layout_required,
            frag_in:  layout_required,
            frag_out: true,
        });
        let vertex = glsl::Module::new(glsl_version);
        let fragment = glsl::Module::new(glsl_version);
        Self { layout, vertex, fragment }
    }

    pub fn compute(
        &mut self,
        cfg: &ShaderConfig,
        vertex_code: CodeTemplate,
        fragment_code: CodeTemplate,
    ) {
        self.gen_precision_code(cfg);
        self.gen_attributes_code(cfg);
        self.gen_shared_attributes_code(cfg);
        self.gen_uniforms_code(cfg);
        self.gen_outputs_code(cfg);
        self.add_template_code(vertex_code, fragment_code);
    }

    fn add_template_code(&mut self, vertex_code: CodeTemplate, fragment_code: CodeTemplate) {
        self.vertex.main.body.add(&vertex_code.main);
        self.vertex.add(glsl::Statement::Raw(glsl::RawCode::new(vertex_code.before_main)));

        self.fragment.main.body.add(&fragment_code.main);
        self.fragment.add(glsl::Statement::Raw(glsl::RawCode::new(fragment_code.before_main)));
    }

    fn gen_precision_code(&mut self, cfg: &ShaderConfig) {
        for (typ, prec) in &cfg.precision.vertex {
            self.vertex.add(glsl::PrecisionDecl::new(prec, typ));
        }
        for (typ, prec) in &cfg.precision.fragment {
            self.fragment.add(glsl::PrecisionDecl::new(prec, typ));
        }
    }

    fn gen_attributes_code(&mut self, cfg: &ShaderConfig) {
        if !cfg.attributes.is_empty() {
            for (name, qual) in &cfg.attributes {
                let vert_name = mk_vertex_name(name);
                let frag_name = mk_fragment_name(name);
                let sharing = glsl::Assignment::new(&frag_name, &vert_name);
                self.vertex.add(qual.to_input_var(&vert_name, &mut self.layout.vert_in, false));
                self.vertex.add(qual.to_output_var(&frag_name, &mut self.layout.vert_out));
                self.fragment.add(qual.to_input_var(&frag_name, &mut self.layout.frag_in, true));
                self.vertex.main.add(sharing);
            }
        }
    }

    fn gen_shared_attributes_code(&mut self, cfg: &ShaderConfig) {
        if !cfg.shared.is_empty() {
            for (name, qual) in &cfg.shared {
                let frag_name = mk_fragment_name(name);
                self.vertex.add(qual.to_output_var(&frag_name, &mut self.layout.vert_out));
                self.fragment.add(qual.to_input_var(&frag_name, &mut self.layout.frag_in, true));
            }
        }
    }

    fn gen_uniforms_code(&mut self, cfg: &ShaderConfig) {
        if !cfg.uniforms.is_empty() {
            for (name, qual) in &cfg.uniforms {
                let name = mk_uniform_name(name);
                let var = qual.to_var(&name, &mut self.layout.uniforms);
                self.vertex.add(var.clone());
                self.fragment.add(var);
            }
        }
    }

    fn gen_outputs_code(&mut self, cfg: &ShaderConfig) {
        for (name, qual) in &cfg.outputs {
            let name = mk_out_name(name);
            self.fragment.add(qual.to_output_var(name, &mut self.layout.frag_out));
        }
    }

    pub fn build(&self) -> shader::Code {
        let vertex = self.vertex.to_code();
        let fragment = self.fragment.to_code();
        shader::Code { vertex, fragment }
    }
}

pub const INPUT_PREFIX: &str = "input_";
pub const OUTPUT_PREFIX: &str = "output_";
pub const VERTEX_PREFIX: &str = "vertex_";

pub fn mk_out_name<S: Str>(s: S) -> String {
    format!("{OUTPUT_PREFIX}{}", s.as_ref())
}
pub fn mk_vertex_name<S: Str>(s: S) -> String {
    format!("{VERTEX_PREFIX}{}", s.as_ref())
}
pub fn mk_fragment_name<S: Str>(s: S) -> String {
    format!("{INPUT_PREFIX}{}", s.as_ref())
}
pub fn mk_uniform_name<S: Str>(s: S) -> String {
    format!("{INPUT_PREFIX}{}", s.as_ref())
}
