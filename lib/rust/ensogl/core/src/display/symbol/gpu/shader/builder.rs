// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;

use crate::system::gpu::shader::glsl;

use code_builder::HasCodeRepr;
use std::collections::BTreeMap;



// ==============
// === Shader ===
// ==============

#[derive(Clone, Debug)]
pub struct Shader {
    pub vertex:   String,
    pub fragment: String,
}



// ====================
// === ShaderConfig ===
// ====================

#[derive(Clone, Debug, Default)]
pub struct ShaderConfig {
    pub precision:  ShaderPrecision,
    pub outputs:    BTreeMap<String, AttributeQualifier>,
    pub shared:     BTreeMap<String, AttributeQualifier>,
    pub attributes: BTreeMap<String, AttributeQualifier>,
    pub uniforms:   BTreeMap<String, UniformQualifier>,
}

impl ShaderConfig {
    pub fn new() -> Self {
        default()
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
        map.insert(glsl::PrimType::UInt, glsl::Precision::High);
        map.insert(glsl::PrimType::Float, glsl::Precision::High);
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
        use_qual: bool,
    ) -> glsl::GlobalVar {
        let mut storage = self.storage;
        if !use_qual {
            storage.interpolation = None;
        }
        glsl::GlobalVar {
            layout:  None,
            storage: Some(glsl::GlobalVarStorage::InStorage(storage)),
            prec:    self.prec,
            typ:     self.typ.clone(),
            ident:   name.into(),
        }
    }

    pub fn to_output_var<Name: Into<glsl::Identifier>>(&self, name: Name) -> glsl::GlobalVar {
        let storage = self.storage;
        glsl::GlobalVar {
            layout:  None,
            storage: Some(glsl::GlobalVarStorage::OutStorage(storage)),
            prec:    self.prec,
            typ:     self.typ.clone(),
            ident:   name.into(),
        }
    }
}

impl From<glsl::Type> for AttributeQualifier {
    fn from(typ: glsl::Type) -> Self {
        let prec = default();
        let prim = &typ.prim;
        let storage = match prim {
            glsl::PrimType::Int => glsl::LinkageStorage {
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
    pub fn to_var<Name: Into<glsl::Identifier>>(&self, name: Name) -> glsl::GlobalVar {
        glsl::GlobalVar {
            layout:  None,
            storage: Some(glsl::GlobalVarStorage::UniformStorage),
            prec:    self.prec,
            typ:     self.typ.clone(),
            ident:   name.into(),
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

#[derive(Clone, Debug, Default)]
pub struct ShaderBuilder {
    pub vertex:   glsl::Module,
    pub fragment: glsl::Module,
}

impl ShaderBuilder {
    pub fn new() -> Self {
        default()
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
                self.vertex.add(qual.to_input_var(&vert_name, false));
                self.vertex.add(qual.to_output_var(&frag_name));
                self.fragment.add(qual.to_input_var(&frag_name, true));
                self.vertex.main.add(sharing);
            }
        }
    }

    fn gen_shared_attributes_code(&mut self, cfg: &ShaderConfig) {
        if !cfg.shared.is_empty() {
            for (name, qual) in &cfg.shared {
                let frag_name = mk_fragment_name(name);
                self.vertex.add(qual.to_output_var(&frag_name));
                self.fragment.add(qual.to_input_var(&frag_name, true));
            }
        }
    }

    fn gen_uniforms_code(&mut self, cfg: &ShaderConfig) {
        if !cfg.uniforms.is_empty() {
            for (name, qual) in &cfg.uniforms {
                let name = mk_uniform_name(name);
                self.vertex.add(qual.to_var(&name));
                self.fragment.add(qual.to_var(&name));
            }
        }
    }

    fn gen_outputs_code(&mut self, cfg: &ShaderConfig) {
        if !cfg.outputs.is_empty() {
            cfg.outputs.iter().enumerate().for_each(|(loc, (name, qual))| {
                let name = mk_out_name(name);
                let mut var = qual.to_output_var(name);
                var.layout = Some(glsl::Layout { location: loc });
                self.fragment.add(var);
            });
        }
    }

    pub fn build(&self) -> Shader {
        let vertex = self.vertex.to_code();
        let fragment = self.fragment.to_code();
        Shader { vertex, fragment }
    }
}

pub fn mk_out_name<S: Str>(s: S) -> String {
    format!("output_{}", s.as_ref())
}
pub fn mk_vertex_name<S: Str>(s: S) -> String {
    format!("vertex_{}", s.as_ref())
}
pub fn mk_fragment_name<S: Str>(s: S) -> String {
    format!("input_{}", s.as_ref())
}
pub fn mk_uniform_name<S: Str>(s: S) -> String {
    format!("input_{}", s.as_ref())
}
