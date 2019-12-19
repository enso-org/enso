use crate::prelude::*;

use crate::data::container::Add;
use super::glsl;
use code_builder::HasCodeRepr;

// ==============
// === Shader ===
// ==============

#[derive(Clone,Debug)]
pub struct Shader {
    pub vertex   : String,
    pub fragment : String,
}


// ====================
// === ShaderConfig ===
// ====================

#[derive(Clone,Debug,Default)]
pub struct ShaderConfig {
    pub precision  : ShaderPrecision,
    pub outputs    : HashMap<String,AttributeQualifier>,
    pub shared     : HashMap<String,AttributeQualifier>,
    pub attributes : HashMap<String,AttributeQualifier>,
    pub uniforms   : HashMap<String,UniformQualifier>,
}

impl ShaderConfig {
    pub fn new() -> Self { default() }
}

// === ShaderPrecision ===

#[derive(Clone,Debug)]
pub struct ShaderPrecision {
    pub vertex   : HashMap<glsl::PrimType,glsl::Precision>,
    pub fragment : HashMap<glsl::PrimType,glsl::Precision>,
}

impl Default for ShaderPrecision {
    fn default() -> Self {
        let mut map = HashMap::new();
        map.insert(glsl::PrimType::Int   , glsl::Precision::Medium);
        map.insert(glsl::PrimType::Float , glsl::Precision::Medium);
        let vertex   = map.clone();
        let fragment = map;
        Self {vertex,fragment}
    }
}

// === LocalVarQualifier ===

#[derive(Clone,Debug)]
pub struct LocalVarQualifier {
    pub constant : bool,
    pub typ      : glsl::Type,
}

impl LocalVarQualifier {
    pub fn to_var<Name:Into<glsl::Identifier>>
    (&self, name:Name) -> glsl::LocalVar {
        glsl::LocalVar {
            constant : self.constant,
            typ      : self.typ.clone(),
            ident    : name.into()
        }
    }
}

// === AttributeQualifier ===

#[derive(Clone,Debug)]
pub struct AttributeQualifier {
    pub storage : glsl::LinkageStorage,
    pub prec    : Option<glsl::Precision>,
    pub typ     : glsl::Type,
}

impl AttributeQualifier {
    pub fn to_input_var<Name:Into<glsl::Identifier>>
    (&self, name:Name) -> glsl::GlobalVar {
        let storage = self.storage.clone();
        glsl::GlobalVar {
            layout  : None,
            storage : Some(glsl::GlobalVarStorage::InStorage(storage)),
            prec    : self.prec.clone(),
            typ     : self.typ.clone(),
            ident   : name.into()
        }
    }

    pub fn to_output_var<Name:Into<glsl::Identifier>>
    (&self, name:Name) -> glsl::GlobalVar {
        let storage = self.storage.clone();
        glsl::GlobalVar {
            layout  : None,
            storage : Some(glsl::GlobalVarStorage::OutStorage(storage)),
            prec    : self.prec.clone(),
            typ     : self.typ.clone(),
            ident   : name.into()
        }
    }
}

// === UniformQualifier ===

#[derive(Clone,Debug)]
pub struct UniformQualifier {
    pub prec : Option<glsl::Precision>,
    pub typ  : glsl::Type,
}

impl UniformQualifier {
    pub fn to_var<Name:Into<glsl::Identifier>>
    (&self, name:Name) -> glsl::GlobalVar {
        glsl::GlobalVar{
            layout  : None,
            storage : Some(glsl::GlobalVarStorage::UniformStorage),
            prec    : self.prec.clone(),
            typ     : self.typ.clone(),
            ident   : name.into()
        }
    }
}


// =====================
// === ShaderBuilder ===
// =====================

#[derive(Clone,Debug,Default)]
pub struct ShaderBuilder {
    pub vertex   : glsl::Module,
    pub fragment : glsl::Module,
}

impl ShaderBuilder {
    pub fn new() -> Self { default() }

    pub fn compute<V:Str,F:Str>
    (&mut self, cfg:&ShaderConfig, _vertex_code:V, _fragment_code:F) {
        self.gen_precision_code(cfg);
        self.gen_attributes_code(cfg);
        self.gen_shared_attributes_code(cfg);
        self.gen_uniforms_code(cfg);
        self.gen_outputs_code(cfg);
    }

    fn gen_precision_code(&mut self, cfg:&ShaderConfig) {
        for (typ,prec) in &cfg.precision.vertex {
            self.vertex.add(glsl::PrecisionDecl::new(prec,typ));
        }
        for (typ,prec) in &cfg.precision.fragment {
            self.fragment.add(glsl::PrecisionDecl::new(prec,typ));
        }
    }

    fn gen_attributes_code(&mut self, cfg:&ShaderConfig) {
        if !cfg.attributes.is_empty() {
            for (name,qual) in &cfg.attributes {
                let vert_name = mk_vertex_name(&name);
                let frag_name = mk_fragment_name(&name);
                let sharing   = glsl::Assignment::new(&frag_name,&vert_name);
                self.vertex  .add(qual.to_input_var (&vert_name));
                self.vertex  .add(qual.to_output_var(&frag_name));
                self.fragment.add(qual.to_input_var (&frag_name));
                self.vertex.main.add(sharing);
            }
        }
    }

    fn gen_shared_attributes_code(&mut self, cfg:&ShaderConfig) {
        if !cfg.shared.is_empty() {
            for (name,qual) in &cfg.shared {
                let vert_name = mk_vertex_name(&name);
                let frag_name = mk_fragment_name(&name);
                self.vertex  .add(qual.to_output_var(vert_name));
                self.fragment.add(qual.to_input_var (frag_name));
            }
        }
    }

    fn gen_uniforms_code(&mut self, cfg:&ShaderConfig) {
        if !cfg.uniforms.is_empty() {
            for (name,qual) in &cfg.uniforms {
                self.vertex  .add(qual.to_var(name));
                self.fragment.add(qual.to_var(name));
            }
        }
    }

    fn gen_outputs_code(&mut self, cfg:&ShaderConfig) {
        if !cfg.outputs.is_empty() {
            cfg.outputs.iter().enumerate().for_each(|(loc,(name,qual))|{
                let mut var = qual.to_output_var(name);
                var.layout = Some(glsl::Layout {location: loc});
                self.fragment.add(var);
            });
        }
    }

    pub fn get(&self) -> Shader {
        let vertex   = self.vertex  .to_code();
        let fragment = self.fragment.to_code();
        Shader {vertex,fragment}
    }
}

fn _mk_out_name     <S:Str> (s:S) -> String { format!("out_{}", s.as_ref()) }
fn mk_vertex_name   <S:Str> (s:S) -> String { format!("v_{}"  , s.as_ref()) }
fn mk_fragment_name <S:Str> (s:S) -> String { s.as_ref().into() }


//use crate::prelude::*;
//
//pub fn main() {
//    let mut cfg = builder::ShaderConfig::new();
//    let mut sb = builder::ShaderBuilder::new();
//    cfg.attributes.insert("foo".to_string(),builder::AttributeQualifier{
//        storage: default(),
//        prec: default(),
//        typ: glsl::Type {
//            prim: glsl::PrimType::Float,
//            array: None
//        }
//    });
//    sb.compute(&cfg,"--1--","--2--");
//    let s  = sb.get();
//    println!("{}",s.vertex);
//    println!("{}",s.fragment);
//}
