//! Canvas for drawing vector graphics. See the documentation of `Canvas` to learn more.

use crate::prelude::*;
use crate::display::shape::primitive::shader::data::ShaderData;
use crate::system::gpu::shader::glsl::Glsl;



// ===================
// === CanvasShape ===
// ===================

/// Immutable reference to a shape defined on `Canvas` with a fast clone.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct CanvasShape {
    rc: Rc<CanvasShapeData>
}

impl CanvasShape {
    /// Constructor.
    pub fn new(data:CanvasShapeData) -> Self {
        let rc = Rc::new(data);
        Self {rc}
    }
}



// =======================
// === CanvasShapeData ===
// =======================

/// Definition of a shape defined on `Canvas`. Please note that this shape definition is just a
/// reference to GLSL code which defines a vector shape there.
#[derive(Clone,Debug)]
pub struct CanvasShapeData {
    shape_num : usize,
    ids       : Vec<usize>,
    name      : String,
}

impl CanvasShapeData {
    /// Constructor.
    pub fn new(shape_num:usize) -> Self {
        let ids  = default();
        let name = format!("shape_{}",shape_num.to_string());
        Self {shape_num,ids,name}
    }

    /// Adds new id enclosed in this shape.
    pub fn add_id(&mut self, id:usize) {
        self.ids.push(id);
    }

    /// Add multiple ids enclosed in this shape.
    pub fn add_ids(&mut self, ids:&[usize]) {
        self.ids.extend(ids)
    }

    /// Getter of the shape as GLSL expression.
    pub fn getter(&self) -> String {
        iformat!("{self.name}(env,position)")
    }
}



// ==============
// === Canvas ===
// ==============

// === Definition ===

/// Canvas for drawing vector graphics.
///
/// The API is stateful, similar to the API of HTML5 canvas element.
/// It uses GLSL and signed distance fields under the hood.

#[derive(Debug,Default)]
pub struct Canvas {
    next_id                : usize,
    functions              : Vec<String>,
    current_function_lines : Vec<String>,
    defined_shapes         : HashMap<usize, CanvasShape>,
}


// === ID Management ===

impl Canvas {
    /// Generates a new unique shape's ID.
    pub fn get_new_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}


// === GLSL Modification ===

impl Canvas {
    /// Checks if shape with the given id was already defined. If so, a cached `ShapeCanvas` is
    /// returned. Otherwise the provided constructor is run and the result is cached.
    pub fn if_not_defined<F:FnOnce(&mut Self) -> CanvasShapeData>
    (&mut self, id:usize, f:F) -> CanvasShape {
        match self.defined_shapes.get(&id) {
            Some(shape) => shape.clone(),
            None => {
                let shape = CanvasShape::new(f(self));
                self.defined_shapes.insert(id,shape.clone());
                shape
            }
        }
    }

    /// Adds new code line to the GLSL code.
    pub fn add_current_function_code_line<S:Into<String>>(&mut self, line:S) {
        self.current_function_lines.push(line.into());
    }

    /// Defines a new variable in the GLSL code.
    pub fn define<E:Str>(&mut self, ty:&str, name:&str, expr:E) {
        let max_type_length = 8;
        let max_name_length = 6;
        let ty              = format!("{:1$}" , ty   , max_type_length);
        let name            = format!("{:1$}" , name , max_name_length);
        self.add_current_function_code_line(iformat!("{ty} {name} = {expr.as_ref()};"));
    }

    /// Submits the `current_function_lines` as a new shape construction function in the GLSL code.
    pub fn submit_shape_constructor(&mut self, name:&str) {
        let body = self.current_function_lines.join("\n    ");
        let func = iformat!("Shape {name} (Env env, vec2 position) {{\n    {body}\n}}");
        self.current_function_lines = default();
        self.functions.push(func);
    }

    /// Get the final GLSL code.
    pub fn to_glsl(&self) -> String {
        if !self.current_function_lines.is_empty() {
            panic!("Internal error. Not all canvas GLSL code lines were converted to functions.");
        }
        self.functions.join("\n\n")
    }
}


// === Shape Definition ===

impl Canvas {
    /// Defines a new shape with a new id and associated parameters, like color.
    pub fn define_shape(&mut self, num:usize, sdf:&str) -> CanvasShape {
        self.if_not_defined(num, |this| {
            let color     = "lcha(rgba(1.0,0.0,0.0))";
            let mut shape = CanvasShapeData::new(num);
            let id        = this.get_new_id();
            this.define("LCHA"     , "color" , iformat!("{color}"));
            this.define("BoundSdf" , "sdf"   , iformat!("{sdf}"));
            this.define("Id"       , "id"    , iformat!("new_id_layer(sdf,{id})"));
            this.add_current_function_code_line("return shape(id,sdf,color);");
            this.submit_shape_constructor(&shape.name);
            shape.add_id(id);
            shape
        })
    }

    /// Define a new shape from the provided GLSL expression.
    pub fn new_shape_from_expr(&mut self, num:usize, expr:&str) -> CanvasShapeData {
        let shape = CanvasShapeData::new(num);
        self.add_current_function_code_line(expr);
        self.submit_shape_constructor(&shape.name);
        shape
    }
}


// === Shape Modification ===

impl Canvas {
    /// Create a union shape from the provided shape components.
    pub fn union(&mut self, num:usize, s1:CanvasShape, s2:CanvasShape) -> CanvasShape {
        self.if_not_defined(num, |this| {
            let expr      = iformat!("return unify({s1.getter()},{s2.getter()});");
            let mut shape = this.new_shape_from_expr(num,&expr);
            shape.add_ids(&s1.ids);
            shape.add_ids(&s2.ids);
            shape
        })
    }

    /// Translate the current canvas origin.
    pub fn translate<X:ShaderData<f32>, Y:ShaderData<f32>>
    (&mut self, num:usize, s1:CanvasShape, x:X, y:Y) -> CanvasShape {
        self.if_not_defined(num, |this| {
            let x:Glsl = x.into();
            let y:Glsl = y.into();
            let trans  = iformat!("position = translate(position,vec2({x},{y}));");
            let expr   = iformat!("return {s1.getter()};");
            this.add_current_function_code_line(trans);
            let mut shape = this.new_shape_from_expr(num,&expr);
            shape.add_ids(&s1.ids);
            shape
        })
    }
}
