//! Canvas for drawing vector graphics. See the documentation of `Canvas` to learn more.

use crate::prelude::*;
use crate::system::gpu::types::*;

use crate::data::color;
use crate::display::shape::primitive::def::var::Var;
use crate::system::gpu::shader::glsl::Glsl;



// =============
// === Shape ===
// =============

/// Immutable reference to a shape defined on `Canvas` with a fast clone.
#[derive(Clone, Debug, Deref)]
pub struct Shape {
    rc: Rc<ShapeData>,
}

impl Shape {
    /// Constructor.
    pub fn new(data: ShapeData) -> Self {
        let rc = Rc::new(data);
        Self { rc }
    }
}



// =================
// === ShapeData ===
// =================

/// Definition of a shape defined on `Canvas`. Please note that this shape definition is just a
/// reference to GLSL code which defines a vector shape there.
#[derive(Clone, Debug)]
pub struct ShapeData {
    name: String,
}

impl ShapeData {
    /// Constructor.
    pub fn new(shape_id: usize) -> Self {
        let name = format!("shape_{shape_id}");
        Self { name }
    }

    /// Getter of the shape as GLSL expression.
    pub fn getter(&self) -> String {
        format!("{}(position)", self.name)
    }
}



// ==============
// === Canvas ===
// ==============

// === Definition ===

/// Canvas for drawing vector graphics.
///
/// The API is stateful, similar to the API of HTML5 canvas element. It uses GLSL and signed
/// distance fields under the hood.

#[derive(Debug, Default)]
pub struct Canvas {
    next_id:                usize,
    functions:              Vec<String>,
    current_function_lines: Vec<String>,
    defined_shapes:         HashMap<usize, Shape>,
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
    pub fn if_not_defined<F: FnOnce(&mut Self) -> ShapeData>(&mut self, id: usize, f: F) -> Shape {
        match self.defined_shapes.get(&id) {
            Some(shape) => shape.clone(),
            None => {
                let shape = Shape::new(f(self));
                self.defined_shapes.insert(id, shape.clone());
                shape
            }
        }
    }

    /// Adds new code line to the GLSL code.
    pub fn add_current_function_code_line<S: Into<String>>(&mut self, line: S) {
        self.current_function_lines.push(line.into());
    }

    /// Defines a new variable in the GLSL code.
    pub fn define<E: Str>(&mut self, ty: &str, name: &str, expr: E) {
        self.add_current_function_code_line(format!("{ty} {name} = {};", expr.as_ref()));
    }

    /// Submits the `current_function_lines` as a new shape construction function in the GLSL code.
    pub fn submit_shape_constructor(&mut self, name: &str) {
        let body = self.current_function_lines.join("\n    ");
        let func = format!("Shape {name} (vec2 position) {{\n    {body}\n}}");
        self.current_function_lines = default();
        self.functions.push(func);
    }

    /// Get the final GLSL code.
    pub fn to_glsl(&self) -> String {
        assert!(
            self.current_function_lines.is_empty(),
            "Internal error. Not all canvas GLSL code lines were converted to functions."
        );
        self.functions.join("\n\n")
    }
}


// === Shape Definition ===

impl Canvas {
    /// Defines a new shape with a new id and associated parameters, like color.
    pub fn define_shape(&mut self, num: usize, sdf: &str) -> Shape {
        self.if_not_defined(num, |this| {
            // NOTE: Generated GLSL determinism
            // Use sequential IDs for shape names. This guarantees that the shape names are unique
            // per shader and deterministic across multiple runs of the codegen for identical shape
            // definition. This would not be true if we use `num`, which is derived from the address
            // of shape pointer. This is important for shader caching based on generated GLSL code.
            let id = this.get_new_id();
            let shape = ShapeData::new(id);
            this.define("BoundSdf", "sdf", sdf.to_string());
            this.define("Id", "id", format!("new_id_layer(sdf,{id})"));
            this.add_current_function_code_line("return shape(id, sdf);");
            this.submit_shape_constructor(&shape.name);
            shape
        })
    }

    /// Define a new shape from the provided GLSL expression.
    pub fn new_shape_from_expr(&mut self, expr: &str) -> ShapeData {
        // NOTE: Generated GLSL determinism - see above
        let id = self.get_new_id();
        let shape = ShapeData::new(id);
        self.add_current_function_code_line(expr);
        self.submit_shape_constructor(&shape.name);
        shape
    }
}


// === Shape Modification ===

impl Canvas {
    /// Create a union shape from the provided shape components.
    pub fn union(&mut self, num: usize, s1: Shape, s2: Shape) -> Shape {
        self.if_not_defined(num, |this| {
            let expr = format!("return unify({},{});", s1.getter(), s2.getter());
            this.new_shape_from_expr(&expr)
        })
    }

    /// Create a difference shape from the provided shape components.
    pub fn difference(&mut self, num: usize, s1: Shape, s2: Shape) -> Shape {
        self.if_not_defined(num, |this| {
            let expr = format!("return difference({},{});", s1.getter(), s2.getter());
            this.new_shape_from_expr(&expr)
        })
    }

    /// Create a difference shape from the provided shape components.
    pub fn intersection(&mut self, num: usize, s1: Shape, s2: Shape) -> Shape {
        self.if_not_defined(num, |this| {
            let expr = format!("return intersection({},{});", s1.getter(), s2.getter());
            this.new_shape_from_expr(&expr)
        })
    }

    /// Translate the current canvas origin.
    pub fn translate<V: Into<Var<Vector2<Pixels>>>>(
        &mut self,
        num: usize,
        s1: Shape,
        v: V,
    ) -> Shape {
        self.if_not_defined(num, |this| {
            let v = v.into().glsl();
            let trans = format!("position = translate(position,{v});");
            let expr = format!("return {};", s1.getter());
            this.add_current_function_code_line(trans);
            this.new_shape_from_expr(&expr)
        })
    }

    /// Rotate the current canvas origin.
    pub fn rotation<A: Into<Var<Radians>>>(&mut self, num: usize, s1: Shape, angle: A) -> Shape {
        self.if_not_defined(num, |this| {
            let angle: Glsl = angle.into().glsl();
            let trans = format!("position = rotate(position,{angle});");
            let expr = format!("return {};", s1.getter());
            this.add_current_function_code_line(trans);
            this.new_shape_from_expr(&expr)
        })
    }

    /// Scale the current canvas origin.
    pub fn scale<T: Into<Var<f32>>>(&mut self, num: usize, s1: Shape, value: T) -> Shape {
        self.if_not_defined(num, |this| {
            let value: Glsl = value.into().glsl();
            let trans = format!("position = scale(position,{value});");
            let expr = format!("return resample({},{value});", s1.getter());
            this.add_current_function_code_line(trans);
            this.new_shape_from_expr(&expr)
        })
    }

    /// Fill the shape with the provided color.
    pub fn fill<Color: Into<Var<color::Rgba>>>(
        &mut self,
        num: usize,
        s: Shape,
        color: Color,
    ) -> Shape {
        self.if_not_defined(num, |this| {
            let color: Glsl = color.into().glsl();
            this.add_current_function_code_line(format!("Shape shape = {};", s.getter()));
            this.add_current_function_code_line(format!("Srgba color = srgba({color});"));
            this.new_shape_from_expr("return set_color(shape,rgba(color));")
        })
    }

    /// Make the borders of the shape crisp. Please note that it removes any form of antialiasing.
    pub fn pixel_snap(&mut self, num: usize, s: Shape) -> Shape {
        self.if_not_defined(num, |this| {
            let expr = format!("return pixel_snap({});", s.getter());
            this.new_shape_from_expr(&expr)
        })
    }

    /// Grow the shape by the given value.
    pub fn grow<T: Into<Var<f32>>>(&mut self, num: usize, s: Shape, value: T) -> Shape {
        self.if_not_defined(num, |this| {
            let value: Glsl = value.into().glsl();
            let expr = format!("return grow({},{value});", s.getter());
            this.new_shape_from_expr(&expr)
        })
    }

    /// Shrink the shape by the given value.
    pub fn shrink<T: Into<Var<f32>>>(&mut self, num: usize, s: Shape, value: T) -> Shape {
        let value = value.into();
        self.grow(num, s, -value)
    }

    /// Repeat the shape with the given tile size.
    pub fn repeat<T: Into<Var<Vector2<Pixels>>>>(
        &mut self,
        num: usize,
        s: Shape,
        tile_size: T,
    ) -> Shape {
        self.if_not_defined(num, |this| {
            let value: Glsl = tile_size.into().glsl();
            let repeat = format!("position = repeat(position,{value});");
            let expr = format!("return with_infinite_bounds({});", s.getter());
            this.add_current_function_code_line(repeat);
            this.new_shape_from_expr(&expr)
        })
    }
}


// ============
// === Draw ===
// ============

/// Trait for every object which can be drawn on the canvas.
pub trait Draw: Debug {
    /// Draw the element on the canvas.
    fn draw(&self, canvas: &mut Canvas) -> Shape;
}
