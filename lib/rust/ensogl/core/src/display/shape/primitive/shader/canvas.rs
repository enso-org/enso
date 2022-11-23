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
#[derive(Clone, Debug, Shrinkwrap)]
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
    shape_num: usize,
    ids:       Vec<usize>,
    name:      String,
}

impl ShapeData {
    /// Constructor.
    pub fn new(shape_num: usize) -> Self {
        let ids = default();
        let name = format!("shape_{}", shape_num);
        Self { shape_num, ids, name }
    }

    /// Adds new id enclosed in this shape.
    pub fn add_id(&mut self, id: usize) {
        self.ids.push(id);
    }

    /// Add multiple ids enclosed in this shape.
    pub fn add_ids(&mut self, ids: &[usize]) {
        self.ids.extend(ids)
    }

    /// Getter of the shape as GLSL expression.
    pub fn getter(&self) -> String {
        iformat!("{self.name}(position)")
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
        self.add_current_function_code_line(iformat!("{ty} {name} = {expr.as_ref()};"));
    }

    /// Submits the `current_function_lines` as a new shape construction function in the GLSL code.
    pub fn submit_shape_constructor(&mut self, name: &str) {
        let body = self.current_function_lines.join("\n    ");
        let func = iformat!("Shape {name} (vec2 position) {{\n    {body}\n}}");
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
            let mut shape = ShapeData::new(num);
            let id = this.get_new_id();
            this.define("BoundSdf", "sdf", iformat!("{sdf}"));
            this.define("Id", "id", iformat!("new_id_layer(sdf,{id})"));
            this.add_current_function_code_line("return shape(id, sdf);");
            this.submit_shape_constructor(&shape.name);
            shape.add_id(id);
            shape
        })
    }

    /// Define a new shape from the provided GLSL expression.
    pub fn new_shape_from_expr(&mut self, num: usize, expr: &str) -> ShapeData {
        let shape = ShapeData::new(num);
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
            let expr = iformat!("return unify({s1.getter()},{s2.getter()});");
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s1.ids);
            shape.add_ids(&s2.ids);
            shape
        })
    }

    /// Create a difference shape from the provided shape components.
    pub fn difference(&mut self, num: usize, s1: Shape, s2: Shape) -> Shape {
        self.if_not_defined(num, |this| {
            let expr = iformat!("return difference({s1.getter()},{s2.getter()});");
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s1.ids);
            shape.add_ids(&s2.ids);
            shape
        })
    }

    /// Create a difference shape from the provided shape components.
    pub fn intersection(&mut self, num: usize, s1: Shape, s2: Shape) -> Shape {
        self.if_not_defined(num, |this| {
            let expr = iformat!("return intersection({s1.getter()},{s2.getter()});");
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s1.ids);
            shape.add_ids(&s2.ids);
            shape
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
            let trans = iformat!("position = translate(position,{v});");
            let expr = iformat!("return {s1.getter()};");
            this.add_current_function_code_line(trans);
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s1.ids);
            shape
        })
    }

    /// Rotate the current canvas origin.
    pub fn rotation<A: Into<Var<Radians>>>(&mut self, num: usize, s1: Shape, angle: A) -> Shape {
        self.if_not_defined(num, |this| {
            let angle: Glsl = angle.into().glsl();
            let trans = iformat!("position = rotate(position,{angle});");
            let expr = iformat!("return {s1.getter()};");
            this.add_current_function_code_line(trans);
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s1.ids);
            shape
        })
    }

    /// Scale the current canvas origin.
    pub fn scale<T: Into<Var<f32>>>(&mut self, num: usize, s1: Shape, value: T) -> Shape {
        self.if_not_defined(num, |this| {
            let value: Glsl = value.into().glsl();
            let trans = iformat!("position = scale(position,{value});");
            let expr = iformat!("return resample({s1.getter()},{value});");
            this.add_current_function_code_line(trans);
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s1.ids);
            shape
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
            this.add_current_function_code_line(iformat!("Shape shape = {s.getter()};"));
            this.add_current_function_code_line(iformat!("Srgba color = srgba({color});"));
            let expr = iformat!("return set_color(shape,color);");
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s.ids);
            shape
        })
    }

    /// Make the borders of the shape crisp. Please note that it removes any form of antialiasing.
    pub fn pixel_snap(&mut self, num: usize, s: Shape) -> Shape {
        self.if_not_defined(num, |this| {
            let expr = iformat!("return pixel_snap({s.getter()});");
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s.ids);
            shape
        })
    }

    /// Grow the shape by the given value.
    pub fn grow<T: Into<Var<f32>>>(&mut self, num: usize, s: Shape, value: T) -> Shape {
        self.if_not_defined(num, |this| {
            let value: Glsl = value.into().glsl();
            let expr = iformat!("return grow({s.getter()},{value});");
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s.ids);
            shape
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
            let repeat = iformat!("position = repeat(position,{value});");
            let expr = iformat!("return withInfiniteBounds({s.getter()});");
            this.add_current_function_code_line(repeat);
            let mut shape = this.new_shape_from_expr(num, &expr);
            shape.add_ids(&s.ids);
            shape
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
