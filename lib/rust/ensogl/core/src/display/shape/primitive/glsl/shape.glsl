//! GLSL shape definition boilerplate.


// =============
// === Color ===
// =============

/// The default color used for [`Shape`]s.
struct Color {
    Rgba repr;
};

Color color (Rgba color) {
    return Color(color);
}

Color color(vec3 rgb, float a) {
    return Color(rgba(rgb, a));
}

Color color(Rgb rgb, float a) {
    return Color(rgba(rgb, a));
}

Srgba srgba(Color color) {
    return srgba(color.repr);
}



// ==========================
// === PremultipliedColor ===
// ==========================

/// The premultiplied version of [`Color`] (the `xyz` components are multiplied by its alpha).
struct PremultipliedColor {
    Rgba repr;
};

PremultipliedColor premultiply(Color t) {
    float alpha = a(t.repr);
    vec3 rgb = t.repr.raw.rgb * alpha;
    return PremultipliedColor(rgba(rgb, alpha));
}

Color unpremultiply(PremultipliedColor c) {
    float alpha = c.repr.raw.a;
    vec3 rgb = alpha > 0.0 ? c.repr.raw.rgb / alpha : c.repr.raw.rgb;
    return color(rgb, alpha);
}

/// Implements glBlendFuncSeparate(GL_ONE, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE_MINUS_SRC_ALPHA)
/// in the [`Color`]'s color space. See docs of [`Color`] to learn more.
PremultipliedColor blend(PremultipliedColor bg, PremultipliedColor fg) {
    vec4 raw = fg.repr.raw + (1.0 - fg.repr.raw.a) * bg.repr.raw;
    return PremultipliedColor(rgba(raw));
}

Srgba srgba(PremultipliedColor color) {
    return srgba(unpremultiply(color));
}

Rgba rgba(PremultipliedColor color) {
    return unpremultiply(color).repr;
}



// ===================
// === BoundingBox ===
// ===================

/// Describes the rectangular convex hull of an object.
struct BoundingBox {
    float min_x;
    float max_x;
    float min_y;
    float max_y;
};

BoundingBox bounding_box (float min_x, float max_x, float min_y, float max_y) {
    return BoundingBox(min_x, max_x, min_y, max_y);
}

BoundingBox bounding_box (float w, float h) {
    return BoundingBox(-w, w, -h, h);
}

BoundingBox bounding_box (vec2 center_point, float w, float h) {
    return BoundingBox(center_point.x - w, center_point.x + w, center_point.y - h, center_point.y + h);
}

BoundingBox bounding_box (vec2 size) {
    float w2 = size.x / 2.0;
    float h2 = size.y / 2.0;
    return BoundingBox(-w2, w2, -h2, h2);
}

BoundingBox infinite () {
    return BoundingBox(0.0, 0.0, 0.0, 0.0);
}

/// Inverses the bounding box. Please note that the inversed bounding box is infinite and thus
/// it does not have a proper representation. We represent infinite bounding boxes as 0-sized ones.
BoundingBox inverse (BoundingBox a) {
    return infinite();
}

BoundingBox unify (BoundingBox a, BoundingBox b) {
    float min_x = min(a.min_x, b.min_x);
    float max_x = max(a.max_x, b.max_x);
    float min_y = min(a.min_y, b.min_y);
    float max_y = max(a.max_y, b.max_y);
    return BoundingBox(min_x, max_x, min_y, max_y);
}

BoundingBox intersection (BoundingBox a, BoundingBox b) {
    float min_x = max(a.min_x, b.min_x);
    float max_x = min(a.max_x, b.max_x);
    float min_y = max(a.min_y, b.min_y);
    float max_y = min(a.max_y, b.max_y);
    return BoundingBox(min_x, max_x, min_y, max_y);
}

/// Please note that we cannot compute the exact bounding box for a difference of `a - b`. Thus, we
/// output the best approximation we have, which is the original bounding box.
BoundingBox difference (BoundingBox a, BoundingBox b) {
    return a;
}

BoundingBox grow (BoundingBox a, float value) {
    float min_x = a.min_x - value;
    float max_x = a.max_x + value;
    float min_y = a.min_y - value;
    float max_y = a.max_y + value;
    return BoundingBox(min_x, max_x, min_y, max_y);
}

bool contains(BoundingBox box, vec2 point) {
    return box.min_x <= point.x && box.max_x >= point.x && box.min_y <= point.y && box.max_y >= point.y;
}



// ===========
// === Sdf ===
// ===========

/// Signed distance field. Describes the distance to the nearest point of a shape. Follow the link
/// to learn more: https://en.wikipedia.org/wiki/Signed_distance_function.
struct Sdf {
    float distance;
};

Sdf sdf (float distance) {
    return Sdf(distance);
}

Sdf inverse (Sdf a) {
    return Sdf(-a.distance);
}

Sdf unify (Sdf a, Sdf b) {
    return Sdf(min(a.distance, b.distance));
}

Sdf intersection (Sdf a, Sdf b) {
    return Sdf(max(a.distance, b.distance));
}

Sdf difference (Sdf a, Sdf b) {
    return intersection(a, inverse(b));
}

Sdf grow (Sdf a, float size) {
    return Sdf(a.distance - size);
}



// ================
// === BoundSdf ===
// ================

/// Bound SDF. Signed distance field with associated bounds. See documentation of [`sdf`] and
/// [`bbox`] to learn more.
struct BoundSdf {
    float       distance;
    BoundingBox bounds;
};

float render(BoundSdf sdf) {
    float zoom = zoom();
    float growth = 0.5 / zoom;
    return clamp((-sdf.distance * input_pixel_ratio + growth) * zoom);
}


// === Getters ===

float distance (BoundSdf a) {
    return a.distance;
}

Sdf sdf (BoundSdf a) {
    return sdf(a.distance);
}


// === Smart Constructors ===

BoundSdf bound_sdf (float distance, BoundingBox bounds) {
    return BoundSdf(distance, bounds);
}

BoundSdf bound_sdf (Sdf sdf, BoundingBox bounds) {
    return bound_sdf(sdf.distance, bounds);
}


// === API ===

BoundSdf resample (BoundSdf a, float multiplier) {
    a.distance *= multiplier;
    return a;
}

BoundSdf pixel_snap (BoundSdf a) {
    a.distance = floor(a.distance) + 0.5;
    return a;
}

BoundSdf grow (BoundSdf a, float size) {
    a.distance = a.distance - size;
    a.bounds = grow(a.bounds,size);
    return a;
}

BoundSdf inverse (BoundSdf a) {
    return bound_sdf(inverse(sdf(a)), inverse(a.bounds));
}

BoundSdf unify (BoundSdf a, BoundSdf b) {
    return bound_sdf(unify(sdf(a), sdf(b)), unify(a.bounds, b.bounds));
}

BoundSdf difference (BoundSdf a, BoundSdf b) {
    return bound_sdf(difference(sdf(a), sdf(b)), difference(a.bounds, b.bounds));
}

BoundSdf intersection (BoundSdf a, BoundSdf b) {
    return bound_sdf(intersection(sdf(a), sdf(b)), intersection(a.bounds, b.bounds));
}



// ==========
// === Id ===
// ==========

/// The ID of a shape. It is used to identify shapes after rendering to an non-antialiased ID
/// texture.
struct Id {
    int value;
};

Id id(int value) {
    return Id(value);
}

Id new_id_layer (BoundSdf sdf, int i) {
    return Id((sdf.distance <= 0.0) ? i : 0);
}



// =============
// === Shape ===
// =============

/// A visual shape that can be displayed on the screen or combined with other shapes.
struct Shape {
    /// The ID of the shape. It is used to identify shapes after rendering to an non-antialiased ID
    /// texture.
    Id id;
    /// The Signed Distance Field, describing the shape boundaries.
    BoundSdf sdf;
    /// The color of the shape.
    ///
    /// We are storing the premultiplied version of the color, as blending requires premultiplied
    /// values. We could store the non-premultiplied, however, then we would need to unpremultiply
    /// the color after blending, which leads to a serious issue. If the alpha is 0, then either
    /// unpremultiplication needs to be more costly and check for this condition, or it will produce
    /// infinite/invalid values for the `xyz` components. If we blend such a color with another one,
    /// then we will get artifacts, as multiplying an infinite/invalid value by 0 is an undefined
    /// behavior.
    ///
    /// The [`alpha`] field is applied on thin field, except if we are in
    /// `DISPLAY_MODE_CACHED_SHAPES_TEXTURE`. This display mode is an exception, because otherwise
    /// cached shapes would have darker borders if not aligned to the texture pixels.
    PremultipliedColor color;
    /// The opacity of the shape. It is the result of rendering of the [`sdf`].
    float alpha;
};

Shape shape (Id id, BoundSdf bound_sdf, PremultipliedColor color) {
    float alpha = render(bound_sdf);
    // See [`color`] field documentation for explanation why DISPLAY_MODE_CACHED_SHAPES_TEXTURE is
    // an exception.
    if (input_display_mode != DISPLAY_MODE_CACHED_SHAPES_TEXTURE) {
        color.repr.raw *= alpha;
    }
    return Shape(id, bound_sdf, color, alpha);
}

Shape shape (Id id, BoundSdf bound_sdf, Color color) {
    return shape(id, bound_sdf, premultiply(color));
}

Shape shape (Id id, BoundSdf bound_sdf, Rgba rgba) {
    return shape(id, bound_sdf, Color(rgba));
}

Shape shape (Id id, BoundSdf bound_sdf) {
    return shape(id, bound_sdf, rgba(1.0, 0.0, 0.0, 1.0));
}

/// A debug [`Shape`] constructor. Should not be used to create shapes that are rendered to the
/// screen as it's ID is always 0. It can be used to create temporary shapes. For example, it can
/// be used to create a clipping rectangle, that will be intersected with another shape.
Shape debug_shape (BoundSdf bound_sdf) {
    Id id = new_id_layer(bound_sdf, 10);
    return shape(id, bound_sdf);
}

Shape resample (Shape s, float multiplier) {
    Id id = s.id;
    BoundSdf sdf = resample(s.sdf, multiplier);
    // The [`alpha`] field is not applied on [`color`] if we are in
    // `DISPLAY_MODE_CACHED_SHAPES_TEXTURE`. See [`color`] docs for explanation.
    if (input_display_mode != DISPLAY_MODE_CACHED_SHAPES_TEXTURE) {
        s.color.repr.raw.a /= s.alpha;
    }
    return shape(id, sdf, s.color);
}

Shape pixel_snap (Shape s) {
    Id id = s.id;
    BoundSdf sdf = pixel_snap(s.sdf);
    // The [`alpha`] field is not applied on [`color`] if we are in
    // `DISPLAY_MODE_CACHED_SHAPES_TEXTURE`. See [`color`] docs for explanation.
    if (input_display_mode != DISPLAY_MODE_CACHED_SHAPES_TEXTURE) {
        s.color.repr.raw.a /= s.alpha;
    }
    return shape(id, sdf, s.color);
}

Shape grow (Shape s, float value) {
    Id id = s.id;
    BoundSdf sdf = grow(s.sdf,value);
    // The [`alpha`] field is not applied on [`color`] if we are in
    // `DISPLAY_MODE_CACHED_SHAPES_TEXTURE`. See [`color`] docs for explanation.
    if (input_display_mode != DISPLAY_MODE_CACHED_SHAPES_TEXTURE) {
        s.color.repr.raw.a /= s.alpha;
    }
    return shape(id, sdf, s.color);
}

Shape inverse (Shape s1) {
    return shape(s1.id, inverse(s1.sdf), s1.color);
}

Shape unify (Shape s1, Shape s2) {
    if (input_display_mode == DISPLAY_MODE_CACHED_SHAPES_TEXTURE) {
        // In DISPLAY_MODE_CACHED_SHAPES_TEXTURE the color has not [`alpha`] field applied (See
        // [`color`] documentation for explanation). That means, that even outside the
        // foreground shape the [`color`] field will be in the full shape's intensity. However we
        // need to make the foregroud color to give way for background colors, so it won't "cover"
        // the background shape.
        // There are two conditions we want to met:
        // * If we are outside the foreground shape, but inside the background shape, the foreground
        //   color should blend properly with background color, so it alpha must apply the sdf render
        //   results ([`alpha`] field).
        // * We want to keep the color consistent near border of the both shapes.
        // The code below meets the both conditions.
        if (s2.sdf.distance > s1.sdf.distance) {
            s2.color.repr.raw *= s2.alpha;
        }
    }
    return shape(s1.id, unify(s1.sdf, s2.sdf), blend(s1.color, s2.color));
}

Shape difference (Shape s1, Shape s2) {
    return shape(s1.id, difference(s1.sdf, s2.sdf), s1.color);
}

Shape intersection (Shape s1, Shape s2) {
    return shape(s1.id, intersection(s1.sdf, s2.sdf), blend(s1.color, s2.color));
}

Shape intersection_no_blend (Shape s1, Shape s2) {
    return shape(s1.id, intersection(s1.sdf, s2.sdf), s1.color);
}

Shape set_color(Shape shape, Rgba t) {
    // The [`alpha`] field is applied on [`color`] only if we are not in
    // `DISPLAY_MODE_CACHED_SHAPES_TEXTURE`. See [`color`] docs for explanation.
    if (input_display_mode != DISPLAY_MODE_CACHED_SHAPES_TEXTURE) {
        t.raw.a *= shape.alpha;
    }
    shape.color = premultiply(Color(t));
    return shape;
}

/// Change the shape color depending on RGB components.
///
/// See documentation of [`ShapeOps::colorize`] for detailed explanation and
/// usage examples.
Shape recolorize(Shape shape, Rgba r, Rgba g, Rgba b) {
    PremultipliedColor r_prem = premultiply(Color(r));
    PremultipliedColor g_prem = premultiply(Color(g));
    PremultipliedColor b_prem = premultiply(Color(b));
    vec4 r_component = premultiply(Color(r)).repr.raw * shape.color.repr.raw.r;
    vec4 g_component = premultiply(Color(g)).repr.raw * shape.color.repr.raw.g;
    vec4 b_component = premultiply(Color(b)).repr.raw * shape.color.repr.raw.b;
    Rgba new = rgba(r_component + g_component + b_component);
    // The original shape's color components had the [`alpha`] field already applied, so we don't need to apply it
    // again here.
    shape.color = PremultipliedColor(new);
    return shape;
}

Shape with_infinite_bounds (Shape s) {
    BoundSdf sdf = s.sdf;
    sdf.bounds = infinite();
    return shape(s.id, sdf, s.color);
}

/// Read the shape from the Cached Shapes Texture.
///
/// The `tex_bbox` is expressed in texture pixels, where origin is at the texture center.
Shape cached_shape(Id id, vec2 position, vec4 tex_bbox) {
    BoundingBox texture_bbox = bounding_box(tex_bbox.x, tex_bbox.z, tex_bbox.y, tex_bbox.w);
    BoundingBox shape_bbox = bounding_box(position, (tex_bbox.z - tex_bbox.x) / 2.0, (tex_bbox.w - tex_bbox.y) / 2.0);
    
    vec2 texture_bbox_center = (tex_bbox.xy + tex_bbox.zw) / 2.0;
    vec2 texture_position = texture_bbox_center + position;
    vec2 texture_uv_origin = vec2(0.5, 0.5);
    vec2 texture_size = vec2(textureSize(input_pass_cached_shapes, 0)) / input_pixel_ratio;
    vec2 texture_uv = (texture_position / vec2(texture_size)) + texture_uv_origin;
    Rgba color_and_distance;
    if (contains(texture_bbox, texture_position)) {
        color_and_distance = rgba(texture(input_pass_cached_shapes, texture_uv));
    } else {
        color_and_distance = rgba(0.0, 0.0, 0.0, 0.0);
    }
    // The signed distance is stored in the texture's alpha channel in a special way. See
    // [`crate::display::shape::primitive::system::cached`] documentation for details.
    float alpha_representing_0_sdf = 0.5;
    float distance = -(color_and_distance.raw.a - alpha_representing_0_sdf) * 2.0 * CACHED_SHAPE_MAX_DISTANCE;
    BoundSdf sdf = bound_sdf(distance, shape_bbox);
    Color shape_color = color(color_and_distance.raw.rgb, 1.0);
    return shape(id, sdf, shape_color);
}



// =================
// === Transform ===
// =================

vec2 translate (vec2 position, vec2 t) {
    return position - t;
}

vec2 rotate (vec2 position, Radians angle) {
    float v_angle = value(angle);
    return position * cos(-v_angle) + vec2(position.y, -position.x) * sin(-v_angle);
}

vec2 scale (vec2 position, float value) {
    return position / value;
}

vec2 cartesian2polar (vec2 position) {
  return vec2(length(position), atan(position.y, position.x));
}

vec2 repeat (vec2 position, vec2 tile_size) {
    return mod(position+tile_size/2.0, tile_size) - tile_size/2.0;
}



// =============
// === Debug ===
// =============

vec4 draw_grid(vec2 position, int level, vec4 color, vec4 output_color) {
    float sampling = pow(2.0, float(level - 1));
    float width = min(sampling, zoom()) / input_pixel_ratio + 0.1;
    float sampling2 = sampling / 2.0;
    bool v = abs(mod(position.x + sampling2, sampling) - sampling2) <= width/2.0/zoom();
    bool h = abs(mod(position.y + sampling2, sampling) - sampling2) <= width/2.0/zoom();
    if ((v || h) && !outside_of_uv()) {
        float alpha = clamp((zoom() - (4.0 - sampling))/(5.0 - sampling), 0.0, 1.0);
        color *= alpha;
        output_color = color + (1.0 - color.a) * output_color;
    }
    return output_color;
}
