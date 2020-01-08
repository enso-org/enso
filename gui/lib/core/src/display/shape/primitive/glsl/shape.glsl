
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
    return BoundingBox(min_x,max_x,min_y,max_y);
}

BoundingBox bounding_box (float w, float h) {
    return BoundingBox(-w,w,-h,h);
}

BoundingBox unify (BoundingBox a, BoundingBox b) {
    float min_x = min(a.min_x,b.min_x);
    float max_x = max(a.max_x,b.max_x);
    float min_y = min(a.min_y,b.min_y);
    float max_y = max(a.max_y,b.max_y);
    return BoundingBox(min_x,max_x,min_y,max_y);
}



// ===========
// === Sdf ===
// ===========

/// Signed distance field. Describes the distance to the nearest point of a shape.
/// Follow the link to learn more: https://en.wikipedia.org/wiki/Signed_distance_function .
struct Sdf {
    float distance;
};

Sdf sdf (float distance) {
    return Sdf(distance);
}

Sdf unify (Sdf a, Sdf b) {
    return Sdf(min(a.distance,b.distance));
}



// ================
// === BoundSdf ===
// ================

/// Bound SDF. Signed distance field with associated bounds. See documentation of `sdf` and `bbox`
/// to learn more.
struct BoundSdf {
    float       distance;
    BoundingBox bounds;
};


// === Getters ===

float distance (BoundSdf a) {
    return a.distance;
}

//BoundingBox bounds (BoundSdf a) {
//    return a.bounds;
//}

Sdf sdf (BoundSdf a) {
    return sdf(a.distance);
}


// === Smart Constructors ===

BoundSdf bound_sdf (float distance, BoundingBox bounds) {
    return BoundSdf(distance,bounds);
}

BoundSdf bound_sdf (Sdf sdf, BoundingBox bounds) {
    return bound_sdf(sdf.distance,bounds);
}


// === API ===

BoundSdf unify (BoundSdf a, BoundSdf b) {
    return bound_sdf(unify(sdf(a),sdf(b)),unify(a.bounds,b.bounds));
}



// ==========
// === Id ===
// ==========

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

struct Shape {
    Id       id;
    BoundSdf sdf;
    LCHA    color;
};

Shape shape (Id id, BoundSdf bound_sdf, LCHA color) {
    return Shape(id,bound_sdf,color);
}

Shape unify (Shape s1, Shape s2) {
    return Shape(s1.id,unify(s1.sdf,s2.sdf),s1.color);
}

float render(Shape shape) {
    return clamp((0.5 - shape.sdf.distance) * input_pixel_ratio * input_zoom);
}



// ===========
// === Env ===
// ===========

struct Env {
    int test;
};



///////////////////////
////// Transform //////
///////////////////////

vec2 translate (vec2 p, vec2 t) {
    return p - t;
}

vec2 rotate (vec2 p, float angle) {
	return p*cos(angle) + vec2(p.y,-p.x)*sin(angle);
}

vec2 cartesian2polar (vec2 p) {
  return vec2(length(p), atan(p.y, p.x));
}
