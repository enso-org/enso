// =================================================================================================
// === Math Helpers ================================================================================
// =================================================================================================

// =================
// === Constants ===
// =================

#define PI         3.14159265
#define TAU        (2.0*PI)
#define PHI        (sqrt(5.0)*0.5 + 0.5)
#define FLOAT_MAX  3.402823466e+38
#define FLOAT_MIN  1.175494351e-38
#define DOUBLE_MAX 1.7976931348623158e+308
#define DOUBLE_MIN 2.2250738585072014e-308
const float INF = 1e10;



// =============
// === Utils ===
// =============

// === Mix ===

/// Weight interpolation between two values.
float mix (float a, float b, float weight_a, float weight_b) {
    return (a*weight_a + b*weight_b) / (weight_a + weight_b);
}

/// Weight interpolation between two values.
vec2 mix (vec2 a, vec2 b, float weight_a, float weight_b) {
    vec2 c;
    float total_weight = weight_a + weight_b;
    c.x = (a.x*weight_a + b.x*weight_b) / total_weight;
    c.y = (a.y*weight_a + b.y*weight_b) / total_weight;
    return c;
}

/// Weight interpolation between two values.
vec3 mix (vec3 a, vec3 b, float weight_a, float weight_b) {
    vec3 c;
    float total_weight = weight_a + weight_b;
    c.x = (a.x * weight_a + b.x * weight_b) / total_weight;
    c.y = (a.y * weight_a + b.y * weight_b) / total_weight;
    c.z = (a.z * weight_a + b.z * weight_b) / total_weight;
    return c;
}

/// Weight interpolation between two values.
vec4 mix (vec4 a, vec4 b, float weight_a, float weight_b) {
    vec4 c;
    float total_weight = weight_a + weight_b;
    c.x = (a.x * weight_a + b.x * weight_b) / total_weight;
    c.y = (a.y * weight_a + b.y * weight_b) / total_weight;
    c.z = (a.z * weight_a + b.z * weight_b) / total_weight;
    c.w = (a.w * weight_a + b.w * weight_b) / total_weight;
    return c;
}


// === Clamp ===

/// Constrain a value to lie between 0 and 1.
float clamp (float a) {
    return clamp(a,0.0,1.0);
}

/// Constrain a value to lie between 0 and 1.
vec2 clamp (vec2 a) {
    return clamp(a,0.0,1.0);
}

/// Constrain a value to lie between 0 and 1.
vec3 clamp (vec3 a) {
    return clamp(a,0.0,1.0);
}

/// Constrain a value to lie between 0 and 1.
vec4 clamp (vec4 a) {
    return clamp(a,0.0,1.0);
}


// === Max ===

/// Return the greater of all field values.
float max (vec2 v) {
    return max(v.x,v.y);
}

/// Return the greater of all field values.
float max (vec3 v) {
    return max(max(v.x,v.y),v.z);
}

/// Return the greater of all field values.
float max (vec4 v) {
    return max(max(v.x,v.y),max(v.z,v.w));
}


// === Min ===

/// Return the lesser of all field values.
float min (vec2 v) {
    return min(v.x, v.y);
}

/// Return the lesser of all field values.
float min (vec3 v) {
    return min(min(v.x, v.y), v.z);
}

/// Return the lesser of all field values.
float min (vec4 v) {
    return min(min(v.x, v.y), min(v.z, v.w));
}


// === Smoothstep ===

/// Perform Hermite interpolation between 0 and 1.
float smoothstep (float a) {
    return smoothstep (0.0, 1.0, a);
}


// === Angle ===

struct Radians {
    float value;
};

float value(Radians t) {
    return t.value;
}

struct Degrees {
    float value;
};

float value(Degrees t) {
    return t.value;
}

Radians radians(Degrees t) {
    return Radians(radians(t.value));
}

Radians div(Radians a, float b) {
    return Radians(a.value/b);
}

Radians neg(Radians a) {
    return Radians(-a.value);
}


// === Operators ===

float add(float a, float b) {
    return a + b;
}

float sub(float a, float b) {
    return a - b;
}

float div(float a, float b) {
    return a / b;
}

float mul(float a, float b) {
    return a * b;
}

float rem(float a, float b) {
    return mod(a,b);
}

float neg(float a) {
    return -a;
}


// === Encode ===

/// Encodes na [`uint`] values so it can be stored as a u8 encoded [`float`]. Will clamp values that
/// are out of range.
float as_float_u8(uint value) {
    return clamp(float(value) / 255.0);
}

vec3 as_float_u8(uvec3 v) {
    return vec3(as_float_u8(v.x),as_float_u8(v.y),as_float_u8(v.z));
}

/// The threshold used to decide whether a value should be included in the generated ID map. The
/// threshold is defined as 0.0 because it is quite common to use almost completely transparent
/// colors (like `Rgba(0.0, 0.0, 0.0, 0.000001)`) for shapes which should just catch mouse events
/// without providing any visual feedback.
const float ID_ALPHA_THRESHOLD = 0.0;

/// The maximum ID that can be encoded. We are encoding IDs using rgb values (3 bytes).
const int MAX_ENCODE_ID = 0;

/// Enables check for ID encoding.
#define ID_ENCODING_OVERFLOW_CHECK

/// This encoding must correspond to the decoding in the [`PointerTarget`] struct in the
/// `ensogl/core/src/display/scene/pointer_target.rs` file.
///
/// *Overflow Behavior*
///
uvec3 encode(int value) {
    int r_mask = 0xFF;
    int g_mask = 0xFF00;
    int b_mask = 0xFF0000;
    int r = (value & r_mask);
    int g = (value & g_mask) >> 8;
    int b = (value & b_mask) >> 16;
    return uvec3(r,g,b);
}

vec4 encode2(int value, float alpha) {
    uvec3 chunks = encode(value);
    vec3 rgb = as_float_u8(chunks);
    rgb *= alpha;
#ifdef ID_ENCODING_OVERFLOW_CHECK
    bool is_overflow = value > MAX_ENCODE_ID;
    alpha = is_overflow ? (100.0/255.0) : alpha;
#endif
    return vec4(as_float_u8(chunks),alpha);
}
