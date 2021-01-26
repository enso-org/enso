// =================================================================================================
// === Colorspaces Definition ======================================================================
// =================================================================================================

/// Helper for colors definition.
/// For the given input of `DEF_COLOR(Rgb,Rgba,rgb,rgba,r,g,b)`, it defines:
///   - A Rgb  struct which contains `vec3 raw` field for each component.
///   - A Rgba struct which contains `vec4 raw` field for each component.
///   - A rich set of smart constructors for each of the types.
///   - A rich set of simple conversions between them.
///
/// All component values are in range 0.0..1.0.


#define DEF_COLOR(type_name3,type_name4,name3,name4,t1,t2,t3) \
                                                              \
/* ============================= */                           \
/* === Non Transparent Color === */                           \
/* ============================= */                           \
                                                              \
/* === Definition === */                                      \
                                                              \
struct type_name3 {                                           \
   vec3 raw;                                                  \
};                                                            \
                                                              \
                                                              \
/* === Getters === */                                         \
                                                              \
float t1 (type_name3 color) { return color.raw.x; }           \
float t2 (type_name3 color) { return color.raw.y; }           \
float t3 (type_name3 color) { return color.raw.z; }           \
                                                              \
                                                              \
/* === Constructors === */                                    \
                                                              \
type_name3 name3(type_name3 identity) {                       \
    return identity;                                          \
}                                                             \
                                                              \
type_name3 name3(vec3 raw) {                                  \
    return type_name3(raw);                                   \
}                                                             \
                                                              \
type_name3 name3(float t1, float t2, float t3) {              \
    return name3(vec3(t1,t2,t3));                             \
}                                                             \
                                                              \
                                                              \
                                                              \
/* ========================= */                               \
/* === Transparent Color === */                               \
/* ========================= */                               \
                                                              \
/* === Definition === */                                      \
                                                              \
struct type_name4 {                                           \
   vec4 raw;                                                  \
};                                                            \
                                                              \
                                                              \
/* === Getters === */                                         \
                                                              \
float t1 (type_name4 name4) { return name4.raw.x; }           \
float t2 (type_name4 name4) { return name4.raw.y; }           \
float t3 (type_name4 name4) { return name4.raw.z; }           \
float a  (type_name4 name4) { return name4.raw.a; }           \
                                                              \
                                                              \
/* === Constructors === */                                    \
                                                              \
type_name4 name4 (type_name4 identity) {                      \
    return identity;                                          \
}                                                             \
                                                              \
type_name4 name4 (vec4 raw) {                                 \
    return type_name4(raw);                                   \
}                                                             \
                                                              \
type_name4 name4 (vec3 raw) {                                 \
    return name4(vec4(raw,1.0));                              \
}                                                             \
                                                              \
type_name4 name4 (vec3 raw, float a) {                        \
    return name4(vec4(raw,a));                                \
}                                                             \
                                                              \
type_name4 name4 (type_name3 name3) {                         \
    return name4(name3.raw);                                  \
}                                                             \
                                                              \
type_name4 name4 (type_name3 name3, float a) {                \
    return name4(name3.raw,a);                                \
}                                                             \
                                                              \
type_name4 name4 (float t1, float t2, float t3) {             \
    return name4(vec3(t1,t2,t3));                             \
}                                                             \
                                                              \
type_name4 name4 (float t1, float t2, float t3, float a) {    \
    return name4(vec4(t1,t2,t3,a));                           \
}                                                             \
                                                              \
                                                              \
/* === Conversions === */                                     \
                                                              \
type_name3 name3 (type_name4 a) {                             \
    return name3(a.raw.xyz);                                  \
}


DEF_COLOR(Srgb , Srgba , srgb , srgba , r , g ,b)
DEF_COLOR(Rgb  , Rgba  , rgb  , rgba  , r , g ,b)
DEF_COLOR(HSV  , HSVA  , hsv  , hsva  , h , s ,v)
DEF_COLOR(Lch  , Lcha  , lch  , lcha  , l , c ,h)



// =================================================================================================
// === Colorpsace Conversion =======================================================================
// =================================================================================================

#define DEF_TRANSITIVE_CONVERSIONS_1_WAY(                                                 \
a_type_name3,a_type_name4,a_name3,a_name4,b_type_name3,b_type_name4,b_name3,b_name4) \
                                                                                     \
a_type_name3 a_name3(b_type_name4 b_name4) {                                         \
    return a_name3(b_name3(b_name4));                                                \
}                                                                                    \
                                                                                     \
a_type_name4 a_name4(b_type_name4 b_name4) {                                         \
    return a_name4(a_name3(b_name3(b_name4)),a(b_name4));                            \
}                                                                                    \
                                                                                     \
a_type_name4 a_name4(b_type_name3 b_name3) {                                         \
    return a_name4(a_name3(b_name3));                                                \
}

#define DEF_TRANSITIVE_CONVERSIONS(                                                       \
a_type_name3,a_type_name4,a_name3,a_name4,b_type_name3,b_type_name4,b_name3,b_name4) \
DEF_TRANSITIVE_CONVERSIONS_1_WAY(                                                         \
a_type_name3,a_type_name4,a_name3,a_name4,b_type_name3,b_type_name4,b_name3,b_name4) \
DEF_TRANSITIVE_CONVERSIONS_1_WAY(                                                         \
b_type_name3,b_type_name4,b_name3,b_name4,a_type_name3,a_type_name4,a_name3,a_name4)



// ====================
// === Rgb <-> Srgb ===
// ====================

const float SRGB_ALPHA = 0.055;

float channel_rgb_to_srgb(float channel) {
    if(channel <= 0.0031308)
    return 12.92 * channel;
    else
    return (1.0 + SRGB_ALPHA) * pow(channel, 1.0/2.4) - SRGB_ALPHA;
}

float channel_srgb_to_rgb(float channel) {
    if (channel <= 0.04045)
    return channel / 12.92;
    else
    return pow((channel + SRGB_ALPHA) / (1.0 + SRGB_ALPHA), 2.4);
}

Srgb srgb(Rgb rgb) {
    float r = channel_rgb_to_srgb(r(rgb));
    float g = channel_rgb_to_srgb(g(rgb));
    float b = channel_rgb_to_srgb(b(rgb));
    return srgb(r,g,b);
}

Rgb rgb(Srgb srgb) {
    float r = channel_srgb_to_rgb(r(srgb));
    float g = channel_srgb_to_rgb(g(srgb));
    float b = channel_srgb_to_rgb(b(srgb));
    return rgb(r,g,b);
}

DEF_TRANSITIVE_CONVERSIONS(Srgb,Srgba,srgb,srgba,Rgb,Rgba,rgb,rgba)



// ====================
// === Srgb <-> Lch ===
// ====================

vec3 lch_rgb_weights = vec3(1.0,1.0,1.0);
float xyzF(float t){ return mix(pow(t,1./3.), 7.787037 * t + 0.139731  , step(t,0.00885645)); }
float xyzR(float t){ return mix(t*t*t       , 0.1284185*(t - 0.139731) , step(t,0.20689655)); }
Lch lch(Srgb rgb) {
    vec3 c = rgb.raw;
    c *= mat3
        ( 0.4124, 0.3576, 0.1805
        , 0.2126, 0.7152, 0.0722
        , 0.0193, 0.1192, 0.9505 );
    c.x = xyzF(c.x/lch_rgb_weights.x);
    c.y = xyzF(c.y/lch_rgb_weights.y);
    c.z = xyzF(c.z/lch_rgb_weights.z);
    vec3 lab = vec3(max(0.,116.0*c.y - 16.0), 500.0*(c.x - c.y), 200.0*(c.y - c.z));
    return lch(lab.x, length(vec2(lab.y,lab.z)), atan(lab.z, lab.y));
}

Srgb srgb (Lch lch) {
    vec3 c = lch.raw;
    c = vec3(c.x, cos(c.z) * c.y, sin(c.z) * c.y);
    float lg = 1./116.*(c.x + 16.);
    float x  = lch_rgb_weights.x*xyzR(lg + 0.002*c.y);
    float y  = lch_rgb_weights.y*xyzR(lg);
    float z  = lch_rgb_weights.z*xyzR(lg - 0.005*c.z);
    vec3 xyz = vec3(x,y,z);
    vec3 raw = xyz * mat3
        (  3.2406, -1.5372, -0.4986
        , -0.9689,  1.8758,  0.0415
        ,  0.0557, -0.2040,  1.0570 );
    return srgb(raw);
}

DEF_TRANSITIVE_CONVERSIONS(Srgb,Srgba,srgb,srgba,Lch,Lcha,lch,lcha)



// ====================
// === Srgb <-> HSV ===
// ====================

HSV hsv(Srgb rgb) {
    vec3  c = rgb.raw;
    vec4  K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4  p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4  q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));
    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    float h = abs(q.z + (q.w - q.y) / (6.0 * d + e));
    float s = d / (q.x + e);
    float v = q.x;
    return hsv(h,s,v);
}

Srgb srgb(HSV hsv) {
    vec3 c = hsv.raw;
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return srgb(c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y));
}

DEF_TRANSITIVE_CONVERSIONS(Srgb,Srgba,srgb,srgba,HSV,HSVA,hsv,hsva)



// =================================================================================================
// === Mixing ======================================================================================
// =================================================================================================

Rgb mix(Rgb color1, Rgb color2, float t) {
    return rgb(mix(color1.raw,color2.raw,t));
}

Rgba mix(Rgba color1, Rgba color2, float t) {
    return rgba(mix(color1.raw,color2.raw,t));
}




// ================
// === Gradient ===
// ================


#define DEF_GRADIENT(COLOR,COLOR_CONS,CONTROL_POINT,GRADIENT1,GRADIENT2,GRADIENT3,GRADIENT4,GRADIENT5) \
struct CONTROL_POINT {                                                                             \
    float offset;                                                                                  \
    COLOR color;                                                                                   \
};                                                                                                 \
                                                                                                   \
CONTROL_POINT gradient_control_point(float offset, COLOR color) {                                  \
    return CONTROL_POINT(offset,color);                                                            \
}                                                                                                  \
                                                                                                   \
struct GRADIENT1 {                                                                                 \
    CONTROL_POINT control_point1;                                                                  \
};                                                                                                 \
                                                                                                   \
struct GRADIENT2 {                                                                                 \
    CONTROL_POINT control_point1;                                                                  \
    CONTROL_POINT control_point2;                                                                  \
};                                                                                                 \
                                                                                                   \
struct GRADIENT3 {                                                                                 \
    CONTROL_POINT control_point1;                                                                  \
    CONTROL_POINT control_point2;                                                                  \
    CONTROL_POINT control_point3;                                                                  \
};                                                                                                 \
                                                                                                   \
struct GRADIENT4 {                                                                                 \
    CONTROL_POINT control_point1;                                                                  \
    CONTROL_POINT control_point2;                                                                  \
    CONTROL_POINT control_point3;                                                                  \
    CONTROL_POINT control_point4;                                                                  \
};                                                                                                 \
                                                                                                   \
struct GRADIENT5 {                                                                                 \
    CONTROL_POINT control_point1;                                                                  \
    CONTROL_POINT control_point2;                                                                  \
    CONTROL_POINT control_point3;                                                                  \
    CONTROL_POINT control_point4;                                                                  \
    CONTROL_POINT control_point5;                                                                  \
};                                                                                                 \
                                                                                                   \
GRADIENT2 gradient                                                                                 \
( CONTROL_POINT control_point1                                                                     \
, CONTROL_POINT control_point2 ) {                                                                 \
    return GRADIENT2(control_point1,control_point2);                                               \
}                                                                                                  \
                                                                                                   \
GRADIENT3 gradient                                                                                 \
( CONTROL_POINT control_point1                                                                     \
, CONTROL_POINT control_point2                                                                     \
, CONTROL_POINT control_point3 ) {                                                                 \
    return GRADIENT3(control_point1,control_point2,control_point3);                                \
}                                                                                                  \
                                                                                                   \
GRADIENT4 gradient                                                                                 \
( CONTROL_POINT control_point1                                                                     \
, CONTROL_POINT control_point2                                                                     \
, CONTROL_POINT control_point3                                                                     \
, CONTROL_POINT control_point4 ) {                                                                 \
    return GRADIENT4(control_point1,control_point2,control_point3,control_point4);                 \
}                                                                                                  \
                                                                                                   \
GRADIENT5 gradient                                                                                 \
( CONTROL_POINT control_point1                                                                     \
, CONTROL_POINT control_point2                                                                     \
, CONTROL_POINT control_point3                                                                     \
, CONTROL_POINT control_point4                                                                     \
, CONTROL_POINT control_point5 ) {                                                                 \
    return GRADIENT5(control_point1,control_point2,control_point3,control_point4,control_point5);  \
}                                                                                                  \
                                                                                                   \
COLOR sample(GRADIENT2 gradient, float offset) {                                                   \
    float span = gradient.control_point2.offset - gradient.control_point1.offset;                  \
    float t    = clamp((offset - gradient.control_point1.offset)/span);                            \
    return COLOR_CONS(mix(gradient.control_point1.color.raw,gradient.control_point2.color.raw,t)); \
}                                                                                                  \
                                                                                                   \
COLOR sample(GRADIENT3 gradient, float offset) {                                                   \
    if (offset < gradient.control_point1.offset) {                                                 \
        return gradient.control_point1.color;                                                      \
    } else if (offset < gradient.control_point2.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point1,gradient.control_point2),offset);          \
    } else if (offset < gradient.control_point3.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point2,gradient.control_point3),offset);          \
    } else {                                                                                       \
        return gradient.control_point3.color;                                                      \
    }                                                                                              \
}                                                                                                  \
                                                                                                   \
COLOR sample(GRADIENT4 gradient, float offset) {                                                   \
    if (offset < gradient.control_point1.offset) {                                                 \
        return gradient.control_point1.color;                                                      \
    } else if (offset < gradient.control_point2.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point1,gradient.control_point2),offset);          \
    } else if (offset < gradient.control_point3.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point2,gradient.control_point3),offset);          \
    } else if (offset < gradient.control_point4.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point3,gradient.control_point4),offset);          \
    } else {                                                                                       \
        return gradient.control_point4.color;                                                      \
    }                                                                                              \
}                                                                                                  \
                                                                                                   \
COLOR sample(GRADIENT5 gradient, float offset) {                                                   \
    if (offset < gradient.control_point1.offset) {                                                 \
        return gradient.control_point1.color;                                                      \
    } else if (offset < gradient.control_point2.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point1,gradient.control_point2),offset);          \
    } else if (offset < gradient.control_point3.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point2,gradient.control_point3),offset);          \
    } else if (offset < gradient.control_point4.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point3,gradient.control_point4),offset);          \
    } else if (offset < gradient.control_point5.offset) {                                          \
        return sample(GRADIENT2(gradient.control_point4,gradient.control_point5),offset);          \
    } else {                                                                                       \
        return gradient.control_point5.color;                                                      \
    }                                                                                              \
}                                                                                                  \


DEF_GRADIENT(Rgb,rgb,RgbGradientControlPoint,RgbGradient1,RgbGradient2,RgbGradient3,RgbGradient4,RgbGradient5)
DEF_GRADIENT(Rgba,rgba,RgbaGradientControlPoint,RgbaGradient1,RgbaGradient2,RgbaGradient3,RgbaGradient4,RgbaGradient5)




//Rgba blend(Rgba bottom, Rgba top, float t) {
//    float weight = t * a(top);
//    float rgb    = mix(bottom.raw.rgb, top.raw.rgb, weight);
//    float alpha  = a(bottom) + a(top);
//}
