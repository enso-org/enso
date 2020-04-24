/// This code is the body of the fragment shader main function of a GLSL shape.

Env   env        = Env(1);
vec2  position   = input_local.xy ;
Shape shape      = run(env,position);
float alpha      = shape.color.color.raw.a;



// ===========================
// === Object ID Rendering ===
// ===========================

uvec3 chunks = encode(input_symbol_id,input_instance_id);

float alpha_no_aa = alpha > 0.5 ? 1.0 : 0.0;

output_id = vec4(as_float_u8(chunks.x),as_float_u8(chunks.y),as_float_u8(chunks.z),alpha_no_aa);
output_id.r *= alpha_no_aa;
output_id.g *= alpha_no_aa;
output_id.b *= alpha_no_aa;



// =======================
// === Color Rendering ===
// =======================

if (input_display_mode == 0) {
    output_color = srgba(unpremultiply(shape.color)).raw;
    output_color.rgb *= alpha;
} else if (input_display_mode == 1) {
    Rgb col = distance_meter(shape.sdf.distance, 200.0 * input_zoom * input_pixel_ratio, 200.0/input_zoom * input_pixel_ratio);
    output_color = rgba(col).raw;
} else if (input_display_mode == 2) {
    float object_hue  = float((input_instance_id * 7) % 100) / 100.0;
    Srgb object_color = srgb(hsv(object_hue, 1.0, 0.5));
    output_color.rgb  = object_color.raw.rgb;
    output_color.a    = float(alpha_no_aa);
    output_color.rgb *= float(alpha_no_aa);
}
