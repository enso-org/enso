/// This code is the body of the fragment shader main function of a GLSL shape.



// ============
// === Init ===
// ============

Env   env      = Env(1);
vec2  position = input_local.xy ;
Shape shape    = run(env, position);
float alpha    = shape.color.color.raw.a;



// ===========================
// === Object ID Rendering ===
// ===========================

float alpha_no_aa = alpha > ID_ALPHA_THRESHOLD ? 1.0 : 0.0;

if (pointer_events_enabled) {
    output_id = encode(input_global_instance_id,alpha_no_aa);
}



// =======================
// === Color Rendering ===
// =======================

if (input_display_mode == DISPLAY_MODE_NORMAL) {
    output_color      = srgba(unpremultiply(shape.color)).raw;
    output_color.rgb *= alpha;

} else if (input_display_mode == DISPLAY_MODE_DEBUG_SHAPE_AA_SPAN) {
   output_color = srgba(unpremultiply(shape.color)).raw;
   output_color.rgb *= alpha;
   if (input_uv.x < 0.0 || input_uv.x > 1.0 || input_uv.y < 0.0 || input_uv.y > 1.0) {
       output_color = vec4(1.0,0.0,0.0,1.0);
   }

} else if (input_display_mode == DISPLAY_MODE_DEBUG_SDF) {
    float zoom        = zoom();
    float factor      = 200.0/zoom * input_pixel_ratio;
    Rgb col           = distance_meter(shape.sdf.distance, factor, factor);
    output_color      = rgba(col).raw;
    output_color.a    = alpha_no_aa;
    output_color.rgb *= alpha_no_aa;

} else if (input_display_mode == DISPLAY_MODE_DEBUG_INSTANCE_ID) {
      float object_hue  = float((input_global_instance_id * 7) % 100) / 100.0;
      Srgb object_color = srgb(hsv(object_hue, 1.0, 0.5));
      output_color.rgb  = object_color.raw.rgb;
      output_color.a    = alpha_no_aa;
      output_color.rgb *= alpha_no_aa;

} else if (input_display_mode == DISPLAY_MODE_DEBUG_SPRITE_UV) {
       float b = 0.0;
       if (input_uv.x >= 1.0 || input_uv.y >= 1.0) {
           b= .5;
       };
       output_color = vec4(input_uv, b, 1.0);

} else {
    // Unknown code, display everything in red.
    output_color = vec4(1.0, 0.0, 0.0, 1.0);
}
