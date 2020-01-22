/// This code is the body of the fragment shader main function of a GLSL shape.

Env   env      = Env(1);
vec2  position = input_local.xy ;
Shape shape    = run(env,position);
float alpha    = render(shape);

if (input_display_mode == 0) {
    output_color = vec4(1.0,0.0,0.0,alpha);
    output_color.r *= alpha;
    output_color.g *= alpha;
    output_color.b *= alpha;
} else if (input_display_mode == 1) {
    RGB col = distance_meter(shape.sdf.distance, 200.0 * input_zoom * input_pixel_ratio, 200.0/input_zoom * input_pixel_ratio);
    output_color = rgba(col).raw;
}
