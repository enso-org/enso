const bool DEBUG = false;

highp float median(highp vec3 v) {
    return max(min(v.x, v.y), min(max(v.x, v.y), v.z));
}

highp float get_fatting() {
    highp vec2 local_to_px_ratio = 1.0 / fwidth(input_local.xy);
    highp float font_size_px = input_font_size * (local_to_px_ratio.x + local_to_px_ratio.y) * 0.5;
    highp float fatting = input_sdf_weight;
    return font_size_px * fatting;
}

highp float get_alpha(vec2 uv) {
    highp vec2  msdf_unit_px = input_msdf_range / (fwidth(uv) * vec2(input_msdf_size));
    highp float avg_msdf_unit_px = (msdf_unit_px.x + msdf_unit_px.y) * 0.5;

    highp vec3  msdf_sample = texture(input_atlas,vec3(uv, input_atlas_index)).rgb;
    highp float sig_dist = median(msdf_sample) - 0.5;
    highp float sig_dist_px = sig_dist * avg_msdf_unit_px + get_fatting();
    highp float opacity = 0.5 + sig_dist_px;
    opacity += input_opacity_increase;
    opacity = clamp(opacity, 0.0, 1.0);
    opacity = pow(opacity, input_opacity_exponent);
    return opacity;
}

highp vec4 color_from_msdf() {
    highp vec4 color = input_color;
    color.a *= get_alpha(input_uv);
    color.rgb *= color.a; // premultiply

    if(DEBUG) {
        vec4 bg_box = vec4(input_uv * input_size / 10.0, 0.0, 1.0);
        color = (color * 0.7 + bg_box * 0.3);
    }
    return color;

}
