highp float median(highp vec3 v) {
    return max(min(v.x, v.y), min(max(v.x, v.y), v.z));
}

/// Compute the uv coordinates of the MSDF texture fragment where it should be sampled.
///
/// Essentially, it's an input_uv which is a bit transformed to "cut off" the half of the MSDF cell from each side. This
/// way we have better pixel alignment on low resolutions.
highp vec2 uv_to_msdf_uv(vec2 uv) {
    return uv;
    // highp vec2 msdf_cell_size = 1.0 / input_msdf_size;
    // highp vec2 offset = msdf_cell_size / 2.0;
    // highp vec2 scale = 1.0 - msdf_cell_size;
    // return offset + uv * scale;
}

highp vec2 uv_to_texture_coord(vec2 uv) {
    highp vec2 texture_glyph_offset = input_msdf_size / vec2(textureSize(input_atlas, 0));
    highp vec2 offset = vec2(0.0, input_atlas_index) * texture_glyph_offset;
    return offset + uv_to_msdf_uv(uv) * texture_glyph_offset;
}

highp float get_fatting() {
    highp vec2 local_to_px_ratio = 1.0 / fwidth(input_local.xy);
    highp float font_size_px = input_font_size * (local_to_px_ratio.x + local_to_px_ratio.y) / 2.0;
    highp float fatting = input_sdf_weight;
    return font_size_px * fatting;
}

highp float get_alpha(vec2 uv) {
    highp vec2  tex_coord = uv_to_texture_coord(uv);
    highp vec2  msdf_unit_tex = input_msdf_range / vec2(textureSize(input_atlas, 0));
    highp vec2  msdf_unit_px = msdf_unit_tex / fwidth(tex_coord);
    highp float avg_msdf_unit_px = (msdf_unit_px.x + msdf_unit_px.y) / 2.0;

    // We use this parameter to fatten somewhat font on low resolutions. The threshold and exact
    // value of this fattening was picked by trial an error, searching for best rendering effect.
    // highp float dpi_dilate  = avg_msdf_unit_px < input_msdf_range*0.49 ? 0.08 : 0.0;
    highp float dpi_dilate  = 0.0; // input_msdf_range*0.49;
    highp vec3  msdf_sample = texture(input_atlas,tex_coord).rgb;
    highp float sig_dist    = median(msdf_sample) - 0.5;
    highp float sig_dist_px = sig_dist * avg_msdf_unit_px + get_fatting();
    highp float opacity     = 0.5 + sig_dist_px + dpi_dilate;
    opacity += input_opacity_increase;
    opacity = clamp(opacity, 0.0, 1.0);
    opacity = pow(opacity, input_opacity_exponent);
    return opacity;
}

highp vec4 color_from_msdf() {
    highp vec4 color = input_color;
    color.a *= get_alpha(input_uv);
    color.rgb *= color.a; // premultiply

    return color;
    // vec4 color2 = vec4(input_uv * input_size / 10.0, 0.0, 1.0);
    // return (color * 0.7 + color2 * 0.3);
}
