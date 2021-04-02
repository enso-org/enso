highp float median(highp vec3 v) {
    return max(min(v.x, v.y), min(max(v.x, v.y), v.z));
}

highp vec2 get_scaled_uv() {
    highp vec2 msdf_cell_size = 1.0/input_msdf_size;
    highp vec2 offset         = msdf_cell_size/2.0;
    highp vec2 scale          = 1.0 - msdf_cell_size;
    return offset + input_uv * scale;
}

highp vec2 get_texture_coord() {
    highp vec2 msdf_fragment_size = input_msdf_size / vec2(textureSize(input_atlas,0));
    highp vec2 offset             = vec2(0.0, input_atlas_index) * msdf_fragment_size;
    return offset + get_scaled_uv() * msdf_fragment_size;
}

// FIXME
// The following function uses non-standard font adjustiments (lines marked with FIXME). They make
// the font bolder and more crisp. It was designed to look nice on nodes in the GUI but leaves the
// fonts with a non-standard look (not the one defined by the font author). This should be
// revisited, generalized, and refactored out in the future.
highp float msdf_alpha() {
    highp vec2  tex_coord        = get_texture_coord();
    highp vec2  msdf_unit_tex    = input_msdf_range / vec2(textureSize(input_atlas,0));
    highp vec2  msdf_unit_px     = msdf_unit_tex / fwidth(tex_coord);
    highp float avg_msdf_unit_px = (msdf_unit_px.x + msdf_unit_px.y) / 2.0;

    // We use this parameter to fatten somewhat font on low resolutions. The thershold and exact
    // value of this fattening was picked by trial an error, searching for best rendering effect.
    highp float dpi_dilate       = avg_msdf_unit_px < input_msdf_range*0.49 ? 1.0 : 0.0;

    highp vec3  msdf_sample      = texture(input_atlas,tex_coord).rgb;
    highp float sig_dist         = median(msdf_sample) - 0.5;
    highp float sig_dist_px      = sig_dist * avg_msdf_unit_px;
    highp float opacity          = 0.5 + sig_dist_px + dpi_dilate * 0.08;
    opacity += 0.6;                      // FIXME: Widen + sharpen
    opacity = clamp(opacity, 0.0, 1.0);
    opacity = pow(opacity,3.0);          // FIXME: sharpen
    return opacity;
}

highp vec4 color_from_msdf() {
    highp vec4 color = input_color;
    color.a *= msdf_alpha();
    color.rgb *= color.a; // premultiply
    return color;
}
