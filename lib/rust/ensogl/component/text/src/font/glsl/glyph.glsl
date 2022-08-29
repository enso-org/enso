highp float median(highp vec3 v) {
    return max(min(v.x, v.y), min(max(v.x, v.y), v.z));
}

/// Compute the uv coordinates of the MSDF texture fragment where it should be sampled.
///
/// Essentially, it's an input_uv which is a bit transformed to "cut off" the half of the MSDF cell from each side. This
/// way we have better pixel alignment on low resolutions.
highp vec2 msdf_fragment_uv() {
    highp vec2 msdf_cell_size = 1.0/input_msdf_size;
    highp vec2 offset         = msdf_cell_size/2.0;
    highp vec2 scale          = 1.0 - msdf_cell_size;
    return offset + input_uv * scale;
}

highp vec2 get_texture_coord() {
    highp vec2 msdf_fragment_size = input_msdf_size / vec2(textureSize(input_atlas, 0));
    highp vec2 offset             = vec2(0.0, input_atlas_index) * msdf_fragment_size;
    return offset + msdf_fragment_uv() * msdf_fragment_size;
}

highp float get_fatting() {
    highp vec2  local_to_px_ratio = 1.0 / fwidth(input_local.xy);
    highp float font_size_px      = input_font_size * (local_to_px_ratio.x + local_to_px_ratio.y) / 2.0;
    highp float fatting           = input_sdf_bold;
    return font_size_px * fatting;
}

highp float msdf_alpha() {
    highp vec2  tex_coord        = get_texture_coord();
    highp vec2  msdf_unit_tex    = input_msdf_range / vec2(textureSize(input_atlas,0));
    highp vec2  msdf_unit_px     = msdf_unit_tex / fwidth(tex_coord);
    highp float avg_msdf_unit_px = (msdf_unit_px.x + msdf_unit_px.y) / 2.0;

    // We use this parameter to fatten somewhat font on low resolutions. The thershold and exact
    // value of this fattening was picked by trial an error, searching for best rendering effect.
    highp float dpi_dilate  = avg_msdf_unit_px < input_msdf_range*0.49 ? 1.0 : 0.0;
    highp vec3  msdf_sample = texture(input_atlas,tex_coord).rgb;
    highp float sig_dist    = median(msdf_sample) - 0.5;
    highp float sig_dist_px = sig_dist * avg_msdf_unit_px + get_fatting();
    highp float opacity     = 0.5 + sig_dist_px + dpi_dilate * 0.08;
    opacity = clamp(opacity, 0.0, 1.0);
    return opacity;
}

highp vec4 color_from_msdf() {
    highp vec4 color = input_color;
    color.a *= msdf_alpha();
    color.rgb *= color.a; // premultiply
    return color;
}
