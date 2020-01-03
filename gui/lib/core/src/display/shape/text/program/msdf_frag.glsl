#version 300 es

in highp vec2 v_tex_coord;
in highp vec4 v_clip_distance;

uniform sampler2D   msdf;
// Number of MSDF cells in row and column
uniform highp vec2  msdf_size;
// Range parameter of msdf generation - the distance between 0.0 and 1.0 values
// expressed in MSDF cells
uniform highp float range;
uniform highp vec4  color;

out highp vec4 out_color;

highp float median(highp vec3 v) {
    return max(min(v.x, v.y), min(max(v.x, v.y), v.z));
}

void main() {
    bool clipped                 = any(lessThan(v_clip_distance, vec4(0.0)));
    highp vec2  msdf_unit_tex    = range / msdf_size;
    highp vec2  msdf_unit_px     = msdf_unit_tex/  fwidth(v_tex_coord);
    highp float avg_msdf_unit_px = (msdf_unit_px.x + msdf_unit_px.y) / 2.0;
    // Note [dpiDilate]
    highp float dpi_dilate       = avg_msdf_unit_px < range*0.49 ? 1.0 : 0.0;

    if (clipped) {
        discard;
    } else {
        highp vec3  msdf_sample  = texture(msdf, v_tex_coord).rgb;
        highp float sig_dist     = median(msdf_sample) - 0.5;
        highp float sig_dist_px  = sig_dist * avg_msdf_unit_px;
        highp float opacity      = 0.5 + sig_dist_px + dpi_dilate * 0.08;
        out_color = vec4(color.xyz, color.w * clamp(opacity, 0.0, 1.0));
    }
}

/* Note [dpiDilate]
 *
 * This is 1.0 on low dpi and 0.0 otherwise. We use this parameter to fatten
 * somewhat font on low resolutions. The thershold and exact value of this
 * fattening was picked by trial an error, searching for best rendering effect.
 */
