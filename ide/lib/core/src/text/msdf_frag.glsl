#extension GL_OES_standard_derivatives : enable

varying highp vec2 vTexCoord;
varying highp vec4 vClipDistance;

uniform sampler2D   msdf;
// Number of MSDF cells in row and column
uniform highp vec2  msdfSize;
// Range parameter of msdf generation - the distance between 0.0 and 1.0 values
// expressed in MSDF cells
uniform highp float range;
uniform highp vec4  color;

highp float median(highp vec3 v) {
    return max(min(v.x, v.y), min(max(v.x, v.y), v.z));
}

void main() {
    bool clipped              = any(lessThan(vClipDistance, vec4(0.0)));
    highp vec2  msdfUnitTex   = range/msdfSize;
    highp vec2  msdfUnitPx    = msdfUnitTex/fwidth(vTexCoord);
    highp float avgMsdfUnitPx = (msdfUnitPx.x + msdfUnitPx.y) / 2.0;
    // Note [dpiDilate]
    highp float dpiDilate     = avgMsdfUnitPx < range*0.49 ? 1.0 : 0.0;

    if (clipped) {
        discard;
    } else {
        highp vec3  msdfSample    = texture2D(msdf, vTexCoord).rgb;
        highp float sigDist       = median(msdfSample) - 0.5;
        highp float sigDistPx     = sigDist * avgMsdfUnitPx;
        highp float opacity       = 0.5 + sigDistPx + dpiDilate*0.1;
        gl_FragColor = vec4(color.xyz, color.w * clamp(opacity, 0.0, 1.0));
    }
}

/* Note [dpiDilate]
 *
 * This is 1.0 on low dpi and 0.0 otherwise. We use this parameter to fatten
 * somewhat font on low resolutions. The thershold and exact value of this
 * fattening was picked by trial an error, searching for best rendering effect.
 */
