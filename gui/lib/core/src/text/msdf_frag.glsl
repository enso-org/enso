#extension GL_OES_standard_derivatives : enable

varying highp vec2 vTexCoord;
varying highp vec2 msdfCoord;

uniform sampler2D msdf;
uniform highp vec2 msdfSize;
uniform highp float pxRange;
uniform highp vec4 bgColor;
uniform highp vec4 fgColor;

highp float median(highp float r, highp float g, highp float b) {
    return max(min(r, g), min(max(r, g), b));
}

void main() {
    highp vec2 msdfUnitTex = pxRange/msdfSize;
    highp vec2 msdfUnitPx = msdfUnitTex/fwidth(vTexCoord);
    highp vec3 smple = texture2D(msdf, vTexCoord).rgb;
    highp float sigDist = median(smple.r, smple.g, smple.b) - 0.5;

    highp float dpiDilate = (msdfUnitPx.x + msdfUnitPx.y) < pxRange*0.9 ? 1.0 : 0.0;

    highp float sigDistPx = sigDist*((msdfUnitPx.x + msdfUnitPx.y)/2.0);
    highp float opacity = clamp(sigDistPx + 0.5 + dpiDilate*0.1, 0.0, 1.0);
    gl_FragColor = mix(bgColor, fgColor, opacity);
//    gl_FragColor = vec4(sigDist+0.5, dpiDilate, 0.0, 1.0);
}
