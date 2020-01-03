#version 300 es

in highp vec4 v_clip_distance;

uniform highp vec4 color;

out highp vec4 out_color;

void main() {
    bool clipped = any(lessThan(v_clip_distance, vec4(0.0)));
    if (clipped) {
        discard;
    } else {
        out_color = color;
    }
}
