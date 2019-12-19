#version 300 es

in vec2 position;
in vec2 tex_coord;

uniform highp mat3 to_scene;
uniform highp vec2 clip_lower;
uniform highp vec2 clip_upper;

out vec2 v_tex_coord;
out vec4 v_clip_distance;

void main() {
    highp vec3 position_on_scene = to_scene * vec3(position, 1.0);
    v_clip_distance.x = position_on_scene.x - clip_lower.x;
    v_clip_distance.y = position_on_scene.y - clip_lower.y;
    v_clip_distance.z = clip_upper.x - position_on_scene.x;
    v_clip_distance.w = clip_upper.y - position_on_scene.y;

    v_tex_coord = tex_coord;
    gl_Position = vec4(position_on_scene.xy, 0.0, position_on_scene.z);
}
