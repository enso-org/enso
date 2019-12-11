attribute vec2 position;
attribute vec2 texCoord;

uniform highp mat3 toWindow;
uniform highp vec2 clipLower;
uniform highp vec2 clipUpper;

varying vec2 vTexCoord;
varying vec4 vClipDistance;

void main() {
    highp vec3 positionOnWindow = toWindow * vec3(position, 1.0);
    vClipDistance.x = positionOnWindow.x - clipLower.x;
    vClipDistance.y = positionOnWindow.y - clipLower.y;
    vClipDistance.z = clipUpper.x - positionOnWindow.x;
    vClipDistance.w = clipUpper.y - positionOnWindow.y;

    vTexCoord = texCoord;
    gl_Position = vec4(positionOnWindow.xy, 0.0, positionOnWindow.z);
}
