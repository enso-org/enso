/// A zoom level. Zoom of 1.0 means that all figures on the screen will be displayed with their
/// native size. Zoom of 2.0 means that all figures are scaled down twice.
float zoom() {
    return input_z_zoom_1 / input_local.z;
}

/// We are adding a side padding to all sprites when drawing shapes on the in order to make
/// anti-aliasing working properly. Consider a situation when a box moves 0.5 pixels to the right.
/// Then the pixel with the x-coordinate 0 should be half-transparent. The pixel on the left from it
/// should be half-transparent as well. However, it will not exist without the padding.
///
/// Also, the padding depends on the zoom level. When shapes are zoomed out 2 times, the screen
/// pixel will display 2 pixels of the sprite canvas.
float aa_side_padding() {
    return max(1.0, ceil(1.0 / zoom()));
}

bool outside_of_uv() {
    return input_uv.x < 0.0 || input_uv.x > 1.0 || input_uv.y < 0.0 || input_uv.y > 1.0;
}
