function arr_to_css_matrix3d(a) {
    return "matrix3d(" + a.join(',') + ")"
}

export function set_object_transform(dom, matrix_array) {
    let css = arr_to_css_matrix3d(matrix_array);
    dom.style.transform = "translate(-50%, -50%)" + css;
}

export function setup_perspective(dom, znear) {
    dom.style.perspective = znear + "px";
}

export function setup_camera_transform
(dom, znear, half_width, half_height, matrix_array) {
    let translateZ  = "translateZ(" + znear + "px)";
    let matrix3d    = arr_to_css_matrix3d(matrix_array);
    let translate2d = "translate(" + half_width + "px, " + half_height + "px)";
    let transform   = translateZ + matrix3d + translate2d;
    dom.style.transform = transform;
}
