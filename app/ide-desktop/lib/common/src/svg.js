/// This module defines a set of utils for generating and modifying the SVG images.

import * as math from './math.js'

// ===========
// === SVG ===
// ===========

/// Defines a new SVG with the provided source.
export function new_svg(width, height, str) {
    return `
    <svg version="1.1" baseProfile="full" xmlns="http://www.w3.org/2000/svg"
         xmlns:xlink="http://www.w3.org/1999/xlink"
         height="${height}" width="${width}" viewBox="0 0 ${height} ${width}">
    ${str}
    </svg>`
}

/// Returns SVG code for an arc with a defined radius and angle.
export function arc(radius, end_angle) {
    let start_angle = 0
    if (end_angle < 0) {
        start_angle = end_angle
        end_angle = 0
    }
    let start = math.polar_to_cartesian(radius, end_angle)
    let end = math.polar_to_cartesian(radius, start_angle)
    let large_arc = end_angle - start_angle <= 180 ? '0' : '1'
    return `M 0 0 L ${start.x} ${start.y} A ${radius} ${radius} 0 ${large_arc} 0 ${end.x} ${end.y}`
}
