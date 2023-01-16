/** @file A set of utils for generating and modifying the SVG images. */

import * as math from 'runner/math'

// ===========
// === SVG ===
// ===========

/** Defines a new SVG with the provided source. */
export function newSvg(width: number, height: number, str: string): string {
    return `
    <svg xmlns="http://www.w3.org/2000/svg"
         xmlns:xlink="http://www.w3.org/1999/xlink"
         height="${height}" width="${width}" viewBox="0 0 ${height} ${width}">
    ${str}
    </svg>`
}

/** Returns SVG code for an arc with a defined radius and angle. */
export function arc(radius: number, endAngle: number): string {
    let startAngle = 0
    if (endAngle < 0) {
        startAngle = endAngle
        endAngle = 0
    }
    const start = math.polarToCartesian(radius, endAngle)
    const end = math.polarToCartesian(radius, startAngle)
    const largeArc = endAngle - startAngle <= 180 ? '0' : '1'
    return `M 0 0 L ${start.x} ${start.y} A ${radius} ${radius} 0 ${largeArc} 0 ${end.x} ${end.y}`
}
