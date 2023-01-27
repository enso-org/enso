/** @file Common math operations. */

// ============
// === Math ===
// ============

/** Converts the polar coordinates to cartesian ones. */
export function polarToCartesian(radius: number, angleDegrees: number): { x: number; y: number } {
    const angle = ((angleDegrees - 90) * Math.PI) / 180.0
    return {
        x: radius * Math.cos(angle),
        y: radius * Math.sin(angle),
    }
}

/** Format bytes as megabytes with a single precision number. */
export function formatMb(bytes: number): number {
    return Math.round((10 * bytes) / (1024 * 1024)) / 10
}
