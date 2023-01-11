/// This module defines a common math operations.

// ============
// === Math ===
// ============

/// Converts the polar coordinates to cartesian ones.
export function polar_to_cartesian(
    radius: number,
    angle_degrees: number
): { x: number; y: number } {
    const angle = ((angle_degrees - 90) * Math.PI) / 180.0
    return {
        x: radius * Math.cos(angle),
        y: radius * Math.sin(angle),
    }
}

/// Format bytes as megabytes with a single precision number.
export function format_mb(bytes: number): number {
    return Math.round((10 * bytes) / (1024 * 1024)) / 10
}
