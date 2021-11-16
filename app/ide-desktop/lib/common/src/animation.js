/// This module defines a simple set of animation utils. Follow the link to learn more:
/// https://easings.net/en

// =================
// === Animation ===
// =================

export function ease_in_out_cubic(t) {
    return t < 0.5 ? 4 * t * t * t : 1 - ((-2 * t + 2) * (-2 * t + 2) * (-2 * t + 2)) / 2
}

export function ease_in_out_quad(t) {
    return t < 0.5 ? 2 * t * t : 1 - ((-2 * t + 2) * (-2 * t + 2)) / 2
}

export function ease_out_quart(t) {
    return 1 - --t * t * t * t
}
