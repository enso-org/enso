/** This module defines a simple set of animation utils. Follow the link to learn more:
 * https://easings.net/en */

// =================
// === Animation ===
// =================

export function ease_in_out_cubic(t: number): number {
    return t < 0.5 ? 4 * t * t * t : 1 - ((-2 * t + 2) * (-2 * t + 2) * (-2 * t + 2)) / 2
}

export function ease_in_out_quad(t: number): number {
    return t < 0.5 ? 2 * t * t : 1 - ((-2 * t + 2) * (-2 * t + 2)) / 2
}

export function ease_out_quart(t: number): number {
    return 1 - --t * t * t * t
}

export function easeOutElastic(x: number): number {
    const c4 = (2 * Math.PI) / 3

    return x === 0 ? 0 : x === 1 ? 1 : Math.pow(2, -10 * x) * Math.sin((x * 10 - 0.75) * c4) + 1
}

export function easeInOutElastic(x: number): number {
    const c5 = (2 * Math.PI) / 4.5

    return x === 0
        ? 0
        : x === 1
        ? 1
        : x < 0.5
        ? -(Math.pow(2, 20 * x - 10) * Math.sin((20 * x - 11.125) * c5)) / 2
        : (Math.pow(2, -20 * x + 10) * Math.sin((20 * x - 11.125) * c5)) / 2 + 1
}

// tpmt is two power minus ten times t scaled to [0,1]
export function tpmt(x: number) {
    return (Math.pow(2, -10 * x) - 0.0009765625) * 1.0009775171065494
}

const tau = 2 * Math.PI,
    amplitude = 1,
    period = 0.3

export const elasticOut = (function custom(a: number, p: number) {
    const s = Math.asin(1 / (a = Math.max(1, a))) * (p /= tau)

    function elasticOut(t: number) {
        return 1 - a * tpmt((t = +t)) * Math.sin((t + s) / p)
    }

    elasticOut.amplitude = function (a: number) {
        return custom(a, p * tau)
    }
    elasticOut.period = function (p: number) {
        return custom(a, p)
    }

    return elasticOut
})(amplitude, period)

export const elasticInOut = (function custom(a: number, p: number) {
    const s = Math.asin(1 / (a = Math.max(1, a))) * (p /= tau)

    function elasticInOut(t: number) {
        return (
            ((t = t * 2 - 1) < 0
                ? a * tpmt(-t) * Math.sin((s - t) / p)
                : 2 - a * tpmt(t) * Math.sin((s + t) / p)) / 2
        )
    }

    elasticInOut.amplitude = function (a: number) {
        return custom(a, p * tau)
    }
    elasticInOut.period = function (p: number) {
        return custom(a, p)
    }

    return elasticInOut
})(amplitude, period)
