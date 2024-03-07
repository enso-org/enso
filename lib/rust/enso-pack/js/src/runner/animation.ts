/** @file Easing is a method of distorting time to control apparent motion in animation. This module
 * defines a set of easing functions. To learn more about them and see graphs of how they behave,
 * see: https://github.com/d3/d3-ease. */

// =================
// === Constants ===
// =================

/** Two power minus ten times t scaled to [0,1]. */
export function tpmt(x: number) {
    return (Math.pow(2, -10 * x) - 0.0009765625) * 1.0009775171065494
}

const tau = 2 * Math.PI,
    amplitude = 1,
    period = 0.3

// =================
// === Animation ===
// =================

/** Symmetric quadratic easing.
 * To learn more, see: https://github.com/d3/d3-ease#easeQuadOut. */
export function easeInOutQuad(t: number): number {
    return t < 0.5 ? 2 * t * t : 1 - ((-2 * t + 2) * (-2 * t + 2)) / 2
}

/** Reverse elastic easing; equivalent to `1 - elasticIn(1 - t)`.
 * To learn more, see: https://github.com/d3/d3-ease#easeElasticOut. */
export const elasticOut = (function custom(a: number, p: number) {
    const s = Math.asin(1 / (a = Math.max(1, a))) * (p /= tau)

    const elasticOut = (t: number) => {
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

/** Symmetric elastic easing; scales elasticIn for t in [0, 0.5] and elasticOut for t in [0.5, 1].
 * To learn more, see: https://github.com/d3/d3-ease#easeElasticInOut. */
export const elasticInOut = (function custom(a: number, p: number) {
    const s = Math.asin(1 / (a = Math.max(1, a))) * (p /= tau)

    const elasticInOut = (t: number) => {
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
