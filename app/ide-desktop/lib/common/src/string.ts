/** @file String helpers. */

// ======================
// === String Helpers ===
// ======================

/** Get the common prefix of the two strings. */
export function getCommonPrefix(a: string, b: string): string {
    let i = 0
    while (i < a.length && i < b.length && a[i] === b[i]) {
        i++
    }
    return a.slice(0, i)
}
