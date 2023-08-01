/** @file A function that generates a unique string. */

// ====================
// === uniqueString ===
// ====================

// This is initialized to an unusual number, to minimize the chances of collision.
let counter = Number(new Date()) >>> 2

/** Returns a new, mostly unique string. */
export function uniqueString(): string {
    counter += 1
    return counter.toString()
}
