/** @file Debug-only functions, injected or stripped by the build tool as appropriate. */

export function assert(invariant: boolean, message: string) {
    if (!invariant) {
        console.error('assertion failed: ' + message)
    }
}
