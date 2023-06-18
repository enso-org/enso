/** @file Helper functions for environment detection. */

// ===========================
// === isRunningInElectron ===
// ===========================

/** Returns `true` if running in Electron, else `false`.
 * This is used to determine whether to use a `MemoryRouter` (stores history in an array)
 * or a `BrowserRouter` (stores history in the path of the URL).
 * It is also used to determine whether to send custom state to Amplify for a workaround. */
export function isRunningInElectron() {
    return /electron/i.test(navigator.userAgent)
}
