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

// ================
// === Platform ===
// ================

/** Possible platforms that the app may run on. */
export enum Platform {
    unknown = 'Unknown platform',
    windows = 'Windows',
    macOS = 'macOS',
    linux = 'Linux',
}

/** Returns the platform the app is currently running on.
 * This is used to determine whether `metaKey` or `ctrlKey` is used in shortcuts. */
export function platform(): Platform {
    if (/windows/i.test(navigator.userAgent)) {
        return Platform.windows
    } else if (/mac os/i.test(navigator.userAgent)) {
        return Platform.macOS
    } else if (/linux/i.test(navigator.userAgent)) {
        return Platform.linux
    } else {
        return Platform.unknown
    }
}
