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

/** Return the platform the app is currently running on.
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

/** Return whether the device is running Windows. */
export function isOnWindows() {
    return platform() === Platform.windows
}

/** Return whether the device is running macOS. */
export function isOnMacOS() {
    return platform() === Platform.macOS
}

/** Return whether the device is running Linux. */
export function isOnLinux() {
    return platform() === Platform.linux
}

/** Return whether the device is running an unknown OS. */
export function isOnUnknownOS() {
    return platform() === Platform.unknown
}
