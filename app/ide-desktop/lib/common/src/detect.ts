/** @file Helper functions for environment detection. */

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
    if (isOnWindows()) {
        return Platform.windows
    } else if (isOnMacOS()) {
        return Platform.macOS
    } else if (isOnLinux()) {
        return Platform.linux
    } else {
        return Platform.unknown
    }
}

/** Return whether the device is running Windows. */
export function isOnWindows() {
    return /windows/i.test(navigator.userAgent)
}

/** Return whether the device is running macOS. */
export function isOnMacOS() {
    return /mac os/i.test(navigator.userAgent)
}

/** Return whether the device is running Linux. */
export function isOnLinux() {
    return /linux/i.test(navigator.userAgent)
}

/** Return whether the device is running an unknown OS. */
export function isOnUnknownOS() {
    return platform() === Platform.unknown
}

// ===============
// === Browser ===
// ===============

/** Possible browsers that the app may run on. */
export enum Browser {
    unknown = 'Unknown browser',
    electron = 'Electron',
    chrome = 'Chrome',
    edge = 'Edge',
    firefox = 'Firefox',
    safari = 'Safari',
    opera = 'Opera',
}

/** Return the platform the app is currently running on.
 * This is used to determine whether `metaKey` or `ctrlKey` is used in shortcuts. */
export function browser(): Browser {
    if (isOnElectron()) {
        return Browser.electron
        // This MUST be above Chrome as it is Chromium-based.
    } else if (isOnEdge()) {
        return Browser.opera
        // This MUST be above Chrome as it is Chromium-based.
    } else if (isOnOpera()) {
        return Browser.edge
    } else if (isOnChrome()) {
        return Browser.chrome
    } else if (isOnFirefox()) {
        return Browser.firefox
    } else if (isOnSafari()) {
        return Browser.safari
    } else {
        return Browser.unknown
    }
}
/** Returns `true` if running in Electron, else `false`.
 * This is used to determine whether to use a `MemoryRouter` (stores history in an array)
 * or a `BrowserRouter` (stores history in the path of the URL).
 * It is also used to determine whether to send custom state to Amplify for a workaround. */
export function isOnElectron() {
    return /electron/i.test(navigator.userAgent)
}

/** Return whether the current browser is Microsoft Edge. */
export function isOnEdge() {
    return /edg/i.test(navigator.userAgent)
}

/** Return whether the current browser is Opera. */
export function isOnOpera() {
    return /opr/i.test(navigator.userAgent)
}

/** Return whether the current browser is Google Chrome. */
export function isOnChrome() {
    return /chrome/i.test(navigator.userAgent)
}

/** Return whether the current browser is Mozilla Firefox. */
export function isOnFirefox() {
    return /firefox/i.test(navigator.userAgent)
}

/** Return whether the current browser is Safari. */
export function isOnSafari() {
    return /safari/i.test(navigator.userAgent)
}

/** Return whether the current browser is not a recognized browser. */
export function isOnUnknownBrowser() {
    return browser() === Browser.unknown
}
