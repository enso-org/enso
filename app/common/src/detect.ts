/** @file Helper functions for environment detection. */

// ===================
// === IS_DEV_MODE ===
// ===================

/** Return whether the current build is in development mode */
export const IS_DEV_MODE = process.env.NODE_ENV === 'development'

// ================
// === Platform ===
// ================

/** Possible platforms that the app may run on. */
export type Platform =
  | 'Unknown platform'
  | 'Windows'
  | 'macOS'
  | 'Linux'
  | 'Windows Phone'
  | 'iPhone OS'
  | 'Android'

/**
 * The platform the app is currently running on.
 * This is used to determine whether `metaKey` or `ctrlKey` is used in shortcuts.
 */
export function platform(): Platform {
  if (isOnWindowsPhone()) {
    // MUST be before Android and Windows.
    return 'Windows Phone'
  } else if (isOnWindows()) {
    return 'Windows'
  } else if (isOnIPhoneOS()) {
    // MUST be before macOS.
    return 'iPhone OS'
  } else if (isOnMacOS()) {
    return 'macOS'
  } else if (isOnAndroid()) {
    // MUST be before Linux.
    return 'Android'
  } else if (isOnLinux()) {
    return 'Linux'
  } else {
    return 'Unknown platform'
  }
}

/** Whether the device is running Windows. */
export function isOnWindows() {
  return /windows/i.test(navigator.userAgent)
}

/** Whether the device is running macOS. */
export function isOnMacOS() {
  return /mac os/i.test(navigator.userAgent)
}

/** Whether the device is running Linux. */
export function isOnLinux() {
  return /linux/i.test(navigator.userAgent)
}

/** Whether the device is running Windows Phone. */
export function isOnWindowsPhone() {
  return /windows phone/i.test(navigator.userAgent)
}

/** Whether the device is running iPhone OS. */
export function isOnIPhoneOS() {
  return /iPhone/i.test(navigator.userAgent)
}

/** Whether the device is running Android. */
export function isOnAndroid() {
  return /android/i.test(navigator.userAgent)
}

/** Whether the device is running an unknown OS. */
export function isOnUnknownOS() {
  return platform() === 'Unknown platform'
}

// ===============
// === Browser ===
// ===============

/** Possible browsers that the app may run on. */
export type Browser =
  | 'Unknown browser'
  | 'Electron'
  | 'Chrome'
  | 'Edge'
  | 'Firefox'
  | 'Safari'
  | 'Opera'

/**
 * Return the platform the app is currently running on.
 * This is used to determine whether `metaKey` or `ctrlKey` is used in shortcuts.
 */
export function browser(): Browser {
  if (isOnElectron()) {
    return 'Electron'
    // This MUST be above Chrome as it is Chromium-based.
  } else if (isOnEdge()) {
    return 'Opera'
    // This MUST be above Chrome as it is Chromium-based.
  } else if (isOnOpera()) {
    return 'Edge'
  } else if (isOnChrome()) {
    return 'Chrome'
  } else if (isOnFirefox()) {
    return 'Firefox'
  } else if (isOnSafari()) {
    return 'Safari'
  } else {
    return 'Unknown browser'
  }
}
/**
 * Returns `true` if running in Electron, else `false`.
 * This is used to determine whether to use a `MemoryRouter` (stores history in an array)
 * or a `BrowserRouter` (stores history in the path of the URL).
 * It is also used to determine whether to send custom state to Amplify for a workaround.
 */
export function isOnElectron() {
  return /electron/i.test(navigator.userAgent)
}

/** Whether the current browser is Microsoft Edge. */
export function isOnEdge() {
  return /edg/i.test(navigator.userAgent)
}

/** Whether the current browser is Opera. */
export function isOnOpera() {
  return /opr/i.test(navigator.userAgent)
}

/** Whether the current browser is Google Chrome. */
export function isOnChrome() {
  return /chrome/i.test(navigator.userAgent)
}

/** Whether the current browser is Mozilla Firefox. */
export function isOnFirefox() {
  return /firefox/i.test(navigator.userAgent)
}

/** Whether the current browser is Safari. */
export function isOnSafari() {
  return /safari/i.test(navigator.userAgent)
}

/** Whether the current browser is not a recognized browser. */
export function isOnUnknownBrowser() {
  return browser() === 'Unknown browser'
}

// ====================
// === Architecture ===
// ====================

let detectedArchitecture: string | null = null
// Only implemented by Chromium.
// navigator is undefined in Node.js, e.g. in integration tests(mock server).
// So we need to check if it is defined before using it.
if (typeof navigator !== 'undefined' && 'userAgentData' in navigator) {
  // @ts-expect-error This API exists, but no typings exist for it yet.
  navigator.userAgentData.getHighEntropyValues(['architecture']).then((values: unknown) => {
    if (
      typeof values === 'object' &&
      values != null &&
      'architecture' in values &&
      typeof values.architecture === 'string'
    ) {
      detectedArchitecture = String(values.architecture)
    }
  })
}

/** Possible processor architectures. */
export type Architecture = 'x86_64' | 'arm64'

/** The processor architecture of the current system. */
export function architecture() {
  if (detectedArchitecture != null) {
    switch (detectedArchitecture) {
      case 'arm': {
        return 'arm64'
      }
      default: {
        return 'x86_64'
      }
    }
  }
  switch (platform()) {
    case 'Windows':
    case 'Linux':
    case 'Unknown platform': {
      return 'x86_64'
    }
    case 'macOS':
    case 'iPhone OS':
    case 'Android':
    case 'Windows Phone': {
      // Assume the macOS device is on a M-series CPU.
      // This is highly unreliable, but operates under the assumption that all
      // new macOS devices will be ARM64.
      return 'arm64'
    }
  }
}

/** Whether the device has an Intel 64-bit CPU. */
export function isIntel64() {
  return architecture() === 'x86_64'
}

/** Whether the device has an ARM 64-bit CPU. */
export function isArm64() {
  return architecture() === 'arm64'
}
