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
export enum Platform {
  unknown = 'Unknown platform',
  windows = 'Windows',
  macOS = 'macOS',
  linux = 'Linux',
  windowsPhone = 'Windows Phone',
  iPhoneOS = 'iPhone OS',
  android = 'Android',
}

/**
 * The platform the app is currently running on.
 * This is used to determine whether `metaKey` or `ctrlKey` is used in shortcuts.
 */
export function platform() {
  if (isOnWindowsPhone()) {
    // MUST be before Android and Windows.
    return Platform.windowsPhone
  } else if (isOnWindows()) {
    return Platform.windows
  } else if (isOnIPhoneOS()) {
    // MUST be before macOS.
    return Platform.iPhoneOS
  } else if (isOnMacOS()) {
    return Platform.macOS
  } else if (isOnAndroid()) {
    // MUST be before Linux.
    return Platform.android
  } else if (isOnLinux()) {
    return Platform.linux
  } else {
    return Platform.unknown
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

/**
 * Return the platform the app is currently running on.
 * This is used to determine whether `metaKey` or `ctrlKey` is used in shortcuts.
 */
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
  return browser() === Browser.unknown
}

// ====================
// === Architecture ===
// ====================

let detectedArchitecture: string | null = null
// Only implemented by Chromium.
// @ts-expect-error This API exists, but no typings exist for it yet.
navigator.userAgentData?.getHighEntropyValues(['architecture']).then((values: unknown) => {
  if (
    typeof values === 'object' &&
    values != null &&
    'architecture' in values &&
    typeof values.architecture === 'string'
  ) {
    detectedArchitecture = String(values.architecture)
  }
})

/** Possible processor architectures. */
export enum Architecture {
  intel64 = 'x86_64',
  arm64 = 'arm64',
}

/** The processor architecture of the current system. */
export function architecture() {
  if (detectedArchitecture != null) {
    switch (detectedArchitecture) {
      case 'arm': {
        return Architecture.arm64
      }
      default: {
        return Architecture.intel64
      }
    }
  }
  switch (platform()) {
    case Platform.windows:
    case Platform.linux:
    case Platform.unknown: {
      return Architecture.intel64
    }
    case Platform.macOS:
    case Platform.iPhoneOS:
    case Platform.android:
    case Platform.windowsPhone: {
      // Assume the macOS device is on a M-series CPU.
      // This is highly unreliable, but operates under the assumption that all
      // new macOS devices will be ARM64.
      return Architecture.arm64
    }
  }
}

/** Whether the device has an Intel 64-bit CPU. */
export function isIntel64() {
  return architecture() === Architecture.intel64
}

/** Whether the device has an ARM 64-bit CPU. */
export function isArm64() {
  return architecture() === Architecture.arm64
}
