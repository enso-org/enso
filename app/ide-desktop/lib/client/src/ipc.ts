/** @file Inter-Process communication configuration of the application. IPC allows the web-view
 * content to exchange information with the Electron application. */

// ===============
// === Channel ===
// ===============

/** Channel names used by the IPC protocol. */
export enum Channel {
    error = 'error',
    loadProfiles = 'load-profiles',
    profilesLoaded = 'profiles-loaded',
    saveProfile = 'save-profile',
    openGpuDebugInfo = 'open-debug-info-api',
    quit = 'quit-ide',
    /** Channel for requesting that a URL be opened by the system browser. */
    openUrlInSystemBrowser = 'open-url-in-system-browser',
    /** Channel for setting a callback that handles deep links to this application. */
    setDeepLinkHandler = 'set-deep-link-handler',
    /** Channel for signaling that a deep link to this application was opened. */
    openDeepLink = 'open-deep-link',
}
