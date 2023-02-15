/** @file Inter-Process communication configuration of the application. IPC allows the web-view
 * content to exchange information with the Electron application. */

// ===============
// === Channel ===
// ===============

/** Channel names used by the IPC protocol. */
export const channel = {
    error: 'error',
    loadProfiles: 'load-profiles',
    profilesLoaded: 'profiles-loaded',
    saveProfile: 'save-profile',
    quit: 'quit-ide',
}
