/** @file Preload script containing code that runs before web page is loaded into the browser
 * window. It has access to both DOM APIs and Node environment, and is used to expose privileged
 * APIs to the renderer via the contextBridge API. To learn more, visit:
 * https://www.electronjs.org/docs/latest/tutorial/tutorial-preload. */

const { contextBridge, ipcRenderer } = require('electron')
import * as ipc from 'ipc'



// =================
// === Constants ===
// =================

/** API object exposed to the dashboard and IDE. Contains methods that can be used to open URLs in
 * the system browser. */
const AUTHENTICATION_API_KEY = 'authenticationApi'



// ======================
// === Profiling APIs ===
// ======================

// These APIs expose functionality for use from Rust. See the bindings in the `debug_api` module for
// the primary documentation.

/** Shutdown-related commands and events. */
contextBridge.exposeInMainWorld('enso_lifecycle', {
    /** Allows application-exit to be initiated from WASM code.
     * This is used, for example, in a key binding (Ctrl+Alt+Q) that saves a performance profile and
     * exits. */
    quit: () => ipcRenderer.send(ipc.channel.quit),
})

// Save and load profile data.
let onProfiles: ((profiles: string[]) => void)[] = []
let profilesLoaded: string[]
ipcRenderer.on(ipc.channel.profilesLoaded, (event, profiles: string[]) => {
    for (const callback of onProfiles) {
        callback(profiles)
    }
    onProfiles = []
    profilesLoaded = profiles
})
contextBridge.exposeInMainWorld('enso_profiling_data', {
    // Delivers profiling log.
    saveProfile: (data: any) => ipcRenderer.send(ipc.channel.saveProfile, data),
    // Requests any loaded profiling logs.
    loadProfiles: (callback: (profiles: string[]) => void) => {
        if (profilesLoaded === undefined) {
            ipcRenderer.send('load-profiles')
            onProfiles.push(callback)
        } else {
            callback(profilesLoaded)
        }
    },
})

// Access to the system console that Electron was run from.
contextBridge.exposeInMainWorld('enso_console', {
    // Print an error message with `console.error`.
    error: (data: any) => ipcRenderer.send('error', data),
})



// ==========================
// === Authentication API ===
// ==========================

/** Exposes an `AuthenticationApi` object on the main window that can be used from within our
 * dashboard to open OAuth flows in the system browser, and to accept redirects to the dashboard
 * from the system browser and email client. */
contextBridge.exposeInMainWorld(AUTHENTICATION_API_KEY, {
    /** Open a URL in the system browser (rather than in the app).
     * 
     * OAuth URLs must be opened this way because the dashboard application is sandboxed and thus
     * not privileged to do so unless we explicitly expose this functionality. */
    openExternalUrl: (url: string) => ipcRenderer.send(ipc.channel.openExternalUrl, url),
    /** Set the callback that Electron will call when an authenticated-related URL is opened and
     * handled by the `open-url` handler.
     *
     * The callback is intended to handle links like
     * `enso://authentication/register?code=...&state=...` from external sources like the user's
     * system browser or email client. Handling the links involves resuming whatever flow was in
     * progress when the link was opened (e.g., an OAuth registration flow). */
    setOpenAuthenticationUrlCallback: (callback: (url: string) => void) =>
        ipcRenderer.on(ipc.channel.openAuthenticationUrl, (_event, url) => callback(url)),
})
