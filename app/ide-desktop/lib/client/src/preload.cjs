const { contextBridge, ipcRenderer } = require('electron')
const remote = require('@electron/remote')

const win = remote?.getCurrentWindow()

if (win === undefined) {
    console.warn('Could not get current window object for window startup animation.')
}

// =============================
// === Window Show Animation ===
// =============================

function ease_in_out_quad(t) {
    return t < 0.5 ? 2 * t * t : 1 - ((-2 * t + 2) * (-2 * t + 2)) / 2
}

function animate_show(target) {
    return new Promise(function (resolve, reject) {
        let opacity = 0
        function show_step(timestamp) {
            opacity += 0.02
            if (opacity > 1) {
                opacity = 1
            }
            target.setOpacity(ease_in_out_quad(opacity))
            if (opacity < 1) {
                window.requestAnimationFrame(show_step)
            } else {
                resolve()
            }
        }
        window.requestAnimationFrame(show_step)
    })
}

if (win !== undefined) {
    window.showAnimation = animate_show(win)
}

// ===================
// === Debug Tools ===
// ===================

// TODO[WD] Enable after making preload configurable (we do not want to load it always)
// require('devtron').install()

// ==================
// === Debug APIs ===
// ==================

// These APIs expose functionality for use from Rust; see the bindings in the `debug_api` module for
// the primary documentation.

// Shutdown-related commands and events.
contextBridge.exposeInMainWorld('enso_lifecycle', {
    // Allows application-exit to be initiated from WASM code.
    //
    // This is used, for example, in a key binding (Ctrl+Alt+Q) that saves a performance profile and exits.
    quit: data => ipcRenderer.send('quit-ide'),
})

// Used for capturing profiling data.
contextBridge.exposeInMainWorld('enso_profiling_data', {
    // Delivers profiling log.
    saveProfile: data => ipcRenderer.send('save-profile', data),
})

// Access to the system console that Electron was run from.
contextBridge.exposeInMainWorld('enso_console', {
    // Print an error message with `console.error`.
    error: data => ipcRenderer.send('error', data),
})
