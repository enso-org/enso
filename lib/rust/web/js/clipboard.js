/// Implementation of clipboard management. For full documentation, see the rust module.

// =============
// === Impls ===
// =============

function fallbackWriteText(text) {
    let successful = false
    let textArea = document.createElement('textarea')

    // *** This styling is an extra step which is likely not required. ***
    //
    // Why is it here? To ensure:
    // 1. the element is able to have focus and selection.
    // 2. if element was to flash render it has minimal visual impact.
    // 3. less flakyness with selection and copying which **might** occur if
    //    the textarea element is not visible.
    //
    // The likelihood is the element won't even render, not even a
    // flash, so some of these are just precautions. However in
    // Internet Explorer the element is visible whilst the popup
    // box asking the user for permission for the web page to
    // copy to the clipboard.

    // Place in top-left corner of screen regardless of scroll position.
    textArea.style.position = 'fixed'
    textArea.style.top = 0
    textArea.style.left = 0

    // Ensure it has a small width and height. Setting to 1px / 1em
    // doesn't work as this gives a negative w/h on some browsers.
    textArea.style.width = '2em'
    textArea.style.height = '2em'

    // We don't need padding, reducing the size if it does flash render.
    textArea.style.padding = 0

    // Clean up any borders.
    textArea.style.border = 'none'
    textArea.style.outline = 'none'
    textArea.style.boxShadow = 'none'

    // Avoid flash of white box if rendered for any reason.
    textArea.style.background = 'transparent'

    textArea.value = text
    textArea.style.top = '0'
    textArea.style.left = '0'
    textArea.style.position = 'fixed'
    document.body.appendChild(textArea)
    textArea.focus()
    textArea.select()
    try {
        successful = document.execCommand('copy') == 1
    } catch (err) {}
    document.body.removeChild(textArea)
    if (!successful) {
        console.error('Could not write to clipboard.')
    }
}

export function writeText(text) {
    if (!navigator.clipboard) {
        fallbackWriteText(text)
    } else {
        navigator.clipboard.writeText(text).then(
            () => {},
            err => {
                fallbackWriteText(text)
            }
        )
    }
}

/// Firefox only supports reading the clipboard in browser extensions, so it will
/// only work with `cmd + v` shortcut. To learn more, see the
/// [MSDN compatibility note](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/readText).
let lastPaste = ''
function init_firefox_fallback() {
    // Checking whether the window is defined. It could not be defined if the program is run in
    // node, for example to extract the shaders.
    if (typeof window !== 'undefined') {
        window.addEventListener('paste', event => {
            lastPaste = (event.clipboardData || window.clipboardData).getData('text')
        })
    }
}

export function readText(callback) {
    if (!navigator.clipboard) {
        callback(lastPaste)
    } else {
        navigator.clipboard.readText().then(
            function (text) {
                callback(text)
            },
            function (err) {
                callback(lastPaste)
            }
        )
    }
}

// ======================
// === Initialization ===
// ======================

init_firefox_fallback()
