/**
 * Add script to the DOM.
 */
function loadScript(url, onload) {
    let script = document.createElement('script')
    script.onload = onload
    script.src = url

    document.head.appendChild(script)
}

/**
 * Add css style file to the DOM.
 */
function loadStyle(url, onload) {
    let style = document.createElement('link')
    style.onload = onload
    style.href = url
    style.rel = 'stylesheet'
    style.media = 'screen'
    style.type = 'text/css'

    document.head.appendChild(style)
}
