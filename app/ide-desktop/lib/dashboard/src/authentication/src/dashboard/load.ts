/** @file Utilities for loading resources. */

/** Add a script to the DOM. */
export function loadScript(url: string) {
    const script = document.createElement('script')
    script.crossOrigin = 'anonymous'
    script.src = url
    document.head.appendChild(script)
    return new Promise<HTMLScriptElement>(resolve => {
        script.onload = () => {
            resolve(script)
        }
    })
}

/** Add a CSS stylesheet to the DOM. */
export function loadStyle(url: string) {
    const style = document.createElement('link')
    style.crossOrigin = 'anonymous'
    style.href = url
    style.rel = 'stylesheet'
    style.media = 'screen'
    style.type = 'text/css'
    document.head.appendChild(style)
    return new Promise<HTMLLinkElement>(resolve => {
        style.onload = () => {
            resolve(style)
        }
    })
}
