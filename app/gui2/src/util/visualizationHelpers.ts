/**
 * Add script to the DOM.
 */
export function loadScript(url: string, onload: (this: GlobalEventHandlers, ev: Event) => void) {
  const script = document.createElement('script')
  script.crossOrigin = 'anonymous'
  script.onload = onload
  script.src = url

  document.head.appendChild(script)
}

/**
 * Add css style file to the DOM.
 */
export function loadStyle(url: string, onload: (this: GlobalEventHandlers, ev: Event) => void) {
  const style = document.createElement('link')
  style.crossOrigin = 'anonymous'
  style.onload = onload
  style.href = url
  style.rel = 'stylesheet'
  style.media = 'screen'
  style.type = 'text/css'

  document.head.appendChild(style)
}
