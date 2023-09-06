/**
 * Add script to the DOM.
 */
export function loadScript(url: string, onload: (this: GlobalEventHandlers, ev: Event) => void) {
  let script = document.createElement('script')
  script.crossOrigin = 'anonymous'
  script.onload = onload
  script.src = url

  document.head.appendChild(script)
}

/**
 * Add css style file to the DOM.
 */
export function loadStyle(url: string, onload: (this: GlobalEventHandlers, ev: Event) => void) {
  let style = document.createElement('link')
  style.crossOrigin = 'anonymous'
  style.onload = onload
  style.href = url
  style.rel = 'stylesheet'
  style.media = 'screen'
  style.type = 'text/css'

  document.head.appendChild(style)
}

/**
 * Scrollbar style definitions for textual visualizations which need support for scrolling.
 *
 * The 11px width/height (depending on scrollbar orientation)
 * is set so that it resembles macOS default scrollbar.
 */
export const scrollbarStyle = `
    .scrollable::-webkit-scrollbar { -webkit-appearance: none; }
    
    .scrollable::-webkit-scrollbar-track { -webkit-box-shadow: none; }
    
    .scrollable::-webkit-scrollbar:vertical { width: 11px; }
    
    .scrollable::-webkit-scrollbar:horizontal { height: 11px; }
    
    .scrollable::-webkit-scrollbar-thumb {
        border-radius: 8px;
        border: 1px solid rgba(220, 220, 220, .5);
        background-color: rgba(190, 190, 190, .5);
    }
    
    .scrollable::-webkit-scrollbar-corner { background: rgba(0,0,0,0); }
`
