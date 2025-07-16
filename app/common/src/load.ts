/** @file Utilities for loading resources. */

/** Add a script to the DOM. */
export function loadScript(url: string) {
  return new Promise<HTMLScriptElement>((resolve, reject) => {
    const script = document.createElement('script')
    script.crossOrigin = 'anonymous'
    script.src = url
    document.head.appendChild(script)
    script.onload = () => {
      resolve(script)
    }
    script.onerror = reject
  })
}
