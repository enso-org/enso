import { shallowReactive } from 'vue'

let _measureContext: CanvasRenderingContext2D | undefined
function getMeasureContext() {
  return (_measureContext ??= document.createElement('canvas').getContext('2d')!)
}

/** Helper function to get text width. Accepts font size and family only. For a more precise control, use `getTextWidthByFont`. */
export function getTextWidthBySizeAndFamily(
  text: string | null | undefined,
  fontSize = '11.5px',
  fontFamily = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', 'Helvetica Neue', sans-serif",
) {
  return getTextWidthByFont(text, `${fontSize} ${fontFamily}`)
}

/** Helper function to get text width. `font` is a CSS font specification as per https://developer.mozilla.org/en-US/docs/Web/CSS/font. */
export function getTextWidthByFont(text: string | null | undefined, font: string) {
  if (text == null || font == '' || !fontReady(font)) {
    return 0
  }
  const context = getMeasureContext()
  context.font = font
  const metrics = context.measureText(text)
  return metrics.width
}

/**
 * Stores loading status of queried fonts, so we can make the check synchronous and reactive.
 * This is supposed to be global, since the font loading state is scoped to the document and cannot
 * revert back to loading (assuming we don't dynamically change existing @font-face definitions to
 * point to different URLs, which would be incredibly cursed).
 */
const fontsReady = shallowReactive(new Map())

/**
 * Check if given font is ready to use. In case if it is not, the check will automatically register
 * a reactive dependency, which will be notified once loading is complete.
 * @param font
 * @returns
 */
function fontReady(font: string): boolean {
  const readyState = fontsReady.get(font)
  if (readyState === undefined) {
    let syncReady
    try {
      // This operation can fail if the provided font string is not a valid CSS font specifier.
      syncReady = document.fonts.check(font)
    } catch (e) {
      console.error(e)
      // In case of exception, treat the font as if it was loaded. That way we don't attempt loading
      // it again, and the browser font fallback logic should still make things behave more or less
      // correct.
      syncReady = true
    }
    fontsReady.set(font, syncReady)
    if (syncReady) return true
    else document.fonts.load(font).then(() => fontsReady.set(font, true))
    return false
  }
  return readyState
}
