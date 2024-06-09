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
  if (text == null || text == '' || font == '') {
    return 0
  }

  // Continue with `measureText` even if font is not loaded yet. What matters here is tracking
  // reactive state of font loading in case it isn't ready yet, so the width will be recomputed
  // once the loading finishes.
  const _ = fontReady(font)

  const context = getMeasureContext()
  context.font = font
  context.fillText(text, 0, 0)
  const metrics = context.measureText(text)
  return metrics.width
}

/**
 * Stores loading status of queried fonts, so we can make the check synchronous and reactive.
 * This is supposed to be global, since the font loading state is scoped to the document and cannot
 * revert back to loading (assuming we don't dynamically change existing @font-face definitions to
 * point to different URLs, which would be incredibly cursed).
 */
const fontsLoadState = shallowReactive(new Map())

/**
 * Check if given font is ready to use. In case if it is not, the check will automatically register
 * a reactive dependency, which will be notified once loading is complete.
 */
function fontReady(font: string): boolean {
  const readyState = fontsLoadState.get(font)
  if (readyState === undefined) {
    // Make sure to schedule font loading in separate task, so there are no immediate side effects
    // on reactive state when this function is used in computed context.
    setTimeout(() => loadFont(font), 0)
    // This check by itself is not reactive, but it is fine since already track the loading state
    // and the loading will begin shortly.
    return checkFontSync(font)
  }
  return readyState
}

/** Start loading given font if it wasn't started yet. Mutates reactive state. */
function loadFont(font: string): void {
  if (!fontsLoadState.has(font)) {
    const ready = checkFontSync(font)
    fontsLoadState.set(font, ready)
    if (!ready) document.fonts.load(font).then(() => fontsLoadState.set(font, true))
  }
}

/** Check if given font is loaded. NOT reactive. */
function checkFontSync(font: string): boolean {
  try {
    // This operation can fail if the provided font string is not a valid CSS font specifier.
    return document.fonts.check(font)
  } catch (e) {
    console.error(e)
    // In case of exception, treat the font as if it was loaded. That way we don't attempt loading
    // it again, and the browser font fallback logic should still make things behave more or less
    // correct.
    return true
  }
}
