let _measureContext: CanvasRenderingContext2D | undefined
function getMeasureContext() {
  return (_measureContext ??= document.createElement('canvas').getContext('2d')!)
}

/** Helper function to get text width. */
export function getTextWidthBySizeAndFamily(
  text: string | null | undefined,
  fontSize = '11.5px',
  fontFamily = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', 'Helvetica Neue', sans-serif",
) {
  return getTextWidthByFont(text, `${fontSize} ${fontFamily}`)
}

/** Helper function to get text width. */
export function getTextWidthByFont(text: string | null | undefined, font: string) {
  if (text == null) {
    return 0
  }
  const context = getMeasureContext()
  context.font = font
  const metrics = context.measureText('  ' + text)
  return metrics.width
}
