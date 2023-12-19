let _measureContext: CanvasRenderingContext2D | undefined
function getMeasureContext() {
  return (_measureContext ??= document.createElement('canvas').getContext('2d')!)
}

/** Helper function to get text width to make sure that labels on the x axis do not overlap,
 * and keeps it readable. */
export function getTextWidth(
  text: string | null | undefined,
  fontSize = '11.5px',
  fontFamily = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', 'Helvetica Neue', sans-serif",
) {
  if (text == null) {
    return 0
  }
  const context = getMeasureContext()
  context.font = `${fontSize} ${fontFamily}`
  const metrics = context.measureText('  ' + text)
  return metrics.width
}
