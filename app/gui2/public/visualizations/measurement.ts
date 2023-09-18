/** Helper function to get text width to make sure that labels on x axis do not overlap,
 * and keeps it readable. */
export function getTextWidth(text: string | null | undefined, font: string) {
  if (text == null) {
    return 0
  }
  const canvas = document.createElement('canvas')
  const context = canvas.getContext('2d')
  if (context == null) {
    throw new Error('Could not get canvas 2D context.')
  }
  context.font = font
  const metrics = context.measureText('  ' + text)
  return metrics.width
}
