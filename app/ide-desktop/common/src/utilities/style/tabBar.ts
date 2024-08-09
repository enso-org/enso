/** Functions that generate CSS to render the shapes in a tab bar. */

// ===========
// === Tab ===
// ===========

/** Returns a CSS clip-path for a tab of the specified dimensions. */
export function tabClipPath(
  bounds: Readonly<{ width: number; height: number }>,
  radiusPx: number,
  side: 'top' | 'right' = 'top',
) {
  const sweep0 = 0
  const sweep1 = 1
  const xIndex = side === 'top' ? 0 : 1
  const x1 = [bounds.width, bounds.height][xIndex]!
  const y1 = [bounds.width, bounds.height][1 - xIndex]!
  const orient =
    side === 'top' ? (x: number, y: number) => [x, y] : (x: number, y: number) => [y1 - y, x]
  const pt = (x: number, y: number) => orient(x, y).join(' ')
  const result = path([
    `M ${pt(0, y1)}`,
    `A ${radiusPx} ${radiusPx} 0 0 ${sweep0} ${pt(radiusPx, y1 - radiusPx)}`,
    `L ${pt(radiusPx, radiusPx)}`,
    `A ${radiusPx} ${radiusPx} 0 0 ${sweep1} ${pt(radiusPx * 2, 0)}`,
    `L ${pt(x1 - radiusPx * 2, 0)}`,
    `A ${radiusPx} ${radiusPx} 0 0 ${sweep1} ${pt(x1 - radiusPx, radiusPx)}`,
    `L ${pt(x1 - radiusPx, y1 - radiusPx)}`,
    `A ${radiusPx} ${radiusPx} 0 0 ${sweep0} ${pt(x1, y1)}`,
    `M ${pt(0, 0)}`,
  ])
  return result
}

// ===============
// === Tab Bar ===
// ===============

/** Returns a CSS clip-path for a tab bar of the specified dimensions and parent dimensions. */
export function barClipPath(
  bounds: DOMRectReadOnly,
  rootBounds: DOMRectReadOnly,
  radiusPx: number,
) {
  const tabLeft = bounds.left - rootBounds.left + radiusPx
  const tabRight = bounds.right - rootBounds.left - radiusPx
  const rightSegments = [
    'M 0 0',
    `L ${rootBounds.width + window.outerWidth} 0`,
    `L ${rootBounds.width + window.outerWidth} ${rootBounds.height}`,
    `L ${tabRight + radiusPx} ${rootBounds.height}`,
    `A ${radiusPx} ${radiusPx} 0 0 1 ${tabRight} ${rootBounds.height - radiusPx}`,
  ]
  const leftSegments = [
    `A ${radiusPx} ${radiusPx} 0 0 1 ${tabLeft - radiusPx} ${rootBounds.height}`,
    `L 0 ${rootBounds.height}`,
    'Z',
  ]
  const clipPath = path([
    ...rightSegments,
    `L ${tabRight} ${radiusPx}`,
    `A ${radiusPx} ${radiusPx} 0 0 0 ${tabRight - radiusPx} 0`,
    `L ${tabLeft + radiusPx} 0`,
    `A ${radiusPx} ${radiusPx} 0 0 0 ${tabLeft} ${radiusPx}`,
    `L ${tabLeft} ${rootBounds.height - radiusPx}`,
    ...leftSegments,
  ])
  const rootClipPath = path([
    ...rightSegments,
    `A ${radiusPx} ${radiusPx} 0 0 1 ${tabRight - radiusPx} ${rootBounds.height}`,
    `L ${tabLeft + radiusPx} ${rootBounds.height}`,
    `A ${radiusPx} ${radiusPx} 0 0 1 ${tabLeft} ${rootBounds.height - radiusPx}`,
    ...leftSegments,
  ])
  return { clipPath, rootClipPath }
}

// ===============
// === Helpers ===
// ===============

function path(segments: string[]) {
  return `path("${segments.join(' ')}")`
}
