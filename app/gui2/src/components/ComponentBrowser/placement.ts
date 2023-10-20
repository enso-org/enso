import { Rect } from '@/stores/rect'
import { binarySearch } from '@/util/array'
import { assertFn, bail } from '@/util/assert'
import { Vec2 } from '@/util/vec2'

export interface Environment {
  screenBounds: Rect
  selectedNodeRects: Iterable<Rect>
  nodeRects: Iterable<Rect>
  mousePosition: Vec2
}

export interface PlacementOptions {
  gap?: number
}

export interface Placement {
  position: Vec2
  pan?: Vec2
}

// The default gap is the height of a single node.
const defaultGap = 24

export function nonDictatedPlacement(
  nodeSize: Vec2,
  { screenBounds, nodeRects }: Environment,
  { gap = defaultGap }: PlacementOptions = {},
): Placement {
  const initialPosition = screenBounds.center().sub(nodeSize.scale(0.5))
  const initialRect = new Rect(initialPosition, nodeSize)
  let top = initialPosition.y
  const height = nodeSize.y
  const minimumVerticalSpace = height + gap * 2
  const bottom = () => top + height
  const occupiedYRanges: { top: number; bottom: number }[] = []
  const pred = (range: (typeof occupiedYRanges)[number]) => range.top - bottom() >= gap
  for (const rect of nodeRects) {
    if (initialRect.intersectsX(rect) && rect.bottom + gap > top) {
      if (rect.top - bottom() >= gap) {
        const index = binarySearch(occupiedYRanges, (range) => range.top >= rect.top)
        const range = occupiedYRanges[index]
        if (range && range.top - rect.bottom < minimumVerticalSpace) {
          // TODO: fuzz case for range.top = rect.bottom
          assertFn(() => range.top >= rect.top)
          range.top = rect.top
        } else if (
          !range &&
          rect.top - (occupiedYRanges.at(-1)?.bottom ?? -Infinity) < minimumVerticalSpace
        ) {
          assertFn(() => occupiedYRanges.at(-1)!.bottom <= rect.bottom)
          // `rect.bottom` may be less if the new range is completely within the old range.
          occupiedYRanges.at(-1)!.bottom = Math.max(occupiedYRanges.at(-1)!.bottom, rect.bottom)
        } else {
          occupiedYRanges.splice(index, 0, { top: rect.top, bottom: rect.bottom })
        }
      } else {
        top = rect.bottom + gap
        const lastDeletedElement = occupiedYRanges
          .splice(0, binarySearch(occupiedYRanges, pred))
          .at(-1)
        if (lastDeletedElement) {
          top = Math.max(top, lastDeletedElement.bottom + gap)
        }
      }
    }
  }
  const finalPosition = new Vec2(initialPosition.x, top)
  if (new Rect(finalPosition, nodeSize).within(screenBounds)) return { position: finalPosition }
  else return { position: finalPosition, pan: finalPosition.sub(initialPosition) }
}

export function previousNodeDictatedPlacement(
  nodeSize: Vec2,
  { screenBounds, selectedNodeRects, nodeRects }: Environment,
  { gap = defaultGap }: PlacementOptions = {},
): Placement {
  let initialLeft: number | undefined
  let top = -Infinity
  for (const rect of selectedNodeRects) {
    initialLeft ??= rect.left
    const newTop = rect.bottom + gap
    if (newTop > top) top = newTop
  }
  if (initialLeft == null)
    bail('There are no selected nodes, so placement cannot be dictated by the previous node.')
  let left = initialLeft
  const width = nodeSize.x
  const right = () => left + width
  const minimumHorizontalSpace = width + gap * 2
  const initialPosition = new Vec2(left, top)
  const initialRect = new Rect(initialPosition, nodeSize)
  const occupiedXRanges: { left: number; right: number }[] = []
  const pred = (range: (typeof occupiedXRanges)[number]) => range.left - right() >= gap
  for (const rect of nodeRects) {
    if (initialRect.intersectsY(rect) && rect.right + gap > left) {
      if (rect.left - right() >= gap) {
        const index = binarySearch(occupiedXRanges, (range) => range.left >= rect.left)
        const range = occupiedXRanges[index]
        if (range && range.left - rect.right < minimumHorizontalSpace) {
          range.left = Math.min(range.left, rect.left)
        } else if (
          !range &&
          rect.left - (occupiedXRanges.at(-1)?.right ?? -Infinity) < minimumHorizontalSpace
        ) {
          // `rect.right` may be less if the new range is completely within the old range.
          occupiedXRanges.at(-1)!.right = Math.max(occupiedXRanges.at(-1)!.right, rect.right)
        } else {
          occupiedXRanges.splice(index, 0, { left: rect.left, right: rect.right })
        }
      } else {
        left = rect.right + gap
        const lastDeletedElement = occupiedXRanges
          .splice(0, binarySearch(occupiedXRanges, pred))
          .at(-1)
        if (lastDeletedElement) {
          left = Math.max(left, lastDeletedElement.right + gap)
        }
      }
    }
  }
  const finalPosition = new Vec2(left, top)
  if (new Rect(finalPosition, nodeSize).within(screenBounds)) return { position: finalPosition }
  else return { position: finalPosition, pan: finalPosition.sub(initialPosition) }
}

export function mouseDictatedPlacement(
  nodeSize: Vec2,
  { mousePosition }: Environment,
  _opts?: PlacementOptions,
): Placement {
  const nodeRadius = nodeSize.y / 2
  return { position: mousePosition.sub(new Vec2(nodeRadius, nodeRadius)) }
}
