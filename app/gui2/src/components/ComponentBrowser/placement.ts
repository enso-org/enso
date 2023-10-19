import { Rect } from '@/stores/rect'
import { binarySearch } from '@/util/array'
import { bail } from '@/util/assert'
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
  const occupiedYRanges: { top: number; bottom: number }[] = []
  const pred = (range: (typeof occupiedYRanges)[number]) => range.top - top >= gap
  for (const rect of nodeRects) {
    if (initialRect.intersectsX(rect) && rect.bottom > top) {
      if (rect.top - top > gap) {
        occupiedYRanges.splice(
          binarySearch(occupiedYRanges, (range) => range.top >= rect.top),
          0,
          {
            top: rect.top,
            bottom: rect.bottom,
          },
        )
      } else {
        top = rect.bottom + gap
        occupiedYRanges.splice(0, binarySearch(occupiedYRanges, pred))
      }
    }
  }
  const finalPosition = new Vec2(initialPosition.x, top)
  if (new Rect(finalPosition, nodeSize).within(screenBounds)) return { position: finalPosition }
  else return { position: finalPosition, pan: finalPosition.sub(initialPosition) }
}

export function previousNodeDictatedPlacement(
  previousNode: Rect,
  nodeSize: Vec2,
  { screenBounds, selectedNodeRects, nodeRects }: Environment,
  { gap = defaultGap }: PlacementOptions = {},
): Placement {
  let firstSelectedNodeRect: Rect | undefined
  let top = -Infinity
  for (const rect of selectedNodeRects) {
    firstSelectedNodeRect ??= rect
    const newTop = rect.bottom + gap
    if (newTop > top) top = newTop
  }
  if (firstSelectedNodeRect == null)
    bail('There are no selected nodes, so placement cannot be dictated by the previous node.')
  let left = firstSelectedNodeRect.left
  const initialPosition = new Vec2(left, top)
  const initialRect = new Rect(initialPosition, nodeSize)
  const occupiedXRanges: { left: number; right: number }[] = []
  const pred = (range: (typeof occupiedXRanges)[number]) => range.left - left >= gap
  for (const rect of nodeRects) {
    if (initialRect.intersectsY(rect) && rect.right > left) {
      if (rect.left - left > gap) {
        occupiedXRanges.splice(
          binarySearch(occupiedXRanges, (range) => range.left > rect.left),
          0,
          {
            left: rect.left,
            right: rect.right,
          },
        )
      } else {
        left = rect.right + gap
        occupiedXRanges.splice(0, binarySearch(occupiedXRanges, pred))
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
