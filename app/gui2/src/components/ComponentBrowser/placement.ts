import { bail } from '@/util/assert'
import { MultiRange, Range } from '@/util/range'
import { Rect } from '@/util/rect'
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
  const occupiedYRanges = new MultiRange()
  for (const rect of nodeRects) {
    if (initialRect.intersectsX(rect) && rect.bottom + gap > top) {
      if (rect.top - bottom() >= gap) {
        const range = new Range(rect.top, rect.bottom)
        occupiedYRanges.insert(range, range.expand(gap))
      } else {
        top = rect.bottom + gap
        const rangeIncludingTop = occupiedYRanges
          .remove(new Range(-Infinity, rect.bottom + minimumVerticalSpace))
          .at(-1)
        if (rangeIncludingTop) {
          top = Math.max(top, rangeIncludingTop.end + gap)
          occupiedYRanges.remove(rangeIncludingTop)
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
  const occupiedXRanges = new MultiRange()
  for (const rect of nodeRects) {
    if (initialRect.intersectsY(rect) && rect.right + gap > left) {
      if (rect.left - right() >= gap) {
        const range = new Range(rect.left, rect.right)
        occupiedXRanges.insert(range, range.expand(gap))
      } else {
        left = rect.right + gap
        const rangeIncludingLeft = occupiedXRanges
          .remove(new Range(-Infinity, rect.right + minimumHorizontalSpace))
          .at(-1)
        if (rangeIncludingLeft) {
          left = Math.max(left, rangeIncludingLeft.end + gap)
          occupiedXRanges.remove(rangeIncludingLeft)
        }
      }
    }
  }
  const finalPosition = new Vec2(left, top)
  if (new Rect(finalPosition, nodeSize).within(screenBounds)) return { position: finalPosition }
  else {
    const screenCenter = screenBounds.center().sub(nodeSize.scale(0.5))
    return { position: finalPosition, pan: finalPosition.sub(screenCenter) }
  }
}

export function mouseDictatedPlacement(
  nodeSize: Vec2,
  { mousePosition }: Environment,
  _opts?: PlacementOptions,
): Placement {
  const nodeRadius = nodeSize.y / 2
  return { position: mousePosition.sub(new Vec2(nodeRadius, nodeRadius)) }
}
