import { assert } from '@/util/assert'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import theme from '@/util/theme.json'
import type { ComputedRef, MaybeRefOrGetter } from 'vue'
import { toValue } from 'vue'

// Assumed size of a newly created node. This is used to place the component browser and creating a node before other
// recently-created nodes have rendered and computed their real sizes.
export const DEFAULT_NODE_SIZE = new Vec2(240, 24)

export interface PlacementOptions {
  horizontalGap?: number
  verticalGap?: number
}

type ToValue<T> = MaybeRefOrGetter<T> | ComputedRef<T>

export function usePlacement(nodeRects: ToValue<Iterable<Rect>>, screenBounds: ToValue<Rect>) {
  const orDefaultSize = (rect: Rect) =>
    rect.width !== 0 ? rect : new Rect(rect.pos, DEFAULT_NODE_SIZE)
  const options = { horizontalGap: theme.node.horizontal_gap, verticalGap: theme.node.vertical_gap }
  const environment = (selectedNodeRects: Iterable<Rect>) => ({
    selectedNodeRects: Array.from(selectedNodeRects, orDefaultSize),
    screenBounds: toValue(screenBounds),
    nodeRects: Array.from(toValue(nodeRects), orDefaultSize),
  })
  return {
    place: (
      selectedNodeRects: Iterable<Rect> = [],
      nodeSize: Vec2 = DEFAULT_NODE_SIZE,
    ): Placement =>
      previousNodeDictatedPlacement(nodeSize, environment(selectedNodeRects), options),
    collapse: (selectedNodeRects: Iterable<Rect>, nodeSize: Vec2 = DEFAULT_NODE_SIZE): Placement =>
      collapsedNodePlacement(nodeSize, environment(selectedNodeRects), options),
  }
}

interface NonDictatedEnvironment {
  screenBounds: Rect
  nodeRects: Iterable<Rect>
}

export interface Environment extends NonDictatedEnvironment {
  selectedNodeRects: Iterable<Rect>
}

export interface Placement {
  position: Vec2
  pan?: Vec2
}

/** The new node should appear at the center of the screen if there is enough space for the new node.
 * Otherwise, it should be moved down to the closest free space.
 *
 * Specifically, this code, in order:
 * - uses the center of the screen as the initial position
 * - searches for all vertical spans below the initial position, that horizontally intersect the
 *   initial position (no horizontal gap is required between the new node and old nodes)
 * - shifts the node down (if required) until there is sufficient vertical space -
 *   the height of the node, in addition to the specified gap both above and below the node.
 *
 * [Documentation](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#placement-of-newly-opened-component-browser) */
export function nonDictatedPlacement(
  nodeSize: Vec2,
  { screenBounds, nodeRects }: NonDictatedEnvironment,
  { verticalGap = theme.node.vertical_gap }: PlacementOptions = {},
): Placement {
  const initialPosition = screenBounds.center().sub(new Vec2(nodeSize.y / 2, nodeSize.y / 2))
  const initialRect = new Rect(initialPosition, nodeSize)
  let top = initialPosition.y
  const height = nodeSize.y
  const bottom = () => top + height
  const nodeRectsSorted = Array.from(nodeRects).sort((a, b) => a.top - b.top)
  for (const rect of nodeRectsSorted) {
    if (initialRect.intersectsX(rect) && rect.bottom + verticalGap > top) {
      if (rect.top - bottom() < verticalGap) {
        top = rect.bottom + verticalGap
      }
    }
  }
  const finalPosition = new Vec2(initialPosition.x, top)
  if (new Rect(finalPosition, nodeSize).within(screenBounds)) return { position: finalPosition }
  else return { position: finalPosition, pan: finalPosition.sub(initialPosition) }
}

/** The new node should be left aligned to the first selected node (order of selection matters).
 * The Panel should also be placed vertically directly below the lowest of all selected nodes.
 *
 * If there is not enough empty space, the Expression Input Panel should be moved right
 * to the first empty place and the Magnet Alignment algorithm should be performed horizontally.
 * In case the place is offscreen, the camera should be panned accordingly.
 *
 * Specifically, this code, in order:
 * - uses the left side of the first selected node as the initial x-position
 * - uses the lowest (highest y-position) of all selected nodes, plus the specified gap,
 *   as the initial y-position
 * - searches for all horizontal spans to the right of the initial position,
 *   that vertically intersect the initial position
 *   (no vertical gap is required between the new node and old nodes)
 * - shifts the node right (if required) until there is sufficient horizontal space -
 *   the width of the node, in addition to the specified gap to the left and right of the node.
 *
 * Note that the algorithm for finding free space is almost the same as for non-dictated placement,
 * except it searches horizontally instead of vertically.
 *
 * [Documentation](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#placement-of-newly-opened-component-browser) */
export function previousNodeDictatedPlacement(
  nodeSize: Vec2,
  { screenBounds, selectedNodeRects, nodeRects }: Environment,
  {
    horizontalGap = theme.node.horizontal_gap,
    verticalGap = theme.node.vertical_gap,
  }: PlacementOptions = {},
): Placement {
  let initialLeft: number | undefined
  let top = -Infinity
  for (const rect of selectedNodeRects) {
    initialLeft ??= rect.left
    const newTop = rect.bottom + verticalGap
    if (newTop > top) top = newTop
  }
  if (initialLeft == null) {
    return nonDictatedPlacement(nodeSize, { screenBounds, nodeRects }, { verticalGap })
  }
  let left = initialLeft
  const width = nodeSize.x
  const right = () => left + width
  const initialPosition = new Vec2(left, top)
  const initialRect = new Rect(initialPosition, nodeSize)
  const sortedNodeRects = Array.from(nodeRects).sort((a, b) => a.left - b.left)
  for (const rect of sortedNodeRects) {
    if (initialRect.intersectsY(rect) && rect.right + horizontalGap > left) {
      if (rect.left - right() < horizontalGap) {
        left = rect.right + horizontalGap
      }
    }
  }
  const finalPosition = new Vec2(left, top)
  if (new Rect(finalPosition, nodeSize).within(screenBounds)) return { position: finalPosition }
  else {
    const screenCenter = screenBounds.center().sub(new Vec2(nodeSize.y / 2, nodeSize.y / 2))
    return { position: finalPosition, pan: finalPosition.sub(screenCenter) }
  }
}

/** The new node should appear exactly below the mouse.
 *
 * Specifically, this code assumes the node is fully rounded on the left and right sides,
 * so it adds half the node height (assumed to be the node radius) from the mouse x and y
 * positions.
 *
 * [Documentation](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#placement-of-newly-opened-component-browser) */
export function mouseDictatedPlacement(nodeSize: Vec2, mousePosition: Vec2): Placement {
  const nodeRadius = nodeSize.y / 2
  return { position: mousePosition.add(new Vec2(nodeRadius, nodeRadius)) }
}

/** The new node should appear at the average Y-position of selected nodes and with the X-position of the leftmost node.
 *
 * If the desired place is already occupied by non-selected node, it should be moved down to the closest free space.
 *
 * Specifically, this code, in order:
 * - calculates the average position of selected nodes
 * - searches for all vertical spans below the initial position,
 *   that horizontally intersect the initial position (no horizontal gap is required between
 *   the new node and old nodes)
 * - shifts the node down (if required) until there is sufficient vertical space -
 *   the height of the node, in addition to the specified gap both above and below the node.
 */
export function collapsedNodePlacement(
  nodeSize: Vec2,
  { screenBounds, selectedNodeRects, nodeRects }: Environment,
  { verticalGap = theme.node.vertical_gap }: PlacementOptions = {},
): Placement {
  let leftMostX
  let y = 0
  let selectedNodeRectsCount = 0
  for (const rect of selectedNodeRects) {
    leftMostX = leftMostX == null ? rect.pos.x : Math.min(leftMostX, rect.pos.x)
    y += rect.pos.y
    selectedNodeRectsCount++
  }
  assert(
    selectedNodeRectsCount > 0 && leftMostX != null,
    'averagePositionPlacement works only if at least one node is selected.',
  )
  const initialPosition = new Vec2(leftMostX, y / selectedNodeRectsCount)
  const nonSelectedNodeRects = []
  outer: for (const rect of nodeRects) {
    for (const sel of selectedNodeRects) {
      if (sel.equals(rect)) {
        continue outer
      }
    }
    nonSelectedNodeRects.push(rect)
  }
  let top = initialPosition.y
  const initialRect = new Rect(initialPosition, nodeSize)
  const nodeRectsSorted = Array.from(nonSelectedNodeRects).sort((a, b) => a.top - b.top)
  for (const rect of nodeRectsSorted) {
    if (initialRect.intersectsX(rect) && rect.bottom + verticalGap > top) {
      if (rect.top - (top + nodeSize.y) < verticalGap) {
        top = rect.bottom + verticalGap
      }
    }
  }
  const finalPosition = new Vec2(initialPosition.x, top)
  if (new Rect(finalPosition, nodeSize).within(screenBounds)) {
    return { position: finalPosition }
  } else {
    return { position: finalPosition, pan: finalPosition.sub(initialPosition) }
  }
}
