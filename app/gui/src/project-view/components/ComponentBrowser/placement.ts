import { assert } from '@/util/assert'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import type { ToValue } from '@/util/reactivity'
import theme from '@/util/theme.json'
import { toValue } from 'vue'

// Assumed size of a newly created node. This is used to place the component browser and when creating a node before
// other recently-created nodes have rendered and computed their real sizes.
export const DEFAULT_NODE_SIZE = new Vec2(300, 32)

const orDefaultSize = (rect: Rect) => {
  if (rect.width !== 0 && rect.height !== 0) return rect
  const width = Math.max(rect.width, DEFAULT_NODE_SIZE.x)
  const height = Math.max(rect.height, DEFAULT_NODE_SIZE.y)
  return new Rect(rect.pos, new Vec2(width, height))
}

/** A composable with logic related to nodes placement. */
export function usePlacement(nodeRects: ToValue<Iterable<Rect>>, screenBounds: ToValue<Rect>) {
  const gap = themeGap()
  const environment = (selectedNodeRects: Iterable<Rect>) => ({
    selectedNodeRects: Array.from(selectedNodeRects, orDefaultSize),
    screenBounds: toValue(screenBounds),
    nodeRects: Array.from(toValue(nodeRects), orDefaultSize),
  })
  return {
    /** Find a free position for a new node. For details, see {@link previousNodeDictatedPlacement}. */
    place: (selectedNodeRects: Iterable<Rect> = [], nodeSize: Vec2 = DEFAULT_NODE_SIZE): Vec2 =>
      previousNodeDictatedPlacement(nodeSize, environment(selectedNodeRects), gap),
    /** Compute position of new collapsed node. For details, see {@link collapsedNodePlacement}. */
    collapse: (selectedNodeRects: Iterable<Rect>, nodeSize: Vec2 = DEFAULT_NODE_SIZE): Vec2 =>
      collapsedNodePlacement(nodeSize, environment(selectedNodeRects), gap),
    /** Compute position of an input node. For details, see {@link inputNodePlacement}. */
    input: (nonInputNodeRects: Iterable<Rect>, nodeSize: Vec2 = DEFAULT_NODE_SIZE): Vec2 =>
      inputNodePlacement(nodeSize, { ...environment([]), nonInputNodeRects }, gap),
  }
}

interface NonDictatedEnvironment {
  screenBounds: Rect
  nodeRects: Iterable<Rect>
}

export interface Environment extends NonDictatedEnvironment {
  selectedNodeRects: Iterable<Rect>
}

export interface InputNodeEnvironment extends Environment {
  nonInputNodeRects: Iterable<Rect>
}

function themeGap(): Vec2 {
  return new Vec2(theme.node.horizontal_gap, theme.node.vertical_gap)
}

/**
 * The new node should appear at the center of the screen if there is enough space for the new node.
 * Otherwise, it should be moved down to the closest free space.
 *
 * Specifically, this code, in order:
 * - uses the center of the screen as the initial position
 * - searches for all vertical spans below the initial position, that horizontally intersect the
 *   initial position (no horizontal gap is required between the new node and old nodes)
 * - shifts the node down (if required) until there is sufficient vertical space -
 *   the height of the node, in addition to the specified gap both above and below the node.
 *
 * [Documentation](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#placement-of-newly-opened-component-browser)
 */
export function nonDictatedPlacement(
  nodeSize: Vec2,
  { screenBounds, nodeRects }: NonDictatedEnvironment,
  gap: Vec2 = themeGap(),
): Vec2 {
  const initialPosition = screenBounds.center().sub(new Vec2(nodeSize.y / 2, nodeSize.y / 2))
  return seekVertical(new Rect(initialPosition, nodeSize), nodeRects, gap)
}

/**
 * The new node should be left aligned to the first selected node (order of selection matters).
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
 * [Documentation](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#placement-of-newly-opened-component-browser)
 */
export function previousNodeDictatedPlacement(
  nodeSize: Vec2,
  { screenBounds, selectedNodeRects, nodeRects }: Environment,
  gap: Vec2 = themeGap(),
): Vec2 {
  let initialLeft: number | undefined
  let top = -Infinity
  for (const rect of selectedNodeRects) {
    initialLeft ??= rect.left
    const newTop = rect.bottom + gap.y
    if (newTop > top) top = newTop
  }
  if (initialLeft == null) {
    return nonDictatedPlacement(nodeSize, { screenBounds, nodeRects }, gap)
  }
  const initialPosition = new Vec2(initialLeft, top)
  return seekHorizontal(new Rect(initialPosition, nodeSize), nodeRects, gap)
}

/**
 * The new node should appear exactly below the mouse.
 *
 * Specifically, this code assumes the node is fully rounded on the left and right sides,
 * so it adds half the node height (assumed to be the node radius) from the mouse x and y
 * positions.
 *
 * [Documentation](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#placement-of-newly-opened-component-browser)
 */
export function mouseDictatedPlacement(
  mousePosition: Vec2,
  nodeSize: Vec2 = DEFAULT_NODE_SIZE,
): Vec2 {
  const nodeRadius = nodeSize.y / 2
  return mousePosition.add(new Vec2(nodeRadius, nodeRadius))
}

/** The new node should appear above non-input nodes, left aligned to the leftmost node and vertically aligned with the other input nodes. */
export function inputNodePlacement(
  nodeSize: Vec2,
  { nonInputNodeRects, nodeRects }: InputNodeEnvironment,
  gap = themeGap(),
): Vec2 {
  let topMostY
  let leftMostX
  for (const rect of nonInputNodeRects) {
    topMostY = topMostY == null ? rect.top : Math.min(topMostY, rect.top)
    leftMostX = leftMostX == null ? rect.left : Math.min(leftMostX, rect.left)
  }
  assert(
    topMostY != null && leftMostX != null,
    'inputNodePlacement works only if at least one non-input node is present.',
  )
  const initialPosition = new Vec2(leftMostX, topMostY - nodeSize.y - gap.y)
  return seekHorizontal(new Rect(initialPosition, nodeSize), nodeRects, gap)
}

/**
 * The new node should appear at the average Y-position of selected nodes and with the X-position of the leftmost node.
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
  { selectedNodeRects, nodeRects }: Environment,
  gap = themeGap(),
): Vec2 {
  let leftMostX
  let y = 0
  let selectedNodeRectsCount = 0
  const selectedRectKeys = new Set<string>()
  for (const rect of selectedNodeRects) {
    leftMostX = leftMostX == null ? rect.pos.x : Math.min(leftMostX, rect.pos.x)
    y += rect.pos.y
    selectedNodeRectsCount++
    selectedRectKeys.add(rect.key())
  }
  assert(
    selectedNodeRectsCount > 0 && leftMostX != null,
    'averagePositionPlacement works only if at least one node is selected.',
  )
  const initialPosition = new Vec2(leftMostX, y / selectedNodeRectsCount)
  const nonSelectedNodeRects = [...nodeRects].filter((rect) => !selectedRectKeys.has(rect.key()))
  return seekVertical(new Rect(initialPosition, nodeSize), nonSelectedNodeRects, gap)
}

/**
 * Given a preferred location for a node, adjust the top as low as necessary for it not to collide with any of the
 *  provided `otherRects`.
 */
export function seekVertical(preferredRect: Rect, otherRects: Iterable<Rect>, gap = themeGap()) {
  const initialRect = orDefaultSize(preferredRect)
  const nodeRectsSorted = Array.from(otherRects, orDefaultSize).sort((a, b) => a.top - b.top)
  const bottom = () => top + initialRect.height
  let top = initialRect.top
  for (const rect of nodeRectsSorted) {
    if (initialRect.intersectsX(rect) && rect.bottom + gap.y > top) {
      if (rect.top - bottom() < gap.y) {
        top = rect.bottom + gap.y
      }
    }
  }
  return new Vec2(initialRect.left, top)
}

/**
 * Given a preferred location for a node, adjust the left edge as much as necessary for it not to collide with any of
 *  the provided `otherRects`.
 */
export function seekHorizontal(initialRect: Rect, otherRects: Iterable<Rect>, gap = themeGap()) {
  return seekVertical(
    orDefaultSize(initialRect).reflectXY(),
    Array.from(otherRects, (rect) => orDefaultSize(rect).reflectXY()),
    gap.reflectXY(),
  ).reflectXY()
}
