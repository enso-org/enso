import {
  mouseDictatedPlacement,
  nonDictatedPlacement,
  previousNodeDictatedPlacement,
  type Environment,
  type Placement,
  type PlacementOptions,
} from '@/components/ComponentBrowser/placement'
import { Rect } from '@/stores/rect'
import * as iterable from '@/util/iterable'
import { chain, map, range } from '@/util/iterable'
import { Vec2 } from '@/util/vec2'
import { expect, test, vi } from 'vitest'

// Vue playground to visually inspect failing fuzz cases:
// https://play.vuejs.org/#eNrNU09PwjAU/ypNNeGCMPFC5jRR40EPatSbNXGMxyiMtmnfYGbZd/e1Y0Ci4Wwv6+/Pa3+v7Wp+Y8xgXQKPeeIyKw0yB1iaa6EyrRwyCxk6dsU+hPoU6pAlsmYFzDBmUZ+hNuG7kVOcx+w8ovkcZD4neRSxRqhk2G5ASxNAWJkiRSCUTOWaOfwu4ErwfU0UmeryYD0PBSc/Y6FifTbTlipCFqnapIKzuFuqZkY7iVKrmPXSidNFidDrbzN/nda+YuBRY6qvbQsdTaBltwE6PsBW6aJ2UotbbZJmy9zqUk1p75NxGKNRj86BXydDir/v41/mHdEYj3/l3c6S4e76eJ+jo0cxk/lg4bSih1R7q+CZXhlZgH02viW6mZgFxWtpUejNY+DQltDv+GwO2fIPfuEqzwn+YsGBXYPgOw1TmwO28v3bE1Q034krPS0Lch8RXyGcNGVsbbd0CBT7wBfSPqyMtihV/u7uKwTluqZ8UO9sgl9w+pvujrS+j3sxuAh1QjW8+QFAeS2/

const defaultScreenBounds = new Rect(new Vec2(100, 200), new Vec2(2000, 1000))
const defaultNodeSize = new Vec2(100, 20)

test('Non dictated placement', () => {
  function generateVueCode(newNode: Rect, rects: Rect[]) {
    return `Please visually inspect the code below at https://play.vuejs.org/:
<script setup>
const rects = [
${rects
  .map(
    (rect) =>
      `  { left: ${rect.left}, top: ${rect.top}, width: (${rect.right} - ${rect.left}), height: (${rect.bottom} - ${rect.top}) },`,
  )
  .join('\n')}
]

const rect = { left: ${newNode.pos.x}, top: ${newNode.pos.y}, width: ${newNode.size.x}, height: ${
      newNode.size.y
    } }
</script>

<template>
<div style="height: 1000px; width: 2000px;">
  <div v-for="rect in rects" :style="{ position: 'absolute', left: rect.left + 'px', top: rect.top + 'px', width: rect.width + 'px', height: rect.height + 'px', background: '#88888822' }"></div>
  <div :style="{ position: 'absolute', left: rect.left + 'px', top: rect.top + 'px', width: rect.width + 'px', height: rect.height + 'px', background: '#88222288' }"></div>
  </div>
</template>
`
  }

  const getSelectedNodeRects = vi.fn(() => iterable.empty())
  const getMousePosition = vi.fn(() => Vec2.Zero)
  // Center is at (1100, 700)
  const screenBounds = defaultScreenBounds
  // Half of this is (50, 10)
  const nodeSize = defaultNodeSize
  function environment(nodeRects: Iterable<Rect>): Environment {
    return {
      screenBounds,
      nodeRects,
      get selectedNodeRects() {
        return getSelectedNodeRects()
      },
      get mousePosition() {
        return getMousePosition()
      },
    }
  }
  expect(nonDictatedPlacement(nodeSize, environment([]))).toEqual<Placement>({
    position: new Vec2(1050, 690),
  })
  // Single node
  expect(
    nonDictatedPlacement(nodeSize, environment([new Rect(new Vec2(1050, 690), defaultNodeSize)])),
    // 20px existing node height + 24px gap
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (far enough left that it does not overlap)
  expect(
    nonDictatedPlacement(nodeSize, environment([new Rect(new Vec2(950, 690), defaultNodeSize)])),
  ).toEqual<Placement>({ position: new Vec2(1050, 690) })
  // Single node (far enough right that it does not overlap)
  expect(
    nonDictatedPlacement(nodeSize, environment([new Rect(new Vec2(1150, 690), defaultNodeSize)])),
  ).toEqual<Placement>({ position: new Vec2(1050, 690) })
  // Single node (barely overlaps on the left)
  expect(
    nonDictatedPlacement(nodeSize, environment([new Rect(new Vec2(951, 690), defaultNodeSize)])),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (barely overlaps on the right)
  expect(
    nonDictatedPlacement(nodeSize, environment([new Rect(new Vec2(1149, 690), defaultNodeSize)])),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (BIG gap)
  expect(
    nonDictatedPlacement(nodeSize, environment([new Rect(new Vec2(1050, 690), defaultNodeSize)]), {
      gap: 1000,
    }),
    // 20px existing node height + 1000px gap
  ).toEqual<Placement>({ position: new Vec2(1050, 1710), pan: new Vec2(0, 1020) })
  // Multiple nodes
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(map(range(0, 1001, 20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize))),
    ),
  ).toEqual<Placement>({
    // The bottom-most node ends at 1020, and there should be a 24px gap.
    position: new Vec2(1050, 1044),
  })
  // Multiple nodes (reverse)
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(map(range(1000, -1, -20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize))),
    ),
  ).toEqual<Placement>({
    // Same as above, but with node rects reversed
    position: new Vec2(1050, 1044),
  })
  // Multiple nodes with gap
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(
        chain(
          map(range(500, 901, 20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
          map(range(1000, 1501, 20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
        ),
      ),
    ),
  ).toEqual<Placement>({
    // There is a 100px gap between y = 900 and y = 1000, which is more than large enough for this node.
    position: new Vec2(1050, 944),
  })
  // Multiple nodes with gap (just big enough)
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(map(range(690, 1500, 88), (y) => new Rect(new Vec2(1050, y), defaultNodeSize))),
    ),
  ).toEqual<Placement>({
    position: new Vec2(1050, 734),
  })
  // Multiple nodes with gap (slightly too small)
  expect(
    nonDictatedPlacement(
      nodeSize,
      // 500 + 87 * 4 === 848
      environment(map(range(500, 849, 87), (y) => new Rect(new Vec2(1050, y), defaultNodeSize))),
    ),
  ).toEqual<Placement>({
    // 848 + 20 + 24
    position: new Vec2(1050, 892),
  })
  // Multiple nodes with smallest gap
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(
        chain(
          map(range(500, 901, 20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
          map(range(988, 1489, 20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
        ),
      ),
    ),
  ).toEqual<Placement>({
    // There is a 68px gap (20px height + 24px gap * 2) between y = 920 (the bottom of the previous node)
    // and y = 988 (the top of the next node), which is exactly large enough for this node.
    position: new Vec2(1050, 944),
  })
  // Multiple nodes with smallest gap (reverse)
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(
        chain(
          map(range(1488, 987, -20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
          map(range(900, 499, -20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
        ),
      ),
    ),
  ).toEqual<Placement>({
    // There is a 68px gap (20px height + 24px gap * 2) between y = 920 (the bottom of the previous node)
    // and y = 988 (the top of the next node), which is exactly large enough for this node.
    position: new Vec2(1050, 944),
  })
  // Multiple nodes with gap that is too small
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(
        chain(
          map(range(500, 901, 20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
          map(range(987, 1488, 20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
        ),
      ),
    ),
  ).toEqual<Placement>({
    // This gap is 1px smaller than the previous test - so, 1px too small.
    position: new Vec2(1050, 1531),
    // This position is offscreen (y >= 1000), so we pan so that the new node is centered (1531 - 690).
    pan: new Vec2(0, 841),
  })
  // Multiple nodes with gap that is too small (each range reversed)
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(
        chain(
          map(range(900, 499, -20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
          map(range(1487, 986, -20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize)),
        ),
      ),
    ),
  ).toEqual<Placement>({
    // This gap is 1px smaller than the previous test - so, 1px too small.
    position: new Vec2(1050, 1531),
    // This position is offscreen (y >= 1000), so we pan so that the new node is centered (1531 - 690).
    pan: new Vec2(0, 841),
  })
  // If fuzzing fails, then a new test case should be added above.
  for (let i = 0; i < 100; i += 1) {
    const nodes = Array.from(
      { length: 20 },
      () =>
        new Rect(
          new Vec2(Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000)),
          new Vec2(Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000)),
        ),
    )
    const newNodeRect = new Rect(
      nonDictatedPlacement(nodeSize, environment(nodes)).position,
      nodeSize,
    )
    for (const node of nodes) {
      expect(node.intersects(newNodeRect), {
        toString() {
          return generateVueCode(newNodeRect, nodes)
        },
      } as string).toBe(false)
    }
  }
  expect(getSelectedNodeRects, 'Should not depend on `selectedNodeRects`').not.toHaveBeenCalled()
  expect(getMousePosition, 'Should not depend on `mousePosition`').not.toHaveBeenCalled()
})

test('Previous node dictated placement', () => {
  function generateVueCode(newNode: Rect, rects: Rect[], selectedRects: Rect[]) {
    return `Please visually inspect the code below at https://play.vuejs.org/:
<script setup>
const rects = [
${rects
  .filter((rect) => !selectedRects.includes(rect))
  .map(
    (rect) =>
      `  { left: ${rect.left}, top: ${rect.top}, width: (${rect.right} - ${rect.left}), height: (${rect.bottom} - ${rect.top}) },`,
  )
  .join('\n')}
]

const selectedRects = [
${selectedRects
  .map(
    (rect) =>
      `  { left: ${rect.left}, top: ${rect.top}, width: (${rect.right} - ${rect.left}), height: (${rect.bottom} - ${rect.top}) },`,
  )
  .join('\n')}
]

const rect = { left: ${newNode.pos.x}, top: ${newNode.pos.y}, width: ${newNode.size.x}, height: ${
      newNode.size.y
    } }
</script>

<template>
<div style="height: 1000px; width: 2000px;">
  <div v-for="rect in rects" :style="{ position: 'absolute', left: rect.left + 'px', top: rect.top + 'px', width: rect.width + 'px', height: rect.height + 'px', background: '#88888822' }"></div>
  <div v-for="rect in selectedRects" :style="{ position: 'absolute', left: rect.left + 'px', top: rect.top + 'px', width: rect.width + 'px', height: rect.height + 'px', background: '#4444aa44' }"></div>
  <div :style="{ position: 'absolute', left: rect.left + 'px', top: rect.top + 'px', width: rect.width + 'px', height: rect.height + 'px', background: '#88222288' }"></div>
  </div>
</template>
`
  }

  const getMousePosition = vi.fn(() => Vec2.Zero)
  // Center is at (1100, 700)
  const screenBounds = defaultScreenBounds
  // Half of this is (50, 10)
  const nodeSize = defaultNodeSize
  function environment(nodeRects: Iterable<Rect>, selectedNodeRects: Iterable<Rect>): Environment {
    return {
      screenBounds,
      nodeRects,
      selectedNodeRects,
      get mousePosition() {
        return getMousePosition()
      },
    }
  }
  // Found by fuzzing.
  {
    const rects = [
      new Rect(new Vec2(756, 86), new Vec2(1609 - 756, 1001 - 86)),
      new Rect(new Vec2(485, 998), new Vec2(851 - 485, 1495 - 998)),
      new Rect(new Vec2(320, 419), new Vec2(981 - 320, 1354 - 419)),
      new Rect(new Vec2(312, 945), new Vec2(1100 - 312, 1643 - 945)),
      new Rect(new Vec2(779, 726), new Vec2(878 - 779, 1094 - 726)),
      new Rect(new Vec2(365, 50), new Vec2(1123 - 365, 299 - 50)),
      new Rect(new Vec2(267, 292), new Vec2(562 - 267, 1124 - 292)),
      new Rect(new Vec2(843, 884), new Vec2(1390 - 843, 1375 - 884)),
      new Rect(new Vec2(123, 957), new Vec2(200 - 123, 1542 - 957)),
      new Rect(new Vec2(953, 104), new Vec2(1506 - 953, 1032 - 104)),

      new Rect(new Vec2(23, 152), new Vec2(282 - 23, 1054 - 152)),
      new Rect(new Vec2(454, 589), new Vec2(571 - 454, 833 - 589)),
      new Rect(new Vec2(497, 203), new Vec2(1087 - 497, 641 - 203)),
      new Rect(new Vec2(176, 980), new Vec2(303 - 176, 1054 - 980)),
      new Rect(new Vec2(139, 886), new Vec2(425 - 139, 1139 - 886)),
      new Rect(new Vec2(191, 446), new Vec2(789 - 191, 1033 - 446)),
      new Rect(new Vec2(793, 306), new Vec2(1284 - 793, 650 - 306)),
      new Rect(new Vec2(241, 273), new Vec2(395 - 241, 996 - 273)),
      new Rect(new Vec2(389, 907), new Vec2(721 - 389, 1169 - 907)),
      new Rect(new Vec2(480, 998), new Vec2(576 - 480, 1242 - 998)),
    ]
    const selectedRects = rects.slice(10)
    expect(
      previousNodeDictatedPlacement(nodeSize, environment(rects, selectedRects)).position,
    ).toEqual(new Vec2(1414, 1266))
  }
  // If fuzzing fails, then a new test case should be added above.
  for (let i = 0; i < 100; i += 1) {
    const nodes = Array.from(
      { length: 20 },
      () =>
        new Rect(
          new Vec2(Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000)),
          new Vec2(Math.floor(Math.random() * 1000), Math.floor(Math.random() * 1000)),
        ),
    )
    const selectedNodes = nodes.slice(10)
    const newNodeRect = new Rect(
      previousNodeDictatedPlacement(nodeSize, environment(nodes, selectedNodes)).position,
      nodeSize,
    )
    for (const node of nodes) {
      expect(node.intersects(newNodeRect), {
        toString() {
          return generateVueCode(newNodeRect, nodes, selectedNodes)
        },
      } as string).toBe(false)
    }
  }
  expect(getMousePosition, 'Should not depend on `mousePosition`').not.toHaveBeenCalled()
})

test('Mouse dictated placement', () => {
  const nodeSize = defaultNodeSize
  const radius = nodeSize.y / 2
  const getScreenBounds = vi.fn(() => defaultScreenBounds)
  const getNodeRects = vi.fn(() => iterable.empty())
  const getSelectedNodeRects = vi.fn(() => iterable.empty())
  const getGap = vi.fn(() => 24)
  const opts: PlacementOptions = {
    get gap() {
      return getGap()
    },
  }
  for (let i = 0; i < 1000; i += 1) {
    const x = Math.floor(Math.random() * 1000)
    const y = Math.floor(Math.random() * 1000)
    expect(
      mouseDictatedPlacement(
        nodeSize,
        {
          mousePosition: new Vec2(x, y),
          get screenBounds() {
            return getScreenBounds()
          },
          get nodeRects() {
            return getNodeRects()
          },
          get selectedNodeRects() {
            return getSelectedNodeRects()
          },
        },
        opts,
      ),
    ).toEqual<Placement>({
      // Note: Currently, this is a reimplementation of the entire mouse dictated placement algorithm.
      position: new Vec2(x - radius, y - radius),
    })
  }
  // Non-overlap test omitted, as mouse-dictated node placement MAY overlap existing nodes.
  expect(getScreenBounds, 'Should not depend on `screenBounds`').not.toHaveBeenCalled()
  expect(getNodeRects, 'Should not depend on `nodeRects`').not.toHaveBeenCalled()
  expect(getSelectedNodeRects, 'Should not depend on `selectedNodeRects`').not.toHaveBeenCalled()
  expect(getGap, 'Should not depend on `gap`').not.toHaveBeenCalled()
})
