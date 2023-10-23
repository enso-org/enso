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
  // Single node (overlaps on the left by 1px)
  expect(
    nonDictatedPlacement(nodeSize, environment([new Rect(new Vec2(951, 690), defaultNodeSize)])),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (overlaps on the right by 1px)
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
    // The bottom-most node ends at y=1020, and there should be a 24px gap.
  ).toEqual<Placement>({ position: new Vec2(1050, 1044) })
  // Multiple nodes (reverse)
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment(map(range(1000, -1, -20), (y) => new Rect(new Vec2(1050, y), defaultNodeSize))),
    ),
  ).toEqual<Placement>({ position: new Vec2(1050, 1044) })
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
    // There is a 80x gap between y=920 and y=1000, which is more than large enough for this node.
  ).toEqual<Placement>({ position: new Vec2(1050, 944) })
  // Multiple nodes with gap (just big enough)
  expect(
    nonDictatedPlacement(
      nodeSize,
      // 20px height*2 nodes + 24px*2 gap
      environment(map(range(690, 1500, 88), (y) => new Rect(new Vec2(1050, y), defaultNodeSize))),
    ),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Multiple nodes with gap (slightly too small)
  expect(
    nonDictatedPlacement(
      nodeSize,
      // 500 + 87 * 4 === 848
      environment(map(range(500, 849, 87), (y) => new Rect(new Vec2(1050, y), defaultNodeSize))),
    ),
    // 848 + 20 + 24
  ).toEqual<Placement>({ position: new Vec2(1050, 892) })
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
    // The smallest gap between the top of adjacent nodes this can fit into, is
    // 20px height * 2 nodes + 24px gap * 2.
  ).toEqual<Placement>({ position: new Vec2(1050, 944) })
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
  ).toEqual<Placement>({ position: new Vec2(1050, 944) })
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
    // This gap is 1px smaller than the previous test - so, 1px too small.
    // This position is offscreen (y >= 1000), so we pan so that the new node is centered (1531 - 690).
  ).toEqual<Placement>({ position: new Vec2(1050, 1531), pan: new Vec2(0, 841) })
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
  ).toEqual<Placement>({ position: new Vec2(1050, 1531), pan: new Vec2(0, 841) })
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
  function environment2(nodeRects: Rect[], selectedNodeRectsStart: number): Environment {
    return {
      screenBounds,
      nodeRects,
      selectedNodeRects: nodeRects.slice(selectedNodeRectsStart),
      get mousePosition() {
        return getMousePosition()
      },
    }
  }
  // Previous node dictated placement MUST fail if there is no previous node.
  expect(() => previousNodeDictatedPlacement(nodeSize, environment([], []))).toThrow()
  // Single node
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2([new Rect(new Vec2(1050, 690), defaultNodeSize)], -1),
    ),
    // 20px existing node height + 24px gap
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (far enough up that it does not overlap)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(1150, 714), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (far enough down that it does not overlap)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(1150, 754), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (far enough left that it does not overlap)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(926, 734), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (overlapping on the left by 1px)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(927, 734), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1051, 734) })
  // Single node (blocking initial position)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(1050, 734), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
    // 1050 + 100 (width) + 24 (gap)
  ).toEqual<Placement>({ position: new Vec2(1174, 734) })
  // Single node (far enough right that it does not overlap)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(1174, 690), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1050, 734) })
  // Single node (overlapping on the right by 1px)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(1173, 734), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
    // 1173 + 100 (width) + 24
  ).toEqual<Placement>({ position: new Vec2(1297, 734) })
  // Single node (overlaps on the top by 1px)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(1050, 715), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1174, 734) })
  // Single node (overlaps on the bottom by 1px)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(1050, 753), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1174, 734) })
  // Single node (BIG gap)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2([new Rect(new Vec2(1050, 690), defaultNodeSize)], -1),
      {
        gap: 1000,
      },
    ),
    // 20px existing node height + 1000px gap
  ).toEqual<Placement>({ position: new Vec2(1050, 1710), pan: new Vec2(0, 1020) })
  // Single node (BIG gap, overlapping on the left by 1px)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          new Rect(new Vec2(927, 1710), defaultNodeSize),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
      {
        gap: 1000,
      },
    ),
    // 927 + 100 (width) + 1000 (gap) = 2027
  ).toEqual<Placement>({ position: new Vec2(2027, 1710), pan: new Vec2(977, 1020) })
  // Multiple nodes
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          ...map(range(1000, 2001, 100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
    // The bottom-most node ends at y=500, and there should be a 24px gap.
  ).toEqual<Placement>({ position: new Vec2(2124, 734), pan: new Vec2(1074, 44) })
  // Multiple nodes (reverse)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          ...map(range(2000, 999, -100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(2124, 734), pan: new Vec2(1074, 44) })
  // Multiple nodes with gap
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          ...chain(
            map(range(1000, 1401, 100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
            map(range(1700, 2001, 100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          ),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
    // There is a 200px gap between x=1500 and x=1700, which is more than large enough for this node.
  ).toEqual<Placement>({ position: new Vec2(1524, 734) })
  // Multiple nodes with gap (just big enough)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        // 100px width*2 nodes + 24px*2 gap
        [
          ...map(range(1050, 2000, 248), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1174, 734) })
  // Multiple nodes with gap (slightly too small)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      // 1050 + 247 * 3 === 1791
      environment2(
        [
          ...map(range(1050, 1792, 247), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
    // 1791 + 100 + 24
  ).toEqual<Placement>({ position: new Vec2(1915, 734) })
  // Multiple nodes with smallest gap
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          ...chain(
            map(range(1000, 1401, 100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
            map(range(1648, 1949, 100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          ),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
    // The smallest gap between the top of adjacent nodes this can fit into, is
    // 100px width * 2 nodes + 24px gap * 2.
  ).toEqual<Placement>({ position: new Vec2(1524, 734) })
  // Multiple nodes with smallest gap (reverse)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          ...chain(
            map(range(1948, 1647, -100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
            map(range(1400, 999, -100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          ),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(1524, 734) })
  // Multiple nodes with gap that is too small
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          ...chain(
            map(range(1000, 1401, 100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
            map(range(1647, 1948, 100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          ),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
    // This gap is 1px smaller than the previous test - so, 1px too small.
  ).toEqual<Placement>({ position: new Vec2(2071, 734), pan: new Vec2(1021, 44) })
  // Multiple nodes with gap that is too small (each range reversed)
  expect(
    previousNodeDictatedPlacement(
      nodeSize,
      environment2(
        [
          ...chain(
            map(range(1400, 999, -100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
            map(range(1947, 1646, -100), (x) => new Rect(new Vec2(x, 734), defaultNodeSize)),
          ),
          new Rect(new Vec2(1050, 690), defaultNodeSize),
        ],
        -1,
      ),
    ),
  ).toEqual<Placement>({ position: new Vec2(2071, 734), pan: new Vec2(1021, 44) })
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
    expect(newNodeRect.top, {
      toString() {
        return generateVueCode(newNodeRect, nodes, selectedNodes)
      },
    } as string).toBeGreaterThanOrEqual(Math.max(...selectedNodes.map((node) => node.bottom)))
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
