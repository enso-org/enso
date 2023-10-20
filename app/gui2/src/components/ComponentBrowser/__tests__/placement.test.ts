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
  // Found by fuzzing. Ensures that `Math.max` is present in the `if (lastDeletedElement)` block.
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment([
        new Rect(new Vec2(212, 831), new Vec2(1171 - 212, 1024 - 831)),
        new Rect(new Vec2(261, 680), new Vec2(1137 - 261, 1525 - 680)),
      ]),
    ).position,
  ).toEqual(new Vec2(1050, 1549))
  // Found by fuzzing.
  expect(
    nonDictatedPlacement(
      nodeSize,
      environment([
        new Rect(new Vec2(958, 902), new Vec2(1481 - 958, 973 - 902)),
        new Rect(new Vec2(973, 777), new Vec2(1092 - 973, 1571 - 777)),
        new Rect(new Vec2(991, 30), new Vec2(1981 - 991, 533 - 30)),
        new Rect(new Vec2(576, 287), new Vec2(1022 - 576, 464 - 287)),
        new Rect(new Vec2(173, 520), new Vec2(429 - 173, 1337 - 520)),
        new Rect(new Vec2(858, 98), new Vec2(1002 - 858, 991 - 98)),
        new Rect(new Vec2(750, 547), new Vec2(1703 - 750, 1011 - 547)),
        new Rect(new Vec2(901, 933), new Vec2(1563 - 901, 990 - 933)),
        new Rect(new Vec2(971, 686), new Vec2(1513 - 971, 784 - 686)),
        new Rect(new Vec2(617, 784), new Vec2(831 - 617, 1237 - 784)),
        new Rect(new Vec2(137, 815), new Vec2(922 - 137, 847 - 815)),
        new Rect(new Vec2(48, 277), new Vec2(509 - 48, 1024 - 277)),
        new Rect(new Vec2(43, 651), new Vec2(1015 - 43, 1019 - 651)),
        new Rect(new Vec2(287, 274), new Vec2(1167 - 287, 335 - 274)),
        new Rect(new Vec2(186, 252), new Vec2(572 - 186, 1061 - 252)),
        new Rect(new Vec2(60, 972), new Vec2(149 - 60, 1197 - 972)),
      ]),
    ).position,
  ).toEqual(new Vec2(1050, 1595))
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
  // Found by fuzzing. Ensures that `Math.max` is present in the `if (lastDeletedElement)` block.
  {
    const rects = [
      new Rect(new Vec2(600, 695), new Vec2(795 - 600, 1587 - 695)),
      new Rect(new Vec2(393, 625), new Vec2(806 - 393, 1482 - 625)),

      new Rect(new Vec2(292, 665), new Vec2(395 - 292, 752 - 665)),
      new Rect(new Vec2(578, 475), new Vec2(826 - 578, 1450 - 475)),
    ]
    const selectedRects = rects.slice(2)
    expect(
      previousNodeDictatedPlacement(nodeSize, environment(rects, selectedRects)).position,
    ).toEqual(new Vec2(830, 1474))
  }
  // Found by fuzzing. Caused by `range.left = rect.right` instead of `range.left = rect.left`.
  {
    const rects = [
      new Rect(new Vec2(923, 966), new Vec2(1704 - 923, 1596 - 966)),
      new Rect(new Vec2(510, 757), new Vec2(843 - 510, 1371 - 757)),
      new Rect(new Vec2(193, 181), new Vec2(1177 - 193, 970 - 181)),
      new Rect(new Vec2(841, 936), new Vec2(1421 - 841, 1611 - 936)),
      new Rect(new Vec2(297, 978), new Vec2(1288 - 297, 1224 - 978)),
      new Rect(new Vec2(454, 998), new Vec2(958 - 454, 1545 - 998)),
      new Rect(new Vec2(54, 996), new Vec2(955 - 54, 1808 - 996)),

      new Rect(new Vec2(53, 307), new Vec2(963 - 53, 454 - 307)),
      new Rect(new Vec2(246, 694), new Vec2(536 - 246, 1345 - 694)),
    ]
    const selectedRects = rects.slice(7)
    expect(
      previousNodeDictatedPlacement(nodeSize, environment(rects, selectedRects)).position,
    ).toEqual(new Vec2(1728, 1369))
  }
  {
    // selectedRects starts at left=289
    const rects = [
      new Rect(new Vec2(928, 932), new Vec2(1120 - 928, 1782 - 932)),
      new Rect(new Vec2(612, 987), new Vec2(1469 - 612, 1747 - 987)),
      new Rect(new Vec2(239, 780), new Vec2(738 - 239, 1456 - 780)),
      new Rect(new Vec2(112, 800), new Vec2(565 - 112, 1737 - 800)),
      new Rect(new Vec2(297, 766), new Vec2(556 - 297, 1672 - 766)),
      new Rect(new Vec2(109, 223), new Vec2(597 - 109, 252 - 223)),
      new Rect(new Vec2(355, 513), new Vec2(777 - 355, 864 - 513)),
      new Rect(new Vec2(449, 267), new Vec2(771 - 449, 1166 - 267)),
      new Rect(new Vec2(582, 588), new Vec2(609 - 582, 998 - 588)),
      new Rect(new Vec2(740, 767), new Vec2(1078 - 740, 1310 - 767)),
      new Rect(new Vec2(289, 799), new Vec2(1067 - 289, 852 - 799)),
      new Rect(new Vec2(913, 730), new Vec2(1518 - 913, 1122 - 730)),
      new Rect(new Vec2(208, 882), new Vec2(366 - 208, 983 - 882)),
      new Rect(new Vec2(124, 574), new Vec2(364 - 124, 1326 - 574)),
      new Rect(new Vec2(474, 757), new Vec2(575 - 474, 1682 - 757)),
      new Rect(new Vec2(650, 730), new Vec2(930 - 650, 1069 - 730)),
      new Rect(new Vec2(272, 192), new Vec2(761 - 272, 1109 - 192)),
      new Rect(new Vec2(268, 784), new Vec2(389 - 268, 857 - 784)),
      new Rect(new Vec2(359, 261), new Vec2(951 - 359, 721 - 261)),
      new Rect(new Vec2(64, 976), new Vec2(494 - 64, 1475 - 976)),
    ]
    const selectedRects = rects.slice(10)
    expect(
      previousNodeDictatedPlacement(nodeSize, environment(rects, selectedRects)).position,
    ).toEqual(new Vec2(1493, 1706))
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
