import {
  collapsedNodePlacement,
  inputNodePlacement,
  mouseDictatedPlacement,
  nonDictatedPlacement,
  previousNodeDictatedPlacement,
  type Environment,
  type InputNodeEnvironment,
} from '@/components/ComponentBrowser/placement'
import * as iterable from '@/util/data/iterable'
import { chain, map, range } from '@/util/data/iterable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { fc, test as fcTest } from '@fast-check/vitest'
import { describe, expect, test } from 'vitest'

// Vue playground to visually inspect failing fuzz cases:
// https://play.vuejs.org/#eNrNU09PwjAU/ypNNeGCMPFC5jRR40EPatSbNXGMxyiMtmnfYGbZd/e1Y0Ci4Wwv6+/Pa3+v7Wp+Y8xgXQKPeeIyKw0yB1iaa6EyrRwyCxk6dsU+hPoU6pAlsmYFzDBmUZ+hNuG7kVOcx+w8ovkcZD4neRSxRqhk2G5ASxNAWJkiRSCUTOWaOfwu4ErwfU0UmeryYD0PBSc/Y6FifTbTlipCFqnapIKzuFuqZkY7iVKrmPXSidNFidDrbzN/nda+YuBRY6qvbQsdTaBltwE6PsBW6aJ2UotbbZJmy9zqUk1p75NxGKNRj86BXydDir/v41/mHdEYj3/l3c6S4e76eJ+jo0cxk/lg4bSih1R7q+CZXhlZgH02viW6mZgFxWtpUejNY+DQltDv+GwO2fIPfuEqzwn+YsGBXYPgOw1TmwO28v3bE1Q034krPS0Lch8RXyGcNGVsbbd0CBT7wBfSPqyMtihV/u7uKwTluqZ8UO9sgl9w+pvujrS+j3sxuAh1QjW8+QFAeS2/

const defaultScreenBounds = new Rect(new Vec2(100, 200), new Vec2(2000, 1000))
const size = new Vec2(100, 20)
const radius = size.y / 2
const gap = new Vec2(24, 24)

// Center is at (1100, 700)
const screenBounds = defaultScreenBounds

function rectAt(left: number, top: number) {
  return new Rect(new Vec2(left, top), size)
}

function rectAtX(left: number) {
  return (top: number) => rectAt(left, top)
}

function rectAtY(top: number) {
  return (left: number) => rectAt(left, top)
}

describe('Non dictated placement', () => {
  function nonDictatedEnvironment(nodeRects: Iterable<Rect>): Environment {
    return {
      screenBounds,
      nodeRects,
      selectedNodeRects: iterable.empty(),
    }
  }

  test.each([
    // === Miscellaneous tests ===
    { desc: 'Empty graph', nodes: [], pos: new Vec2(1090, 690) },

    // === Single node tests ===
    { desc: 'Single node', nodes: [rectAt(1050, 690)], pos: new Vec2(1090, 734) },
    //
    {
      desc: 'Single node (far enough left that it does not overlap)',
      nodes: [rectAt(990, 690)],
      pos: new Vec2(1090, 690),
    },
    {
      desc: 'Single node (far enough right that it does not overlap)',
      nodes: [rectAt(1190, 690)],
      pos: new Vec2(1090, 690),
    },
    {
      desc: 'Single node (overlaps on the left by 1px)',
      nodes: [rectAt(991, 690)],
      pos: new Vec2(1090, 734),
    },
    {
      desc: 'Single node (overlaps on the right by 1px)',
      nodes: [rectAt(1189, 690)],
      pos: new Vec2(1090, 734),
    },
    {
      desc: 'Single node (BIG gap)',
      nodes: [rectAt(1050, 690)],
      gap: 1000,
      pos: new Vec2(1090, 1710),
      pan: new Vec2(0, 1020),
    },

    // === Multiple node tests ===
    {
      desc: 'Multiple nodes',
      nodes: map(range(0, 1001, 20), rectAtX(1050)),
      pos: new Vec2(1090, 1044),
    },
    {
      desc: 'Multiple nodes with gap',
      nodes: map(range(1000, -1, -20), rectAtX(1050)),
      pos: new Vec2(1090, 1044),
    },
    {
      desc: 'Multiple nodes with gap 2',
      nodes: chain(
        map(range(500, 901, 20), rectAtX(1050)),
        map(range(1000, 1501, 20), rectAtX(1050)),
      ),
      pos: new Vec2(1090, 944),
    },
    {
      desc: 'Multiple nodes with gap (just big enough)',
      nodes: map(range(690, 1500, 88), rectAtX(1050)),
      pos: new Vec2(1090, 734),
    },
    {
      desc: 'Multiple nodes with gap (slightly too small)',
      nodes: map(range(500, 849, 87), rectAtX(1050)),
      pos: new Vec2(1090, 892),
    },
    {
      desc: 'Multiple nodes with smallest gap',
      nodes: chain(
        map(range(500, 901, 20), rectAtX(1050)),
        map(range(988, 1489, 20), rectAtX(1050)),
      ),
      pos: new Vec2(1090, 944),
    },
    {
      desc: 'Multiple nodes with smallest gap (reverse)',
      nodes: chain(
        map(range(1488, 987, -20), rectAtX(1050)),
        map(range(900, 499, -20), rectAtX(1050)),
      ),
      pos: new Vec2(1090, 944),
    },
    {
      desc: 'Multiple nodes with gap that is too small',
      nodes: chain(
        map(range(500, 901, 20), rectAtX(1050)),
        map(range(987, 1488, 20), rectAtX(1050)),
      ),
      // This gap is 1px smaller than the previous test - so, 1px too small.
      // This position is offscreen (y >= 1000), so we pan so that the new node is centered (1531 - 690).
      pos: new Vec2(1090, 1531),
      pan: new Vec2(0, 841),
    },
    {
      desc: 'Multiple nodes with gap that is too small (each range reversed)',
      nodes: chain(
        map(range(900, 499, -20), rectAtX(1050)),
        map(range(1487, 986, -20), rectAtX(1050)),
      ),
      pos: new Vec2(1090, 1531),
      pan: new Vec2(0, 841),
    },
  ])('$desc', ({ nodes, pos, gap }) => {
    expect(
      nonDictatedPlacement(size, nonDictatedEnvironment(nodes), new Vec2(gap ?? 24, gap ?? 24)),
    ).toEqual(pos)
  })

  fcTest.prop({
    nodeData: fc.array(
      fc.record({
        left: fc.nat(1000),
        top: fc.nat(1000),
        width: fc.nat(1000),
        height: fc.nat(1000),
      }),
      { minLength: 15, maxLength: 25 },
    ),
  })('prop testing', ({ nodeData }) => {
    const nodes = nodeData.map(
      ({ left, top, width, height }) => new Rect(new Vec2(left, top), new Vec2(width, height)),
    )
    const newNodeRect = new Rect(nonDictatedPlacement(size, nonDictatedEnvironment(nodes)), size)
    for (const node of nodes) {
      expect(node.intersects(newNodeRect), {
        toString() {
          return generateVueCodeForNonDictatedPlacement(newNodeRect, nodes)
        },
      } as string).toBe(false)
    }
  })
})

describe('Previous node dictated placement', () => {
  function previousNodeDictatedEnvironment(nodeRects: Rect[]): Environment {
    return {
      screenBounds,
      nodeRects,
      selectedNodeRects: nodeRects.slice(-1),
    }
  }

  test('Previous node dictated placement is equivalent to non-dictated placement when there are no nodes', () => {
    const environment = previousNodeDictatedEnvironment([])
    expect(previousNodeDictatedPlacement(size, environment)).toEqual(
      nonDictatedPlacement(size, environment),
    )
  })

  test.each([
    // === Single node tests ===
    { desc: 'Single node', nodes: [], pos: new Vec2(1090, 734) },
    {
      desc: 'Single node (far enough up that it does not overlap)',
      nodes: [rectAt(1189, 714)],
      pos: new Vec2(1090, 734),
    },
    {
      desc: 'Single node (far enough down that it does not overlap)',
      nodes: [rectAt(1189, 754)],
      pos: new Vec2(1090, 734),
    },
    {
      desc: 'Single node (far enough left that it does not overlap)',
      nodes: [rectAt(966, 734)],
      pos: new Vec2(1090, 734),
    },
    {
      desc: 'Single node (overlapping on the left by 1px)',
      nodes: [rectAt(967, 734)],
      pos: new Vec2(1091, 734),
    },
    {
      desc: 'Single node (blocking initial position)',
      nodes: [rectAt(1090, 734)],
      pos: new Vec2(1214, 734),
    },
    {
      desc: 'Single node (far enough right that it does not overlap)',
      nodes: [rectAt(1174, 690)],
      pos: new Vec2(1090, 734),
    },
    {
      desc: 'Single node (overlapping on the right by 1px)',
      nodes: [rectAt(1173, 734)],
      pos: new Vec2(1297, 734),
    },
    {
      desc: 'Single node (overlaps on the top by 1px)',
      nodes: [rectAt(1050, 715)],
      pos: new Vec2(1174, 734),
    },
    {
      desc: 'Single node (overlaps on the bottom by 1px)',
      nodes: [rectAt(1050, 753)],
      pos: new Vec2(1174, 734),
    },
    {
      desc: 'Single node (BIG gap)',
      nodes: [],
      gap: 1000,
      pos: new Vec2(1090, 1710),
      pan: new Vec2(0, 1020),
    },
    {
      desc: 'Single node (BIG gap, overlapping on the left by 1px)',
      nodes: [rectAt(967, 1710)],
      gap: 1000,
      pos: new Vec2(2067, 1710),
      pan: new Vec2(977, 1020),
    },

    // === Multiple node tests ===
    {
      desc: 'Multiple nodes',
      nodes: map(range(1000, 2001, 100), rectAtY(734)),
      pos: new Vec2(2124, 734),
      pan: new Vec2(1034, 44),
    },
    {
      desc: 'Multiple nodes (reverse)',
      nodes: map(range(2000, 999, -100), rectAtY(734)),
      pos: new Vec2(2124, 734),
      pan: new Vec2(1034, 44),
    },
    {
      desc: 'Multiple nodes with gap',
      nodes: chain(
        map(range(1000, 1401, 100), rectAtY(734)),
        map(range(1700, 2001, 100), rectAtY(734)),
      ),
      pos: new Vec2(1524, 734),
    },
    {
      desc: 'Multiple nodes with gap (just big enough)',
      nodes: map(range(1050, 2000, 248), rectAtY(734)),
      pos: new Vec2(1174, 734),
    },
    {
      desc: 'Multiple nodes with gap (slightly too small)',
      nodes: map(range(1050, 1792, 247), rectAtY(734)),
      pos: new Vec2(1915, 734),
    },
    {
      desc: 'Multiple nodes with smallest gap',
      nodes: chain(
        map(range(1000, 1401, 100), rectAtY(734)),
        map(range(1648, 1949, 100), rectAtY(734)),
      ),
      pos: new Vec2(1524, 734),
    },
    {
      desc: 'Multiple nodes with smallest gap (reverse)',
      nodes: chain(
        map(range(1948, 1647, -100), rectAtY(734)),
        map(range(1400, 999, -100), rectAtY(734)),
      ),
      pos: new Vec2(1524, 734),
    },
    {
      desc: 'Multiple nodes with gap that is too small',
      nodes: chain(
        map(range(1000, 1401, 100), rectAtY(734)),
        map(range(1647, 1948, 100), rectAtY(734)),
      ),
      pos: new Vec2(2071, 734),
      pan: new Vec2(981, 44),
    },
    {
      desc: 'Multiple nodes with gap that is too small (each range reversed)',
      nodes: chain(
        map(range(1400, 999, -100), rectAtY(734)),
        map(range(1947, 1646, -100), rectAtY(734)),
      ),
      pos: new Vec2(2071, 734),
      pan: new Vec2(981, 44),
    },
  ])('$desc', ({ nodes, gap, pos }) => {
    expect(
      previousNodeDictatedPlacement(
        size,
        previousNodeDictatedEnvironment([...nodes, rectAt(1090, 690)]),
        new Vec2(gap ?? 24, gap ?? 24),
      ),
    ).toEqual(pos)
  })

  fcTest.prop({
    nodeData: fc.array(
      fc.record({
        left: fc.nat(1000),
        top: fc.nat(1000),
        width: fc.nat(1000),
        height: fc.nat(1000),
      }),
      { minLength: 15, maxLength: 25 },
    ),
    firstSelectedNode: fc.integer({ min: 7, max: 12 }),
  })('prop testing', ({ nodeData, firstSelectedNode }) => {
    const nodeRects = nodeData.map(
      ({ left, top, width, height }) => new Rect(new Vec2(left, top), new Vec2(width, height)),
    )
    const selectedNodeRects = nodeRects.slice(firstSelectedNode)
    const newNodeRect = new Rect(
      previousNodeDictatedPlacement(size, {
        screenBounds,
        nodeRects,
        selectedNodeRects,
      }),
      size,
    )
    expect(newNodeRect.top, {
      toString() {
        return generateVueCodeForPreviousNodeDictatedPlacement(
          newNodeRect,
          nodeRects,
          selectedNodeRects,
        )
      },
    } as string).toBeGreaterThanOrEqual(Math.max(...selectedNodeRects.map((node) => node.bottom)))
    for (const node of nodeRects) {
      expect(node.intersects(newNodeRect), {
        toString() {
          return generateVueCodeForPreviousNodeDictatedPlacement(
            newNodeRect,
            nodeRects,
            selectedNodeRects,
          )
        },
      } as string).toBe(false)
    }
  })
})

describe('Mouse dictated placement', () => {
  fcTest.prop({
    x: fc.nat(1000),
    y: fc.nat(1000),
  })('prop testing', ({ x, y }) => {
    expect(mouseDictatedPlacement(new Vec2(x, y), size)).toEqual(
      // Note: Currently, this is a reimplementation of the entire mouse dictated placement algorithm.
      new Vec2(x + radius, y + radius),
    )
    // Non-overlap test omitted, as mouse-dictated node placement MAY overlap existing nodes.
  })
})

describe('Collapsed node placement', () => {
  function environment(selectedNodeRects: Rect[], nonSelectedNodeRects: Rect[]): Environment {
    return {
      screenBounds,
      nodeRects: [...selectedNodeRects, ...nonSelectedNodeRects],
      selectedNodeRects,
    }
  }

  test('One selected, no other nodes', () => {
    const X = 1100
    const Y = 700
    const selectedNodeRects = [rectAt(X, Y)]
    const result = collapsedNodePlacement(size, environment(selectedNodeRects, []), gap)
    expect(result).toEqual(new Vec2(X, Y))
  })

  test('Multiple selected, no other nodes', () => {
    const selectedNodeRects = [rectAt(1000, 600), rectAt(1300, 800)]
    const result = collapsedNodePlacement(size, environment(selectedNodeRects, []), gap)
    expect(result).toEqual(new Vec2(1000, 700))
  })

  test('Average position occupied', () => {
    const selectedNodeRects = [rectAt(1000, 600), rectAt(1300, 800)]
    const result = collapsedNodePlacement(
      size,
      environment(selectedNodeRects, [rectAt(1000, 700)]),
      gap,
    )
    expect(result).toEqual(new Vec2(1000, 744))
  })
})

describe('Input node placement', () => {
  function environment(inputNodeRects: Rect[], nonInputNodeRects: Rect[]): InputNodeEnvironment {
    return {
      screenBounds,
      nodeRects: [...inputNodeRects, ...nonInputNodeRects],
      nonInputNodeRects: nonInputNodeRects,
      selectedNodeRects: [],
    }
  }

  test('No input nodes, single component', () => {
    const X = 1100
    const Y = 700
    const nonInputNodeRects = [rectAt(X, Y)]
    const result = inputNodePlacement(size, environment([], nonInputNodeRects), gap)
    expect(result).toEqual(new Vec2(X, 656))
  })

  test('No input nodes, two components', () => {
    const nonInputNodeRects = [rectAt(1000, 600), rectAt(1300, 800)]
    const result = inputNodePlacement(size, environment([], nonInputNodeRects), gap)
    expect(result).toEqual(new Vec2(1000, 556))
  })

  test('One input node, two components', () => {
    const nonInputNodeRects = [rectAt(1000, 600), rectAt(1300, 800)]
    const inputNodeRects = [rectAt(1000, 556)]
    const result = inputNodePlacement(size, environment(inputNodeRects, nonInputNodeRects), gap)
    expect(result).toEqual(new Vec2(1124, 556))
  })
})

// === Helpers for debugging ===

function generateVueCodeForNonDictatedPlacement(newNode: Rect, rects: Rect[]) {
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

function generateVueCodeForPreviousNodeDictatedPlacement(
  newNode: Rect,
  rects: Rect[],
  selectedRects: Rect[],
) {
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
