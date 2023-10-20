import {
  mouseDictatedPlacement,
  nonDictatedPlacement,
  type Environment,
  type Placement,
  type PlacementOptions,
} from '@/components/ComponentBrowser/placement'
import { Rect } from '@/stores/rect'
import * as iterable from '@/util/iterable'
import { chain, map, range } from '@/util/iterable'
import { Vec2 } from '@/util/vec2'
import { expect, test, vi } from 'vitest'

const defaultScreenBounds = new Rect(new Vec2(100, 200), new Vec2(2000, 1000))
const defaultNodeSize = new Vec2(100, 20)

test('Non dictated placement', () => {
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
  expect(getSelectedNodeRects, 'Should not depend on `selectedNodeRects`').toBeCalledTimes(0)
  expect(getMousePosition, 'Should not depend on `mousePosition`').toBeCalledTimes(0)
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
  expect(getScreenBounds, 'Should not depend on `screenBounds`').toBeCalledTimes(0)
  expect(getNodeRects, 'Should not depend on `nodeRects`').toBeCalledTimes(0)
  expect(getSelectedNodeRects, 'Should not depend on `selectedNodeRects`').toBeCalledTimes(0)
  expect(getGap, 'Should not depend on `gap`').toBeCalledTimes(0)
})
