import { useNavigator } from '@/composables/navigator'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { effectScope, ref } from 'vue'

describe('useNavigator', async () => {
  let scope = effectScope()
  beforeEach(() => {
    scope = effectScope()
  })
  afterEach(() => scope.stop())

  function makeTestNavigator() {
    return scope.run(() => {
      const node = document.createElement('div')
      vi.spyOn(node, 'getBoundingClientRect').mockReturnValue(new DOMRect(150, 150, 800, 400))
      const viewportNode = ref(node)
      return useNavigator(viewportNode)
    })!
  }

  test('initializes with centered non-zoomed viewport', () => {
    const navigator = makeTestNavigator()
    expect(navigator.viewport).toStrictEqual(Rect.FromBounds(-400, -200, 400, 200))
  })

  test('clientToScenePos without scaling', () => {
    const navigator = makeTestNavigator()
    expect(navigator.clientToScenePos(Vec2.Zero)).toStrictEqual(new Vec2(-550, -350))
    expect(navigator.clientToScenePos(new Vec2(150, 150))).toStrictEqual(new Vec2(-400, -200))
    expect(navigator.clientToScenePos(new Vec2(550, 350))).toStrictEqual(new Vec2(0, 0))
  })

  test('clientToScenePos with scaling', () => {
    const navigator = makeTestNavigator()
    navigator.scale = 2
    expect(navigator.clientToScenePos(Vec2.Zero)).toStrictEqual(new Vec2(-275, -175))
    expect(navigator.clientToScenePos(new Vec2(150, 150))).toStrictEqual(new Vec2(-200, -100))
    expect(navigator.clientToScenePos(new Vec2(550, 350))).toStrictEqual(new Vec2(0, 0))
  })

  test('clientToSceneRect without scaling', () => {
    const navigator = makeTestNavigator()
    expect(navigator.clientToSceneRect(Rect.Zero)).toStrictEqual(Rect.XYWH(-550, -350, 0, 0))
    expect(navigator.clientToSceneRect(Rect.XYWH(150, 150, 800, 400))).toStrictEqual(
      navigator.viewport,
    )
    expect(navigator.clientToSceneRect(Rect.XYWH(100, 150, 200, 900))).toStrictEqual(
      Rect.XYWH(-450, -200, 200, 900),
    )
  })

  test('clientToSceneRect with scaling', () => {
    const navigator = makeTestNavigator()
    navigator.scale = 2
    expect(navigator.clientToSceneRect(Rect.Zero)).toStrictEqual(Rect.XYWH(-275, -175, 0, 0))
    expect(navigator.clientToSceneRect(Rect.XYWH(150, 150, 800, 400))).toStrictEqual(
      navigator.viewport,
    )
    expect(navigator.clientToSceneRect(Rect.XYWH(100, 150, 200, 900))).toStrictEqual(
      Rect.XYWH(-225, -100, 100, 450),
    )
  })
})
