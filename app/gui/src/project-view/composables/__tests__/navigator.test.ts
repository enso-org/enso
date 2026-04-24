import { useNavigator } from '@/composables/navigator'
import { provideGlobalEventRegistry } from '@/providers/globalEventRegistry'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { withSetup } from '@/util/testing'
import { describe, expect, test, vi } from 'vitest'
import { ref } from 'vue'
import { useGlobalKeyboard } from '../keyboard'

describe('useNavigator', () => {
  function makeTestNavigator() {
    return withSetup(() => {
      const node = document.createElement('div')
      vi.spyOn(node, 'getBoundingClientRect').mockReturnValue(new DOMRect(150, 150, 800, 400))
      const viewportNode = ref(node)
      const globalEventRegistry = provideGlobalEventRegistry()
      const keyboard = useGlobalKeyboard(globalEventRegistry)
      return useNavigator(viewportNode, keyboard)
    })
  }

  test('initializes with non-zoomed viewport', async () => {
    const navigator = await makeTestNavigator()
    expect(navigator.viewport).toStrictEqual(Rect.FromBounds(0, 0, 800, 400))
  })

  test('clientToScenePos without scaling', async () => {
    const navigator = await makeTestNavigator()
    expect(navigator.clientToScenePos(Vec2.Zero)).toStrictEqual(new Vec2(-150, -150))
    expect(navigator.clientToScenePos(new Vec2(150, 150))).toStrictEqual(new Vec2(0, 0))
    expect(navigator.clientToScenePos(new Vec2(550, 350))).toStrictEqual(new Vec2(400, 200))
    expect(navigator.clientToScenePos(new Vec2(950, 550))).toStrictEqual(new Vec2(800, 400))
  })

  test('clientToScenePos with scaling', async () => {
    const navigator = await makeTestNavigator()
    navigator.setPosAndScale(Vec2.Zero, 2)
    expect(navigator.clientToScenePos(Vec2.Zero)).toStrictEqual(new Vec2(-75, -75))
    expect(navigator.clientToScenePos(new Vec2(150, 150))).toStrictEqual(new Vec2(0, 0))
    expect(navigator.clientToScenePos(new Vec2(550, 350))).toStrictEqual(new Vec2(200, 100))
    expect(navigator.clientToScenePos(new Vec2(950, 550))).toStrictEqual(new Vec2(400, 200))
  })

  test('clientToSceneRect without scaling', async () => {
    const navigator = await makeTestNavigator()
    expect(navigator.clientToSceneRect(Rect.Zero)).toStrictEqual(Rect.XYWH(-150, -150, 0, 0))
    expect(navigator.clientToSceneRect(Rect.XYWH(150, 150, 800, 400))).toStrictEqual(
      navigator.viewport,
    )
    expect(navigator.clientToSceneRect(Rect.XYWH(100, 150, 200, 900))).toStrictEqual(
      Rect.XYWH(-50, 0, 200, 900),
    )
  })

  test('clientToSceneRect with scaling', async () => {
    const navigator = await makeTestNavigator()
    navigator.setPosAndScale(Vec2.Zero, 2)
    expect(navigator.clientToSceneRect(Rect.Zero)).toStrictEqual(Rect.XYWH(-75, -75, 0, 0))
    expect(navigator.clientToSceneRect(Rect.XYWH(150, 150, 800, 400))).toStrictEqual(
      navigator.viewport,
    )
    expect(navigator.clientToSceneRect(Rect.XYWH(100, 150, 200, 900))).toStrictEqual(
      Rect.XYWH(-25, 0, 100, 450),
    )
  })
})
