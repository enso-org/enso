import { expect, test, vi } from 'vitest'
import { nextTick, reactive, ref } from 'vue'
import { LazySyncEffectSet } from '../reactivity'

test('LazySyncEffectSet', async () => {
  const lazySet = new LazySyncEffectSet()

  const key1 = ref(0)
  const key2 = ref(100)
  const lazilyUpdatedMap = reactive(new Map<number, string>())

  let runCount = 0
  const stopA = lazySet.lazyEffect((onCleanup) => {
    const currentValue = key1.value
    lazilyUpdatedMap.set(currentValue, 'a' + runCount++)
    onCleanup(() => lazilyUpdatedMap.delete(currentValue))
  })

  lazySet.lazyEffect((onCleanup) => {
    const currentValue = key2.value
    lazilyUpdatedMap.set(currentValue, 'b' + runCount++)
    onCleanup(() => lazilyUpdatedMap.delete(currentValue))
  })

  // Dependant effect, notices when -1 key is inserted into the map by another effect.
  const cleanupSpy = vi.fn()
  lazySet.lazyEffect((onCleanup) => {
    const negOne = lazilyUpdatedMap.get(-1)
    if (negOne != null) {
      lazilyUpdatedMap.set(-2, `noticed ${negOne}!`)
      onCleanup(() => {
        cleanupSpy()
        lazilyUpdatedMap.delete(-2)
      })
    }
  })

  expect(lazilyUpdatedMap, 'The effects should not run immediately after registration').toEqual(
    new Map([]),
  )

  key1.value = 1
  expect(lazilyUpdatedMap, 'The effects should not perform any updates until flush').toEqual(
    new Map([]),
  )

  key1.value = 2
  lazySet.flush()
  expect(
    lazilyUpdatedMap,
    'A cleanup and update should run on flush, but only for the updated key',
  ).toEqual(
    new Map([
      [2, 'a0'],
      [100, 'b1'],
    ]),
  )

  key1.value = 3
  key2.value = 103
  stopA()
  expect(
    lazilyUpdatedMap,
    'Stop should immediately trigger cleanup, but only for stopped effect',
  ).toEqual(new Map([[100, 'b1']]))

  lazySet.flush()
  expect(
    lazilyUpdatedMap,
    'Flush should trigger remaining updates, but not run the stopped effects',
  ).toEqual(new Map([[103, 'b2']]))

  key1.value = 4
  key2.value = 104
  lazySet.lazyEffect((onCleanup) => {
    const currentValue = key1.value
    console.log('currentValue', currentValue)
    console.log('lazilyUpdatedMap', lazilyUpdatedMap)

    lazilyUpdatedMap.set(currentValue, 'c' + runCount++)
    onCleanup(() => lazilyUpdatedMap.delete(currentValue))
  })
  expect(
    lazilyUpdatedMap,
    'Newly registered effect should not run immediately nor trigger a flush',
  ).toEqual(new Map([[103, 'b2']]))

  key1.value = 5
  key2.value = 105
  lazySet.flush()
  expect(
    lazilyUpdatedMap,
    'Flush should trigger both effects when their dependencies change',
  ).toEqual(
    new Map([
      [105, 'b3'],
      [5, 'c4'],
    ]),
  )

  lazySet.flush()
  expect(lazilyUpdatedMap, 'Flush should have no effect when no dependencies changed').toEqual(
    new Map([
      [105, 'b3'],
      [5, 'c4'],
    ]),
  )

  key2.value = -1
  lazySet.flush()
  expect(lazilyUpdatedMap, 'Effects depending on one another should run in the same flush').toEqual(
    new Map([
      [5, 'c4'],
      [-1, 'b5'],
      [-2, 'noticed b5!'],
    ]),
  )

  key2.value = 1
  lazySet.flush()
  expect(cleanupSpy).toHaveBeenCalledTimes(1)
  expect(lazilyUpdatedMap, 'Dependant effect is cleaned up.').toEqual(
    new Map([
      [1, 'b6'],
      [5, 'c4'],
    ]),
  )

  key2.value = 2
  lazySet.flush()
  key2.value = -3
  lazySet.flush()
  expect(cleanupSpy, 'Cleanup runs only once.').toHaveBeenCalledTimes(1)

  key1.value = -1
  key2.value = 456
  await nextTick()
  expect(lazilyUpdatedMap, 'Flush should run automatically before the next tick.').toEqual(
    new Map([
      [456, 'b10'],
      [-1, 'c9'],
      [-2, 'noticed c9!'],
    ]),
  )
})
