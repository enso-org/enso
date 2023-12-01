import type { Opt } from '@/util/opt'
import { reactive, watch, type WatchSource } from 'vue'
import { createContextStore } from '.'

export { provideFn as provideAppClassSet }
const { provideFn, injectFn: injectAppClassSet } = createContextStore('Port info', () => {
  return reactive(new Map<string, number>())
})

export function useAppClass(watchSource: WatchSource<Opt<Record<string, boolean>>>) {
  const classSet = injectAppClassSet(true)
  if (classSet == null) return
  watch(
    watchSource,
    (newVal, _, onCleanup) => {
      if (newVal) {
        const classes = Object.keys(newVal).filter((key) => newVal[key])
        for (const key of classes) {
          classSet.set(key, (classSet.get(key) ?? 0) + 1)
        }

        onCleanup(() => {
          for (const key of classes) {
            const newCount = (classSet.get(key) ?? 0) - 1
            if (newCount > 0) {
              classSet.set(key, newCount)
            } else {
              classSet.delete(key)
            }
          }
        })
      }
    },
    { immediate: true },
  )
}
