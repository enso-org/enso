import { type VueWrapper } from '@vue/test-utils'
import { nextTick } from 'vue'

// It is currently not feasible to use generics here, as the type of the component's emits
// is not exposed.
export function handleEmit(wrapper: VueWrapper<any>, event: string, fn: (...args: any[]) => void) {
  let previousLength = 0
  return {
    async run() {
      const emitted = wrapper.emitted(event)
      if (!emitted) return
      for (let i = previousLength; i < emitted.length; i += 1) {
        fn(...emitted[i]!)
      }
      previousLength = emitted.length
      await nextTick()
    },
  }
}
