import { computed, proxyRefs, ref } from 'vue'
import { createContextStore } from '.'

export const [provideAnimationCounter, injectAnimationCounter] = createContextStore(
  'animation counter',
  () => {
    const parent: { count: number } = injectAnimationCounter(true) ?? { count: 0 }
    const localCount = ref(0)
    return proxyRefs({
      modify(change: number) {
        localCount.value += change
      },
      count: computed((): number => localCount.value + parent.count),
    })
  },
)
