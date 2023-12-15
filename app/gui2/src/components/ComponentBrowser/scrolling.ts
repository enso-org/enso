import { useApproach } from '@/composables/animation'
import { computed, ref } from 'vue'

export type ScrollTarget =
  | { type: 'bottom' }
  | { type: 'selected' }
  | { type: 'offset'; offset: number }

export function useScrolling(
  selectedPos: { value: number },
  scrollerSize: { value: number },
  contentSize: { value: number },
  entrySize: number,
) {
  const targetScroll = ref<ScrollTarget>({ type: 'bottom' })
  const targetScrollPosition = computed(() => {
    switch (targetScroll.value.type) {
      case 'selected':
        return Math.max(selectedPos.value - scrollerSize.value + entrySize, 0)
      case 'bottom':
        return contentSize.value - scrollerSize.value
      case 'offset':
        return targetScroll.value.offset
    }
    return 0.0
  })
  const scrollTransitionTarget = ref(0.0)
  const scrollTransition = useApproach(scrollTransitionTarget)
  const scrollPosition = computed(() => targetScrollPosition.value + scrollTransition.value)

  function scrollWithTransition(target: ScrollTarget) {
    const old = scrollPosition.value
    targetScroll.value = target
    const change = scrollPosition.value - old
    scrollTransitionTarget.value = -change
    scrollTransition.skip()
    scrollTransitionTarget.value = 0.0
  }

  return {
    targetScroll,
    scrollPosition,
    scrollWithTransition,
  }
}
