import { proxyRefs } from '@/util/reactivity'
import { computed, nextTick, onScopeDispose, ref, watch, type WatchSource } from 'vue'
import { createContextStore } from '.'

/**
 * A counter context that is counting actively running javascript-based layout animations,
 * interfaced with using `useAnimationReporter` and `useAnimationsState` composables.
 *
 * This context supports nesting. Any animation reports within a child context will be
 * also reported back to parent contexts, to make sure that `anyAnimationActive` property
 * is correctly representing the whole component subtree, even when nesting is present.
 */
const [provideAnimationCounter, injectAnimationCounter] = createContextStore(
  'animation counter',
  () => {
    const parent = injectAnimationCounter(true)
    const reportedCount = ref(0)

    let disposed = false
    onScopeDispose(() => {
      // See `{@link useLayoutAnimationReporter}().reportAnimationEnded` for reasoning behind using rAF here.
      requestAnimationFrame(() => {
        modifyCount(-reportedCount.value)
        disposed = true
      })
    })

    function modifyCount(change: number) {
      if (disposed) return
      reportedCount.value += change
      parent?.modifyCount(change)
    }

    return {
      modifyCount,
      anyAnimationActive: computed(() => reportedCount.value > 0),
    }
  },
)

/**
 * A composable that allows reporting information about currently scheduled javascript animations
 * that can affect DOM element layout, so that any parent component can temporarily enable extra
 * logic necessary to track them.
 *
 * This is NOT supposed to be used for counting CSS-driven animations, as those can already be discovered
 * using `animationstart` and similar events.
 *
 * See `useLayoutAnimationsState` for use on the receiver end.
 */
export const useLayoutAnimationReporter = () => {
  const counter = injectAnimationCounter(true)

  /**
   * Notify that JS-driven animation affecting DOM node positions and sizes has started. Each call must be
   * paired with **exactly one** call to {@link reportAnimationEnded}.
   *
   */
  function reportAnimationStarted() {
    counter?.modifyCount(1)
  }

  /**
   * Notify that an animation previously reported by {@link reportAnimationStarted} has ended and will no longer
   * modify DOM node positions. Must be called **exactly once** per each invocation of {@link reportAnimationStarted}.
   */
  function reportAnimationEnded() {
    // RequestAnimationFrame is used to ensure that at least one frame has passed since the animation stop
    // was triggered on the JS side. That way the active state is maintained for the full duration of the
    // last animation frame, and any tracking logic has a chance to react to the final element position.
    requestAnimationFrame(() => counter?.modifyCount(-1))
  }

  function reportAnimationWhile(source: WatchSource<boolean>) {
    watch(
      source,
      (report, _, onCleanup) => {
        if (report) {
          reportAnimationStarted()
          onCleanup(() => nextTick(reportAnimationEnded))
        }
      },
      { flush: 'post', immediate: true },
    )
  }

  return {
    reportAnimationStarted,
    reportAnimationEnded,
    reportAnimationWhile,
  }
}

/**
 * A composable that receives information about wheter any potentially layout-shifting animations
 * are currently playing within any of the child components.
 *
 * See `useLayoutAnimationReporter` for use on the animation component end.
 */
export const useLayoutAnimationsState = () => {
  const counter = provideAnimationCounter()
  return proxyRefs({
    anyAnimationActive: counter.anyAnimationActive,
  })
}
