<script lang="ts">
/**
 * A component for performing flawless layout-based (width, height, margin, etc.) show and hide
 * transitions. Works with any style definitions, including default `auto` size. Gracefuly supports
 * canceled animations, such as switching to "leaving" state in the middle of "entering" animation,
 * without causing any visual jumps. The animation always starts from most recent element position.
 *
 * Implemented in terms of Vue's `Transition`, but opting out from class-based transitions. Instead,
 * the animation is driven by [Web Animations API][api], and element's "full size" style shapshot is
 * used as a final keyframe.
 *
 * [api]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Animations_API
 */
export default {}
</script>
<script setup lang="ts">
import { hookBeforeFunctionCall } from '@/util/patching'
import { nextTick } from 'vue'

const PROGRESS_VAR = '--size-transition-progress'

const props = withDefaults(
  defineProps<{
    /** Enable width transition. Implies `overflow-x: clip` during animation. */
    width?: boolean
    /** Enable height transition. Implies `overflow-y: clip` during animation. */
    height?: boolean
    /** Compensate for parent grid or flexbox `gap` by animating `margin-left` from negative value. */
    leftGap?: boolean
    /** Total animation duration in milliseconds. */
    duration?: number
    /** Animation easing function. Can be any CSS easing function. */
    easing?: string
  }>(),
  { duration: 200, easing: 'ease-out' },
)

type Done = (cancelled: boolean) => void
type StyleSnapshot = { width: string; height: string; marginLeft: string; progress: string }
const styleSnapshots = new WeakMap<HTMLElement, StyleSnapshot>()
const animationsMap = new WeakMap<HTMLElement, Animation>()
let leavingElement: HTMLElement | undefined = undefined

function onEnter(e: Element, done: Done) {
  if (e instanceof HTMLElement) runAnimation(e, done, true)
}

function onBeforeLeave(e: Element) {
  if (e instanceof HTMLElement) {
    leavingElement = e
    snapshotCurrentStyles(e)
  }
}

function onLeave(e: Element, done: Done) {
  if (e instanceof HTMLElement) {
    // In case the "leave" animation is interrupted by another "enter", we need to grab a
    // style shapshot from leaving element right before it is removed from DOM.
    //
    // HACK: Unfortunately, all standard Vue transition hooks are called after the element has
    // already been removed from the document. In fact, `leaveCancelled` and `afterLeave` are called
    // right after that happens. In order to intercept the removal, we need to gently override the
    // vue's internal hook that performs the removal and calls the aforementioned public hooks.
    // See: https://github.com/vuejs/core/blob/ca84316bfb3410efe21333670a6ad5cd21857396/packages/runtime-core/src/components/BaseTransition.ts#L363-L371
    {
      const leaveCbKey = Object.getOwnPropertySymbols(e).find((s) => String(s).includes('_leaveCb'))
      if (leaveCbKey) hookBeforeFunctionCall(e, leaveCbKey, () => snapshotCurrentStyles(e))
    }

    runAnimation(e, done, false)
  }
}

function onAfterLeave(e: Element) {
  nextTick(() => {
    if (leavingElement === e) leavingElement = undefined
  })
}

function snapshotCurrentStyles(e: HTMLElement) {
  const computedStyle = getComputedStyle(e)
  const { width, height, marginLeft } = computedStyle
  const progress = computedStyle.getPropertyValue(PROGRESS_VAR)
  styleSnapshots.set(e, { width, height, marginLeft, progress })
}

function runAnimation(e: HTMLElement, done: Done, isEnter: boolean) {
  const lastSnapshot =
    styleSnapshots.get(e) ?? (leavingElement ? styleSnapshots.get(leavingElement) : undefined)

  // If there already is an animation running, stop it before reading final state styles.
  const prevAnimation = animationsMap.get(e)
  if (prevAnimation && !prevAnimation.finished) prevAnimation.cancel()
  const current = getComputedStyle(e)

  const start: Keyframe = { composite: 'replace', easing: props.easing }
  const end: Keyframe = { composite: 'replace', easing: props.easing }
  start[PROGRESS_VAR] = parseFloat(lastSnapshot?.progress || '0')
  end[PROGRESS_VAR] = isEnter ? 1 : 0

  if (props.width) {
    start.width = lastSnapshot?.width || '0px'
    end.width = isEnter ? current.width : '0px'
    start.overflowX = end.overflowX = 'clip'
    start.minWidth = '0px'
    end.minWidth = '0px'
  }
  if (props.height) {
    start.height = lastSnapshot?.height || '0px'
    end.height = isEnter ? current.height : '0px'
    start.overflowY = end.overflowY = 'clip'
  }
  if (props.leftGap && e.parentElement) {
    const parentStyle = getComputedStyle(e.parentElement)
    const negativeGap = `calc(${parentStyle.gap} * -1)`
    start.marginLeft = lastSnapshot?.marginLeft || negativeGap
    end.marginLeft = isEnter ? current.marginLeft : negativeGap
  }
  const animation = e.animate([start, end], { duration: props.duration })
  animation.addEventListener('finish', () => {
    cleanup(e)
    done(false)
  })
  animation.addEventListener('cancel', () => {
    cleanup(e)
    done(true)
  })
  e.dataset['transitioning'] = isEnter ? 'enter' : 'leave'
  animation.play()
  animationsMap.set(e, animation)
}

function cleanup(e: HTMLElement) {
  delete e.dataset['transitioning']
}
</script>

<template>
  <Transition
    :css="false"
    @enter="onEnter"
    @beforeLeave="onBeforeLeave"
    @leave="onLeave"
    @afterLeave="onAfterLeave"
  >
    <slot />
  </Transition>
</template>
<style scoped>
@property --size-transition-progress {
  syntax: '<number>';
  initial-value: 1;
  inherits: true;
}
</style>
