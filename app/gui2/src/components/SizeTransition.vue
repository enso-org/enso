<script setup lang="ts" functional>
import { nextTick } from 'process'

const props = withDefaults(
  defineProps<{
    width?: boolean
    height?: boolean
    leftGap?: boolean
    duration?: number
    easing?: string
  }>(),
  { duration: 200, easing: 'ease-out' },
)

type Done = (cancelled: boolean) => void
type StyleSnapshot = { width: string; height: string; marginLeft: string }
const styleSnapshots = new WeakMap<HTMLElement, StyleSnapshot>()
const animationsMap = new WeakMap<HTMLElement, Animation>()
let leavingElement: HTMLElement | undefined = undefined

function onEnter(e: Element, done: Done) {
  if (e instanceof HTMLElement) animRun(e, done, true)
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
    {
      const leaveCbKey = Object.getOwnPropertySymbols(e).find((sym) =>
        sym.toString().includes('_leaveCb'),
      )
      const el = leaveCbKey && (e as HTMLElement & Record<typeof leaveCbKey, Function>)
      if (el && typeof el[leaveCbKey] === 'function') {
        const originalHook = el[leaveCbKey]!
        el[leaveCbKey] = function (...args: any[]) {
          snapshotCurrentStyles(e)
          originalHook.apply(this, args)
        }
      }
    }

    animRun(e, done, false)
  }
}

function onAfterLeave(e: Element) {
  nextTick(() => {
    if (leavingElement === e) leavingElement = undefined
  })
}

function snapshotCurrentStyles(e: HTMLElement) {
  const { width, height, marginLeft } = getComputedStyle(e)
  styleSnapshots.set(e, { width, height, marginLeft })
}

function animRun(e: HTMLElement, done: Done, isEnter: boolean) {
  const lastSnapshot =
    styleSnapshots.get(e) ?? (leavingElement ? styleSnapshots.get(leavingElement) : undefined)

  // If there already is an animation running, stop it before reading final state styles.
  const prevAnimation = animationsMap.get(e)
  if (prevAnimation && !prevAnimation.finished) {
    prevAnimation.cancel()
  }
  const current = getComputedStyle(e)

  const start: Keyframe = { composite: 'replace', easing: props.easing }
  const end: Keyframe = { composite: 'replace', easing: props.easing }

  if (props.width) {
    start.width = lastSnapshot?.width || '0px'
    end.width = isEnter ? current.width : '0px'
    start.overflowX = end.overflowX = 'clip'
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
  animation.addEventListener('finish', () => done(false))
  animation.addEventListener('cancel', () => done(true))
  animation.play()
  animationsMap.set(e, animation)
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
