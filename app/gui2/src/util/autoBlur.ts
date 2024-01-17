import { useEvent } from '@/composables/events'
import type { Ref } from 'vue'

export function useAutoBlur(root: Ref<HTMLElement | SVGElement | MathMLElement | undefined>) {
  useEvent(
    window,
    'pointerdown',
    (event) => blurIfNecessary(root, event),
    { capture: true },
  )
}

export function blurIfNecessary(root: Ref<HTMLElement | SVGElement | MathMLElement | undefined>, event: MouseEvent) {
  if (
    !root.value?.contains(document.activeElement) ||
    !(event.target instanceof Element) ||
    root.value.contains(event.target)
  )
    return
  if (
    !(document.activeElement instanceof HTMLElement) &&
    !(document.activeElement instanceof SVGElement) &&
    !(document.activeElement instanceof MathMLElement)
  )
    return
  document.activeElement.blur()
}
