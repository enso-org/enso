import { useEvent } from '@/util/events'
import type { Ref } from 'vue'

export function useAutoBlur(root: Ref<HTMLElement | SVGElement | MathMLElement | undefined>) {
  useEvent(
    window,
    'pointerdown',
    (event) => {
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
    },
    { capture: true },
  )
}
