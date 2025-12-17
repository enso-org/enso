import type { ToValue } from '$/utils/reactivity'
import { createContextStore } from '@/providers'
import type { Opt } from '@/util/data/opt'
import { toRaw, toValue } from 'vue'

/** If the element is the first child of another element, return the parent. */
function primogenitureParent(el: Opt<Element>): Element | undefined {
  if (el == null) return undefined
  return el.parentElement?.firstElementChild === el ? el.parentElement : undefined
}

export const [provideTopLevelArgument, useTopLevelArgument] = createContextStore(
  'Top-level argument',
  (topLevelArgumentElement: ToValue<HTMLElement | null>) => {
    /**
     * If the element is the recursively-first-child of a WidgetTopLevelArgument, return the root
     * element of the top-level argument. This can be used for such purposes as positioning
     * dropdowns.
     */
    function enclosingTopLevelArgument(
      element: Opt<Element>,
      rootElement: Opt<Element>,
    ): HTMLElement | undefined {
      const tla = toRaw(toValue(topLevelArgumentElement))
      const stop = toRaw(rootElement)
      let el: Opt<Element> = toRaw(element)
      while (el != null && el !== stop) {
        if (el === tla) return tla
        el = primogenitureParent(el)
      }
      return undefined
    }
    return { enclosingTopLevelArgument }
  },
)
