import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import type { ComputedRef } from 'vue'

/** Custom items for WidgetSelection which will be added at the top of the list.
 * They can’t be selected permanently, but they can be clicked.
 */
interface CustomDropdownItems {
  /** Labels for items. */
  items: (string | ComputedRef<string>)[]
  /** Click handler that receives the index of clicked item. It will not trigger for clicks on non-custom items. */
  onClick: (idx: number) => void
}

export { injectFn as injectCustomDropdownItems, provideFn as provideCustomDropdownItems }
const { provideFn, injectFn } = createContextStore('Function info', identity<CustomDropdownItems>)
