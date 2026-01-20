import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'
import type { Ref } from 'vue'

/**
 * Context store that allows a component to designate an element that is a suitable DOM parent for
 * popovers and dropdowns.
 * The popover root must be in a suitable stacking context. It may also set CSS variables. It may be
 * used as a reference element when computing float positions.
 */
export const [providePopoverRoot, usePopoverRoot] = createContextStore(
  'popoverRoot',
  identity<Readonly<Ref<HTMLElement | undefined | null>>>,
)
