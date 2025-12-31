import type { Opt } from '@/util/data/opt'
import {
  autoUpdate,
  offset,
  shift,
  size,
  useFloating,
  type OffsetOptions,
  type SizeOptions,
} from '@floating-ui/vue'
import { computed, type Ref } from 'vue'

// How much wider a dropdown can be than a port it is attached to, when a long text is present.
// Any text beyond that limit will receive an ellipsis and sliding animation on hover.
const MAX_DROPDOWN_OVERSIZE_PX = 390

/** Limit the width of the dropdown to the width of the port. */
function sizeOptions(limitWidth: boolean): () => SizeOptions {
  return () => ({
    elementContext: 'reference',
    apply({ elements, rects, availableWidth }) {
      const PORT_PADDING_X = 8
      const screenOverflow = Math.max(
        (rects.floating.width - availableWidth) / 2 + PORT_PADDING_X,
        0,
      )
      const portWidth = rects.reference.width + PORT_PADDING_X * 2

      const minWidth = `${Math.max(portWidth - screenOverflow, 0)}px`
      const maxWidth = limitWidth ? `${MAX_DROPDOWN_OVERSIZE_PX}px` : null

      // Delay changing styles to avoid "ResizeObserver loop completed with undelivered notifications" error.
      requestAnimationFrame(() => {
        Object.assign(elements.floating.style, { minWidth, maxWidth })
        elements.floating.style.setProperty('--dropdown-max-width', maxWidth)
      })
    },
  })
}

const TOP_MENU_PADDING = 6
const SUBMENU_PADDING = 10

/** Offset the dropdown below the port or by SUBMENU_PADDING pixels. */
function offsetSubmenu(isTopLevel: boolean): OffsetOptions {
  return {
    mainAxis: isTopLevel ? TOP_MENU_PADDING : SUBMENU_PADDING,
  }
}

/** Rules for positioning the dropdown. */
function middleware(isTopLevel: boolean, limitWidth: boolean, rootElement?: Ref<Opt<HTMLElement>>) {
  return computed(() => [
    offset(offsetSubmenu(isTopLevel)),
    size(sizeOptions(limitWidth)),
    // Try to keep the dropdown within node's bounds.
    shift(() => (rootElement?.value && isTopLevel ? { boundary: rootElement.value } : {})),
    shift(), // Always keep within screen bounds, overriding node bounds.
  ])
}

/** Positioning for activity dropdowns. */
export function activityDropdownStyles(
  floatReference: Ref<Opt<HTMLElement>>,
  dropdownElement: Ref<Opt<HTMLElement>>,
  rootElement: Ref<Opt<HTMLElement>>,
) {
  return useFloating(floatReference, dropdownElement, {
    placement: 'bottom-start',
    middleware: middleware(true, false, rootElement),
    whileElementsMounted: autoUpdate,
  })
}

/** Positioning for dropdown submenus. */
export function submenuDropdownStyles(
  floatReference: Ref<Opt<HTMLElement>>,
  dropdownElement: Ref<Opt<HTMLElement>>,
  isTopLevel: boolean,
  rootElement?: Ref<Opt<HTMLElement>>,
) {
  return useFloating(floatReference, dropdownElement, {
    placement: isTopLevel ? 'bottom-start' : 'right',
    middleware: middleware(isTopLevel, true, rootElement),
    whileElementsMounted: autoUpdate,
  })
}
