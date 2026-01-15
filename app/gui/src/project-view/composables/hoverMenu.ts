import { computed, onUnmounted, ref, type Ref } from 'vue'

const HOVER_OPEN_DELAY_MS = 200
const HOVER_CLOSE_DELAY_MS = 150

export interface UseHoverMenuOptions {
  /** Delay in milliseconds before opening menu on hover. Default: 200ms */
  openDelay?: number
  /** Delay in milliseconds before closing menu on hover out. Default: 150ms */
  closeDelay?: number
}

/**
 * Composable for managing a menu that opens/closes on hover with configurable delays.
 *
 * This handles the complex state management needed for hover menus:
 * - Tracks whether the menu was opened by hover vs. explicit action
 * - Prevents premature closing when hovering between trigger and menu
 * - Cleans up timeouts on unmount
 *
 * @example
 * ```ts
 * const { menuOpen, menuOpenModel, handleMenuEnter, handleMenuLeave } = useHoverMenu()
 *
 * // In template:
 * <DropdownMenu
 *   v-model:open="menuOpenModel"
 *   @pointerenter="handleMenuEnter"
 *   @pointerleave="handleMenuLeave"
 * >
 * ```
 */
export function useHoverMenu(options: UseHoverMenuOptions = {}) {
  const openDelay = options.openDelay ?? HOVER_OPEN_DELAY_MS
  const closeDelay = options.closeDelay ?? HOVER_CLOSE_DELAY_MS

  const menuOpen = ref(false)
  const menuOpenedByHover = ref(false)
  const menuHovering = ref(false)

  let menuOpenTimeout: number | undefined
  let menuCloseTimeout: number | undefined

  function clearMenuOpenTimeout() {
    if (menuOpenTimeout != null) {
      window.clearTimeout(menuOpenTimeout)
      menuOpenTimeout = undefined
    }
  }

  function clearMenuCloseTimeout() {
    if (menuCloseTimeout != null) {
      window.clearTimeout(menuCloseTimeout)
      menuCloseTimeout = undefined
    }
  }

  function scheduleMenuOpen() {
    clearMenuCloseTimeout()
    clearMenuOpenTimeout()
    menuOpenTimeout = window.setTimeout(() => {
      if (!menuHovering.value) return
      menuOpenedByHover.value = true
      menuOpen.value = true
    }, openDelay)
  }

  function scheduleMenuClose() {
    clearMenuOpenTimeout()
    clearMenuCloseTimeout()
    if (!menuOpenedByHover.value) return
    menuCloseTimeout = window.setTimeout(() => {
      if (menuHovering.value) return
      menuOpenedByHover.value = false
      menuOpen.value = false
    }, closeDelay)
  }

  function handleMenuEnter() {
    menuHovering.value = true
    scheduleMenuOpen()
  }

  function handleMenuLeave() {
    menuHovering.value = false
    scheduleMenuClose()
  }

  // Two-way binding model for v-model:open
  // Prevents closing the menu if it was opened by hover and still hovering
  const menuOpenModel = computed({
    get: () => menuOpen.value,
    set: (open) => {
      if (!open && menuOpenedByHover.value && menuHovering.value) return
      menuOpen.value = open
      if (!open) menuOpenedByHover.value = false
    },
  })

  // Clean up timeouts on unmount to prevent memory leaks
  onUnmounted(() => {
    clearMenuOpenTimeout()
    clearMenuCloseTimeout()
  })

  return {
    /** Whether the menu is currently open */
    menuOpen: menuOpen as Ref<boolean>,
    /** Two-way binding model for v-model:open that respects hover state */
    menuOpenModel,
    /** Call this when pointer enters the hover area */
    handleMenuEnter,
    /** Call this when pointer leaves the hover area */
    handleMenuLeave,
  }
}
