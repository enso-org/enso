import { isMacLike, useEvent } from '@/composables/events'
import { useGlobalEventRegistry, type GlobalEventRegistry } from '@/providers/globalEventRegistry'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { ref, toRef, watch, type Ref } from 'vue'

/** Keyboard modifier state API */
export interface KeyboardComposable {
  readonly alt: boolean
  readonly shift: boolean
  readonly mod: boolean
  /** The control key; see also the platform-specific modifier key {@link mod}. */
  readonly ctrl: boolean
  /**
   * Update the known state of modifier keys using the information in the given event. This can be
   * used in an event handler to ensure the state is accurate; currently we miss transitions in some
   * cases when they occur while the window is not focused.
   */
  readonly updateState: (e: MouseEvent | KeyboardEvent) => void
}

/** Composable containing reactive flags for modifier's press state. */
export function useGlobalKeyboard(
  globalEventRegistry = useGlobalEventRegistry(),
): KeyboardComposable {
  const { state, updateState, resetState } = useKeyboardState()
  const { globalEventRegistryPre } = globalEventRegistry

  useEvent(globalEventRegistryPre, 'keydown', updateState, { capture: true })
  useEvent(globalEventRegistryPre, 'keyup', updateState, { capture: true })
  useEvent(globalEventRegistryPre, 'blur', resetState, { capture: true })

  return useKeyboardApi(state, updateState)
}

function useEventListener<T extends keyof HTMLElementEventMap>(
  element: HTMLElement,
  event: T,
  handler: (ev: HTMLElementEventMap[T]) => void,
  onCleanup: (callback: () => void) => void,
) {
  element.addEventListener(event, handler)
  onCleanup(() => element.removeEventListener(event, handler))
}

/**
 * Composable containing reactive flags for modifier's press state, considering only keys pressed
 * while focus was within a certain element.
 */
export function useLocalKeyboard(
  globalEventRegistry: GlobalEventRegistry,
  element: ToValue<Opt<HTMLElement>>,
): KeyboardComposable {
  const { state, updateState, resetState } = useKeyboardState()
  const { globalEventRegistryPre } = globalEventRegistry

  watch(
    toRef(element),
    (element, _prev, onCleanup) => {
      resetState()
      if (element) {
        useEventListener(element, 'keydown', updateState, onCleanup)
        useEventListener(element, 'focusout', resetState, onCleanup)
      }
    },
    { immediate: true },
  )
  useEvent(globalEventRegistryPre, 'keyup', updateState, { capture: true })

  return useKeyboardApi(state, updateState)
}

type ModifierState = Record<'alt' | 'shift' | 'ctrl' | 'meta', Ref<boolean>>
type UpdateState = (e: MouseEvent | KeyboardEvent) => void

function useKeyboardState() {
  const state: ModifierState = {
    alt: ref(false),
    shift: ref(false),
    meta: ref(false),
    ctrl: ref(false),
  }
  return {
    updateState: (e: MouseEvent | KeyboardEvent) => {
      state.alt.value = e.altKey
      state.shift.value = e.shiftKey
      state.meta.value = e.metaKey
      state.ctrl.value = e.ctrlKey
    },
    resetState: () => {
      state.alt.value = false
      state.shift.value = false
      state.meta.value = false
      state.ctrl.value = false
    },
    state,
  }
}

function useKeyboardApi({ alt, shift, ctrl, meta }: ModifierState, updateState: UpdateState) {
  return proxyRefs({
    alt,
    shift,
    ctrl,
    mod: isMacLike ? meta : ctrl,
    updateState,
  })
}
