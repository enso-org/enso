import { isMacLike, useEvent } from '@/composables/events'
import { proxyRefs, ref } from 'vue'

/** {@link useKeyboard} composable object */
export type KeyboardComposable = ReturnType<typeof useKeyboard>
/** Composable containing reactive flags for modifier's press state. */
export function useKeyboard() {
  const state = {
    alt: ref(false),
    shift: ref(false),
    meta: ref(false),
    ctrl: ref(false),
  }

  /**
   * Update the known state of modifier keys using the information in the given event. This can be used in an event
   * handler to ensure the state is accurate; currently we miss transitions in some cases when they occur while the
   * window is not focused.
   */
  const updateState = (e: MouseEvent | KeyboardEvent) => {
    state.alt.value = e.altKey
    state.shift.value = e.shiftKey
    state.meta.value = e.metaKey
    state.ctrl.value = e.ctrlKey
  }
  const resetState = () => {
    state.alt.value = false
    state.shift.value = false
    state.meta.value = false
    state.ctrl.value = false
  }
  useEvent(window, 'keydown', updateState, { capture: true })
  useEvent(window, 'keyup', updateState, { capture: true })
  useEvent(window, 'blur', resetState, { capture: true })

  return proxyRefs({
    alt: state.alt,
    shift: state.shift,
    meta: state.meta,
    ctrl: state.ctrl,
    mod: isMacLike ? state.meta : state.ctrl,
    updateState,
  })
}
