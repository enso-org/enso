import { onlineManager } from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import { onScopeDispose, ref } from 'vue'

function createOnlineState() {
  const isOnline = ref(onlineManager.isOnline())
  const unsub = onlineManager.subscribe((value) => (isOnline.value = value))
  onScopeDispose(unsub)
  return isOnline
}

/** Return "is online" boolean ref. */
export const useIsOnline = createGlobalState(createOnlineState)
