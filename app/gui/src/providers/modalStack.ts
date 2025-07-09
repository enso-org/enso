import { createGlobalState } from '@vueuse/core'
import type { JSX } from 'react'
import { computed, ref } from 'vue'

function createModalStack() {
  const modals = ref<{ readonly id: string; readonly modal: JSX.Element }[]>([])

  function isModalOpen(id: string) {
    return computed(() => modals.value.some((modal) => modal.id === id))
  }

  function pushModal(id: string, modal: JSX.Element) {
    modals.value.push({ id, modal })
  }

  function popModal(id: string) {
    const modalsValue = modals.value
    const index = modalsValue.findIndex((modal) => modal.id)
    if (index === -1) {
      console.warn(`ModalStack: Modal with id '${id}' not found`)
      return
    }
    const modalsLeft = modalsValue.length - index
    if (modalsLeft > 1) {
      console.warn(
        `ModalStack: There are ${modalsLeft - 1} modals after the modal with id '${id}' which will be discarded`,
      )
    }
    modalsValue.splice(index, modalsValue.length)
  }

  return {
    modals,
    isModalOpen,
    pushModal,
    popModal,
  }
}

/** A React provider for the modal stack. */
export const useModalStack = createGlobalState(createModalStack)
