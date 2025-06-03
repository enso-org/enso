import LocalStorage from '#/utilities/LocalStorage'
import { createGlobalState } from '@vueuse/core'

export const useLocalStorage = createGlobalState(() => LocalStorage.getInstance())
