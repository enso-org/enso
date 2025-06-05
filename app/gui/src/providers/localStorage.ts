import LocalStorage from '#/utilities/LocalStorage'
import { createGlobalState } from '@vueuse/core'

export const useLocalStorageClass = createGlobalState(() => LocalStorage.getInstance())
