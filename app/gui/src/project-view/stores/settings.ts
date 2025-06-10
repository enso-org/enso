import { useSyncLocalStorage } from '@/composables/syncLocalStorage'
import { createContextStore } from '@/providers'
import { ref } from 'vue'

/** Global application settings. */
export interface UserSettings {
  /** Whether to show Help panel when Component Browser is opened or not. */
  showHelpForCB: boolean
}

const defaultUserSettings = {
  showHelpForCB: true,
}

export const [provideSettings, useSettings] = createContextStore('settings', () => {
  const user = ref<UserSettings>(defaultUserSettings)

  useSyncLocalStorage<UserSettings>({
    storageKey: 'enso-user-settings',
    mapKeyEncoder: () => {},
    debounce: 200,
    captureState() {
      return user.value ?? defaultUserSettings
    },
    async restoreState(restored) {
      if (restored) {
        user.value = { ...defaultUserSettings, ...restored }
      }
    },
  })

  return { user }
})
