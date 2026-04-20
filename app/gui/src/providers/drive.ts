import LocalStorage from '#/utilities/LocalStorage'
import { proxyRefs } from '$/utils/reactivity'
import { createContextStore } from '@/providers'
import { isDirectoryId, type DirectoryId } from 'enso-common/src/services/Backend'
import { computed, watch } from 'vue'
import * as z from 'zod'
import { useBackends } from './backends'
import {
  CATEGORY_BACKEND,
  categoryEq,
  categoryFromKey,
  categoryKey,
  useCategories,
  type Category,
} from './category'

const DRIVE_DISPLAY_SCHEMA = z
  .object({
    currentDirectoryId: z
      .custom<DirectoryId>((value) =>
        typeof value === 'string' && isDirectoryId(value) ? value : false,
      )
      .nullable(),
    currentCategory: z.string(),
  })
  .nullable()
declare module '#/utilities/LocalStorage' {
  interface LocalStorageData {
    readonly driveDisplay: z.infer<typeof DRIVE_DISPLAY_SCHEMA>
  }
}
LocalStorage.registerKey('driveDisplay', { schema: DRIVE_DISPLAY_SCHEMA, isUserSpecific: true })

export type DriveLocationStore = ReturnType<typeof useDriveLocation>

/** */
export const [provideDriveLocation, useDriveLocation] = createContextStore(
  'drive',
  (startReactTransition: (action: () => void) => void) => {
    const backends = useBackends()
    const categories = useCategories()
    const localStorage = LocalStorage.getInstance()
    const storedDriveDisplay = computed(() => localStorage.get('driveDisplay'))

    const defaultCategory = computed<Category>(() =>
      backends.localBackend != null ? { type: 'local' } : { type: 'cloud' },
    )

    const currentCategory = computed({
      get: () =>
        categoryFromKey(storedDriveDisplay.value?.currentCategory) ?? defaultCategory.value,
      set: (category) =>
        startReactTransition(() =>
          localStorage.set('driveDisplay', {
            currentCategory: categoryKey(category),
            currentDirectoryId: null,
          }),
        ),
    })

    const currentDirectory = computed({
      get: () => storedDriveDisplay.value?.currentDirectoryId ?? null,
      set: (dir) =>
        startReactTransition(() =>
          localStorage.set('driveDisplay', {
            currentCategory: categoryKey(currentCategory.value),
            currentDirectoryId: dir,
          }),
        ),
    })

    const associatedBackend = computed(() =>
      backends.backendForType(CATEGORY_BACKEND[currentCategory.value.type]),
    )

    function setDefaultCategory() {
      localStorage.set('driveDisplay', null)
    }

    watch(
      () => [...categories.localCategoriesList, ...categories.cloudCategoriesList],
      (newList) => {
        if (!newList.find((category) => categoryEq(category, currentCategory.value))) {
          setDefaultCategory()
        }
      },
    )

    return proxyRefs({
      currentCategory,
      currentDirectory,
      associatedBackend,
      setDefaultCategory,
    })
  },
)
