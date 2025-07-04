/** @file Provider for categories. */
import { useOffline } from '#/hooks/offlineHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { driveLocationStore, getDriveLocation, setDriveLocation } from '#/providers/DriveProvider'
import { useBackends, useFullUserSession } from '$/providers/react'
import { useEffect } from 'react'
import { isCloudCategory, type CategoryId } from './Category'
import { CategoriesContext, useCategories, type CategoriesContextValue } from './categoriesHooks'

/** Props for the {@link CategoriesProvider}. */
export interface CategoriesProviderProps extends React.PropsWithChildren {}

/** Provider for categories. */
export function CategoriesProvider(props: CategoriesProviderProps) {
  const { children } = props

  const { cloudCategories, localCategories, findCategoryById } = useCategories()
  const { backendForType, localBackend } = useBackends()
  const { user } = useFullUserSession()
  const { isOffline } = useOffline()

  const [categoryId, privateSetCategoryId, privateResetCategoryId] =
    useSearchParamsState<CategoryId>(
      'driveCategory',
      () => {
        const readSavedCategory = () => {
          const id = getDriveLocation().categoryId
          if (id == null) return null
          const category = findCategoryById(id)
          if (category == null) return null
          const unavailable = (!user.isEnabled || isOffline) && isCloudCategory(category)
          if (unavailable) return null
          return id
        }

        return readSavedCategory() ?? (localBackend != null ? 'local' : 'cloud')
      },
      // This is safe, because we confirm the type inside the function.
      // eslint-disable-next-line no-restricted-syntax
      (value): value is CategoryId => findCategoryById(value as CategoryId) != null,
    )

  const category = findCategoryById(categoryId)

  useEffect(
    () =>
      driveLocationStore.subscribe((state, oldState) => {
        if (state.categoryId !== oldState.categoryId) {
          if (state.categoryId != null) {
            privateSetCategoryId(state.categoryId)
          } else {
            privateResetCategoryId()
          }
        }
      }),
    [privateResetCategoryId, privateSetCategoryId],
  )

  // This usually doesn't happen but if so,
  // We reset the category to the default.
  if (category == null) {
    setDriveLocation(null, null)
    return null
  }

  const backend = backendForType(category.backend)

  const contextValue = {
    cloudCategories,
    localCategories,
    category,
    associatedBackend: backend,
  } satisfies CategoriesContextValue

  return <CategoriesContext.Provider value={contextValue}>{children}</CategoriesContext.Provider>
}
