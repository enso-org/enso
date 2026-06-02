/** @file Provider for categories. */
import { setDriveLocation, useCategoryId } from '#/providers/DriveProvider'
import { useBackends, useUserSession } from '$/providers/react'
import { CategoriesContext, useCategories, type CategoriesContextValue } from './categoriesHooks'

/** Props for the {@link CategoriesProvider}. */
export interface CategoriesProviderProps extends React.PropsWithChildren {}

/**
 * Provides the list of categories.
 * See `DriveProvider` for managing the current category state.
 */
export function CategoriesProvider(props: CategoriesProviderProps) {
  const { children } = props

  const { cloudCategories, localCategories, findCategoryById } = useCategories()
  const { backendForType, localBackend } = useBackends()
  const session = useUserSession()

  // In degraded-auth mode the user must land on cloud so the "Enso Cloud is unavailable"
  // stub (rendered by the cloud-category view) is visible after sign-in. Otherwise the
  // local-first default would silently hide the failure.
  const defaultCategoryId =
    session?.isCloudDataUnavailable ? 'cloud'
    : localBackend != null ? 'local'
    : 'cloud'
  const categoryId = useCategoryId() ?? defaultCategoryId
  const category = findCategoryById(categoryId)

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
