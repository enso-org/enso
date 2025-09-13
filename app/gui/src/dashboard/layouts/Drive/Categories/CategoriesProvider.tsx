/** @file Provider for categories. */
import { setDriveLocation, useCategoryId } from '#/providers/DriveProvider'
import { useBackends } from '$/providers/react'
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

  const categoryId = useCategoryId() ?? (localBackend != null ? 'local' : 'cloud')
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
