/** @file Provider for categories. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline } from '#/hooks/offlineHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { useBackend, useLocalBackend } from '#/providers/BackendProvider'
import type { ReactNode } from 'react'
import { useCategories } from './categoriesHooks'
import type { Category, CategoryId } from './Category'
import { CategoriesContext, type CategoriesContextValue } from './constants'

/** Props for the {@link CategoriesProvider}. */
export interface CategoriesProviderProps {
  readonly children: ReactNode | ((contextValue: CategoriesContextValue) => ReactNode)
  readonly onCategoryChange?: (previousCategory: Category | null, newCategory: Category) => void
}

/** Provider for categories. */
export function CategoriesProvider(props: CategoriesProviderProps): React.JSX.Element {
  const { children, onCategoryChange = () => {} } = props

  const { cloudCategories, localCategories, findCategoryById } = useCategories()
  const localBackend = useLocalBackend()
  const { isOffline } = useOffline()

  const [categoryId, privateSetCategoryId, resetCategoryId] = useSearchParamsState<CategoryId>(
    'driveCategory',
    () => {
      if (isOffline && localBackend != null) {
        return 'local'
      }

      return localBackend != null ? 'local' : 'cloud'
    },
    // This is safe, because we enshure the type inside the function
    // eslint-disable-next-line no-restricted-syntax
    (value): value is CategoryId => findCategoryById(value as CategoryId) != null,
  )

  const setCategoryId = useEventCallback((nextCategoryId: CategoryId) => {
    const previousCategory = findCategoryById(categoryId)
    privateSetCategoryId(nextCategoryId)

    // This is safe, because we know that the result will have the correct type.
    // eslint-disable-next-line no-restricted-syntax
    onCategoryChange(previousCategory, findCategoryById(nextCategoryId) as Category)
  })

  const category = findCategoryById(categoryId)

  // This is safe, because a category always specified
  // eslint-disable-next-line no-restricted-syntax
  const backend = useBackend(category as Category)

  // This usually doesn't happen but if so,
  // We reset the category to the default.
  if (category == null) {
    resetCategoryId(true)
    return <></>
  }

  const contextValue = {
    cloudCategories,
    localCategories,
    category,
    setCategory: setCategoryId,
    resetCategory: resetCategoryId,
    associatedBackend: backend,
  } satisfies CategoriesContextValue

  return (
    <CategoriesContext.Provider value={contextValue}>
      {typeof children === 'function' ? children(contextValue) : children}
    </CategoriesContext.Provider>
  )
}
