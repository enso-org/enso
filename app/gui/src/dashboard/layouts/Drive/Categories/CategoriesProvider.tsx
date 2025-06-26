/**
 * @file
 *
 * Provider for the categories.
 */

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline } from '#/hooks/offlineHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { useBackends, useFullUserSession } from '$/providers/react'
import type { ReactNode } from 'react'
import { isCloudCategory, type Category, type CategoryId } from './Category'
import {
  CategoriesContext,
  categoryIdStore,
  useCategories,
  type CategoriesContextValue,
} from './categoriesHooks'

/**
 * Props for the {@link CategoriesProvider}.
 */
export interface CategoriesProviderProps {
  readonly children: ReactNode | ((contextValue: CategoriesContextValue) => ReactNode)
  readonly onCategoryChange?: (previousCategory: Category | null, newCategory: Category) => void
}

/**
 * Provider for the categories.
 */
export function CategoriesProvider(props: CategoriesProviderProps): React.JSX.Element {
  const { children, onCategoryChange = () => {} } = props

  const { cloudCategories, localCategories, findCategoryById } = useCategories()
  const { backendForType, localBackend } = useBackends()
  const { user } = useFullUserSession()
  const { isOffline } = useOffline()

  const [categoryId, privateSetCategoryId, privateResetCategoryId] =
    useSearchParamsState<CategoryId>(
      'driveCategory',
      () => {
        const readSavedCategory = () => {
          const id = categoryIdStore.getState().categoryId
          if (id == null) return null
          const category = findCategoryById(id)
          if (category == null) return null
          const unavailable = (!user.isEnabled || isOffline) && isCloudCategory(category)
          if (unavailable) return null
          return id
        }

        return readSavedCategory() ?? (localBackend != null ? 'local' : 'cloud')
      },
      // This is safe, because we enshure the type inside the function
      // eslint-disable-next-line no-restricted-syntax
      (value): value is CategoryId => findCategoryById(value as CategoryId) != null,
    )

  const setCategoryId = useEventCallback((nextCategoryId: CategoryId) => {
    const previousCategory = findCategoryById(categoryId)

    if (categoryId === nextCategoryId) {
      return
    }

    privateSetCategoryId(nextCategoryId)
    categoryIdStore.setState({
      categoryId: nextCategoryId,
    })

    // This is safe, because we know that the result will have the correct type.
    // eslint-disable-next-line no-restricted-syntax
    onCategoryChange(previousCategory, findCategoryById(nextCategoryId) as Category)
  })

  const resetCategoryId = useEventCallback((replace?: boolean) => {
    privateResetCategoryId(replace)
    categoryIdStore.setState({
      categoryId: null,
    })
  })

  const category = findCategoryById(categoryId)

  // This usually doesn't happen but if so,
  // We reset the category to the default.
  if (category == null) {
    resetCategoryId(true)
    return <></>
  }

  const backend = backendForType(category.backend)

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
