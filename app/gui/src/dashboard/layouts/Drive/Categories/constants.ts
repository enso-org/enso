/** @file Constants for `Categories`. */
import type Backend from '#/services/Backend'
import { createContext } from 'react'
import type { CloudCategoryResult, LocalCategoryResult } from './categoriesHooks'
import type { Category, CategoryId } from './Category'

/** Context value for the categories. */
export interface CategoriesContextValue {
  readonly cloudCategories: CloudCategoryResult
  readonly localCategories: LocalCategoryResult
  readonly category: Category
  readonly setCategory: (category: CategoryId) => void
  readonly resetCategory: () => void
  readonly associatedBackend: Backend
}

export const CategoriesContext = createContext<CategoriesContextValue | null>(null)
