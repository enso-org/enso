import type { TransferBetweenCategoriesFunction } from '#/layouts/Drive/Categories'
import type { ConfirmDeleteModalProps } from '#/modals/ConfirmDeleteModal'
import { createContextStore } from '@/providers'
import { identity } from '@vueuse/core'

export interface ReactApi {
  startTransition: (action: () => void) => void
  isTransitioning: boolean
  transferBetweenCategories: TransferBetweenCategoriesFunction
  confirmDelete: (properties: ConfirmDeleteModalProps) => void
}

export const [provideReactApi, useReactApi] = createContextStore('reactApi', identity<ReactApi>)
