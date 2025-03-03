/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */

import type { Category } from '#/layouts/CategorySwitcher/Category'
import type Backend from '#/services/Backend'
import type AssetQuery from '#/utilities/AssetQuery'
import { useEffect, useState } from 'react'

import { useErrorBoundary } from '#/components/ErrorBoundary'
import { listDirectoryQueryOptions } from '#/hooks/backendHooks'
import type { DirectoryId } from '#/services/Backend'
import { useQueryClient, useSuspenseQuery } from '@tanstack/react-query'
import { DriveBarNavigation } from './DriveBarNavigation'
import { DriveBarToolbar } from './DriveBarToolbar'

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly backend: Backend
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly category: Category
  readonly rootDirectoryId: DirectoryId
}

const CATEGORIES_TO_DISPLAY_START_MODAL = ['cloud', 'local', 'local-directory']

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBar(props: DriveBarProps) {
  const { backend, query, setQuery, category, rootDirectoryId } = props

  const [shouldForceHideStartModal, setShouldForceHideStartModal] = useState(false)
  const { showBoundary } = useErrorBoundary()

  const queryClient = useQueryClient()
  const rootDirectoryQuery = listDirectoryQueryOptions({
    backend,
    category,
    parentId: rootDirectoryId,
  })

  const {
    data: isEmpty,
    error,
    isFetching,
  } = useSuspenseQuery({
    ...rootDirectoryQuery,
    select: (data) => data.length === 0,
  })

  // Show the error boundary if the query failed, but has data.
  if (error != null && !isFetching) {
    showBoundary(error)
    // Remove the query from the cache.
    // This will force the query to be refetched when the user navigates again.
    queryClient.removeQueries({ queryKey: rootDirectoryQuery.queryKey })
  }

  // When the directory is no longer empty, we need to hide the start modal.
  // This includes the cases when the directory wasn't empty before, but it's now empty
  // (e.g. when the user deletes the last asset).
  useEffect(() => {
    if (!isEmpty) {
      setShouldForceHideStartModal(true)
    }
  }, [isEmpty])

  // When the root directory changes, we need to show the start modal
  // if the directory is empty.
  useEffect(() => {
    setShouldForceHideStartModal(false)
  }, [category.type])

  const shouldDisplayStartModal =
    isEmpty &&
    CATEGORIES_TO_DISPLAY_START_MODAL.includes(category.type) &&
    !shouldForceHideStartModal

  return (
    <div className="flex flex-col gap-2">
      <DriveBarNavigation />

      <DriveBarToolbar
        backend={backend}
        query={query}
        setQuery={setQuery}
        category={category}
        isEmpty={isEmpty}
        shouldDisplayStartModal={shouldDisplayStartModal}
      />
    </div>
  )
}
