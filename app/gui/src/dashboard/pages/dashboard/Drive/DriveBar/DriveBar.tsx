/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */

import type { Category } from '#/layouts/CategorySwitcher/Category'
import type Backend from '#/services/Backend'
import type AssetQuery from '#/utilities/AssetQuery'

import { DriveBarNavigation } from './DriveBarNavigation'
import { DriveBarToolbar } from './DriveBarToolbar'

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly backend: Backend
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly category: Category
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBar(props: DriveBarProps) {
  const { backend, query, setQuery, category } = props

  return (
    <div className="-ml-48 flex flex-col gap-7">
      <DriveBarNavigation />

      <DriveBarToolbar backend={backend} query={query} setQuery={setQuery} category={category} />
    </div>
  )
}
