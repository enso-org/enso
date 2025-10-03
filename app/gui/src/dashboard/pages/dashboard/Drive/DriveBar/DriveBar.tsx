/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import type AssetQuery from '#/utilities/AssetQuery'
import { DriveBarNavigation } from './DriveBarNavigation'
import { DriveBarToolbar } from './DriveBarToolbar'

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBar(props: DriveBarProps) {
  return (
    <div className="flex flex-col gap-2">
      <DriveBarNavigation />
      <DriveBarToolbar {...props} />
    </div>
  )
}
