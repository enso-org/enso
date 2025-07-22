/** @file The icon and name of a {@link DirectoryAsset}. */
import { Button } from '#/components/Button'
import EditableSpan from '#/components/EditableSpan'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import { setDriveLocation, useDriveStore } from '#/providers/DriveProvider'
import { titleSchema, type DirectoryAsset } from '#/services/Backend'
import { merger } from '#/utilities/object'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import { useTransition } from 'react'

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps extends AssetColumnProps {
  readonly item: DirectoryAsset
}

/**
 * The icon and name of a {@link DirectoryAsset}.
 * @throws {Error} when the asset is not a {@link DirectoryAsset}.
 * This should never happen.
 */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
  const { item, rowState, setRowState, isEditable, isNavigating, renameAsset } = props
  const [isLoading, startNavigation] = useTransition()

  const { getText } = useText()
  const driveStore = useDriveStore()
  const getAssetChildren = useGetAssetChildren()

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(merger({ isEditingName }))
    }

    if (!isEditingName) {
      driveStore.setState({ newestFolderId: null })
    }
  }

  const doRename = async (newTitle: string) => {
    await renameAsset(item.id, newTitle)
    setIsEditing(false)
  }

  return (
    <div
      className="group flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
    >
      <Button
        icon="folder"
        variant="icon"
        isLoading={isLoading || isNavigating}
        aria-label={getText('open')}
        tooltipPlacement="left"
        testId="directory-row-navigate-button"
        className="mx-1 transition-transform duration-arrow"
        onPress={() => {
          startNavigation(() => {
            setDriveLocation(item.id)
          })
        }}
      />

      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={twMerge(
          'cursor-pointer bg-transparent font-naming',
          rowState.isEditingName ? 'cursor-text' : 'cursor-pointer',
        )}
        schema={() =>
          titleSchema({
            asset: item,
            siblings: getAssetChildren(item.parentId),
          })
        }
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
