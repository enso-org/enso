/** @file The icon and name of a {@link DirectoryAsset}. */
import FolderIcon from '#/assets/folder.svg'
import { Button } from '#/components/AriaComponents'
import type { AssetColumnProps } from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import { useDriveStore, useSetCurrentDirectoryId } from '#/providers/DriveProvider'
import { useText } from '#/providers/TextProvider'
import { isNewTitleUnique, type DirectoryAsset } from '#/services/Backend'
import { merger } from '#/utilities/object'
import { twMerge } from '#/utilities/tailwindMerge'
import { isDirectoryNameContainInvalidCharacters } from '#/utilities/validation'
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
  const setCurrentDirectoryId = useSetCurrentDirectoryId()
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
        icon={FolderIcon}
        size="medium"
        variant="icon"
        loading={isLoading || isNavigating}
        aria-label={getText('open')}
        tooltipPlacement="left"
        testId="directory-row-navigate-button"
        className="mx-1 transition-transform duration-arrow"
        onPress={() => {
          startNavigation(() => {
            setCurrentDirectoryId({ current: item.id, parent: item.parentId })
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
        schema={(z) =>
          z
            .refine((value) => !isDirectoryNameContainInvalidCharacters(value), {
              message: getText('nameShouldNotContainInvalidCharacters'),
            })
            .refine((value) => isNewTitleUnique(item, value, getAssetChildren(item.parentId)), {
              message: getText('nameShouldBeUnique'),
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
