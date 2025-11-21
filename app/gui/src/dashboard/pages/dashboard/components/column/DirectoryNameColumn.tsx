/** @file The icon and name of a {@link DirectoryAsset}. */
import { Button } from '#/components/Button'
import EditableSpan from '#/components/EditableSpan'
import { useRenameAsset } from '#/hooks/backendHooks'
import { useStore } from '#/hooks/storeHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import type { AssetNameColumnProps } from '#/pages/dashboard/components/column'
import { setDriveLocation, useDriveStore } from '#/providers/DriveProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import { titleSchema, type DirectoryAsset } from 'enso-common/src/services/Backend'
import { useTransition } from 'react'

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps extends AssetNameColumnProps {
  readonly item: DirectoryAsset
}

/**
 * The icon and name of a {@link DirectoryAsset}.
 * @throws {Error} when the asset is not a {@link DirectoryAsset}.
 * This should never happen.
 */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
  const { item, isEditable, isNavigating } = props

  const { associatedBackend: backend } = useCategoriesAPI()
  const [isLoading, startNavigation] = useTransition()
  const { getText } = useText()
  const driveStore = useDriveStore()
  const getAssetChildren = useGetAssetChildren()
  const renameAsset = useRenameAsset(backend)

  const isEditingName = useStore(driveStore, ({ assetToRename }) => assetToRename === item.id)
  const setIsEditing = (isEditing: boolean) => {
    if (isEditable && isEditing) {
      driveStore.setState({ assetToRename: item.id })
    }
    if (!isEditing) {
      driveStore.setState({ assetToRename: null })
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
        if (isEditingName && event.key === 'Enter') {
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
        editable={isEditingName}
        className={twMerge(
          'cursor-pointer bg-transparent font-naming',
          isEditingName ? 'cursor-text' : 'cursor-pointer',
        )}
        schema={() =>
          titleSchema({
            id: item.id,
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
