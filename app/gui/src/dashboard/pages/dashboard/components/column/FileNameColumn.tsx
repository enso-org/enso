/** @file The icon and name of a {@link FileAsset}. */
import EditableSpan from '#/components/EditableSpan'
import { Icon } from '#/components/Icon'
import { useRenameAsset } from '#/hooks/backendHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import type { AssetNameColumnProps } from '#/pages/dashboard/components/column'
import { useDriveStore } from '#/providers/DriveProvider'
import { fileIcon } from '#/utilities/fileIcon'
import { useStore } from '#/utilities/zustand'
import { titleSchema, type FileAsset } from 'enso-common/src/services/Backend'

/** Props for a {@link FileNameColumn}. */
export interface FileNameColumnProps extends AssetNameColumnProps {
  readonly item: FileAsset
}

/**
 * The icon and name of a {@link FileAsset}.
 * @throws {Error} when the asset is not a {@link FileAsset}.
 * This should never happen.
 */
export default function FileNameColumn(props: FileNameColumnProps) {
  const { item, isEditable } = props

  const { associatedBackend: backend } = useCategoriesAPI()
  const getAssetChildren = useGetAssetChildren()
  const driveStore = useDriveStore()
  const renameAsset = useRenameAsset(backend)

  const isEditingName = useStore(driveStore, ({ assetToRename }) => assetToRename === item.id)
  const setIsEditing = (isEditing: boolean) => {
    if (isEditing) {
      if (isEditable) {
        driveStore.setState({ assetToRename: item.id })
      }
    } else {
      driveStore.setState({ assetToRename: null })
    }
  }

  const doRename = async (newTitle: string) => {
    await renameAsset(item.id, newTitle)
    setIsEditing(false)
  }

  return (
    <div
      className="flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
    >
      <Icon icon={fileIcon(item.title)} className="m-name-column-icon" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={isEditingName}
        className="grow bg-transparent font-naming"
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        schema={() =>
          titleSchema({
            id: item.id,
            siblings: getAssetChildren(item.parentId),
          })
        }
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
