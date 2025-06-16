/** @file The icon and name of a {@link FileAsset}. */
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import { fileIcon } from '#/utilities/fileIcon'
import { merger } from '#/utilities/object'
import { useGetAssetChildren } from '$/data-catalog/assetsTableItemsHooks'
import EditableSpan from '$/react-components/EditableSpan'
import { Icon } from '$/react-components/Icon'
import { titleSchema, type FileAsset } from '$/services/Backend'

/** Props for a {@link FileNameColumn}. */
export interface FileNameColumnProps extends AssetColumnProps {
  readonly item: FileAsset
}

/**
 * The icon and name of a {@link FileAsset}.
 * @throws {Error} when the asset is not a {@link FileAsset}.
 * This should never happen.
 */
export default function FileNameColumn(props: FileNameColumnProps) {
  const { item, rowState, setRowState, isEditable, renameAsset } = props

  const getAssetChildren = useGetAssetChildren()

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(merger({ isEditingName }))
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
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
    >
      <Icon icon={fileIcon(item.title)} className="m-name-column-icon" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className="grow bg-transparent font-naming"
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        schema={() =>
          titleSchema({
            asset: item,
            siblings: getAssetChildren(item.parentId),
          })
        }
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
