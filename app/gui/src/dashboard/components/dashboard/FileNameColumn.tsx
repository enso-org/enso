/** @file The icon and name of a {@link FileAsset}. */
import type { AssetColumnProps } from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import { titleSchema, type FileAsset } from '#/services/Backend'
import { fileIcon } from '#/utilities/fileIcon'
import { merger } from '#/utilities/object'

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
      <SvgMask src={fileIcon()} className="m-name-column-icon size-4" />
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
