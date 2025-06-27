/** @file The icon and name of a {@link FileAsset}. */
import EditableSpan from '#/components/EditableSpan'
import { Icon } from '#/components/Icon'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import { useIsEditingName, useSetEditingNameAssetId } from '#/providers/DriveProvider'
import { titleSchema, type FileAsset } from '#/services/Backend'
import { fileIcon } from '#/utilities/fileIcon'
import { useMutationCallback } from '#/utilities/tanstackQuery'

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
  const { item, state, isEditable } = props
  const { backend } = state

  const isEditing = useIsEditingName(item.id) && isEditable
  const setEditingNameAssetId = useSetEditingNameAssetId()
  const getAssetChildren = useGetAssetChildren()

  const updateAsset = useMutationCallback(
    backendMutationOptions(backend, 'updateAsset', {
      onSuccess: () => {
        setEditingNameAssetId(null)
      },
    }),
  )

  return (
    <div
      className="flex h-table-row items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (isEditing && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
    >
      <Icon icon={fileIcon(item.title)} className="m-name-column-icon" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={isEditing}
        schema={() =>
          titleSchema({
            asset: item,
            siblings: getAssetChildren(item.parentId),
          })
        }
        className="grow bg-transparent font-naming"
        onSubmit={(title) => updateAsset([item.id, { title }, item.title])}
        onCancel={() => {
          setEditingNameAssetId(null)
        }}
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
