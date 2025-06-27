/** @file The icon and name of a {@link SecretAsset}. */
import EditableSpan from '#/components/EditableSpan'
import { Icon } from '#/components/Icon'
import { backendMutationOptions } from '#/hooks/backendHooks'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import { useIsEditingName, useSetEditingNameAssetId } from '#/providers/DriveProvider'
import { titleSchema, type DatalinkAsset } from '#/services/Backend'
import { isDoubleClick } from '#/utilities/event'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useRightPanelData } from '$/providers/react'
import { useGetAssetChildren } from '../../../../layouts/Drive/assetsTableItemsHooks'

/** Props for a {@link DatalinkNameColumn}. */
export interface DatalinkNameColumnProps extends AssetColumnProps {
  readonly item: DatalinkAsset
}

/**
 * The icon and name of a {@link DatalinkAsset}.
 * @throws {Error} when the asset is not a {@link DatalinkAsset}.
 * This should never happen.
 */
export default function DatalinkNameColumn(props: DatalinkNameColumnProps) {
  const { item, state, isEditable } = props
  const { backend } = state

  const isEditing = useIsEditingName(item.id) && isEditable
  const setEditingNameAssetId = useSetEditingNameAssetId()
  const getAssetChildren = useGetAssetChildren()
  const rightPanel = useRightPanelData()

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
      onClick={(event) => {
        if (isDoubleClick(event)) {
          event.stopPropagation()
          rightPanel.setTemporaryTab('settings')
        }
      }}
    >
      <Icon icon="connector" className="m-name-column-icon" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={isEditable && isEditing}
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
