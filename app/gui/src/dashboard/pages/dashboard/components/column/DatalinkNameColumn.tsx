/** @file The icon and name of a {@link SecretAsset}. */
import EditableSpan from '#/components/EditableSpan'
import { Icon } from '#/components/Icon'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import { titleSchema, type DatalinkAsset } from '#/services/Backend'
import { isDoubleClick } from '#/utilities/event'
import { merger } from '#/utilities/object'
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
  const { item, rowState, setRowState, isEditable, renameAsset } = props

  const getAssetChildren = useGetAssetChildren()

  const rightPanel = useRightPanelData()

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
      onClick={(event) => {
        if (isDoubleClick(event)) {
          event.stopPropagation()
          rightPanel.setTemporaryTab('settings')
        }
      }}
    >
      <Icon icon="connector" className="m-name-column-icon" />
      <EditableSpan
        editable={rowState.isEditingName}
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
        className="grow bg-transparent font-naming"
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
