/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import DatalinkIcon from '#/assets/datalink.svg'

import * as setAssetHooks from '#/hooks/setAssetHooks'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'

import type * as backendModule from '#/services/Backend'

import { useSetIsAssetPanelTemporarilyVisible } from '#/providers/DriveProvider'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// ====================
// === DatalinkName ===
// ====================

/** Props for a {@link DatalinkNameColumn}. */
export interface DatalinkNameColumnProps extends column.AssetColumnProps {
  readonly item: AssetTreeNode<backendModule.DatalinkAsset>
}

/**
 * The icon and name of a {@link backendModule.DatalinkAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DatalinkAsset}.
 * This should never happen.
 */
export default function DatalinkNameColumn(props: DatalinkNameColumnProps) {
  const { item, setItem, selected, rowState, setRowState, isEditable } = props
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
  // context menu entry should be re-added.
  // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
  const doRename = () => Promise.resolve(null)

  return (
    <div
      className={tailwindMerge.twMerge(
        'flex h-table-row min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y',
        indent.indentClass(item.depth),
      )}
      onKeyDown={(event) => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={(event) => {
        if (eventModule.isSingleClick(event) && selected) {
          setIsEditing(true)
        } else if (eventModule.isDoubleClick(event)) {
          event.stopPropagation()
          setIsAssetPanelTemporarilyVisible(true)
        }
      }}
    >
      <img src={DatalinkIcon} className="m-name-column-icon size-4" />
      <EditableSpan
        editable={false}
        onSubmit={async (newTitle) => {
          setIsEditing(false)

          if (newTitle !== asset.title) {
            const oldTitle = asset.title
            setAsset(object.merger({ title: newTitle }))
            try {
              await doRename()
            } catch {
              setAsset(object.merger({ title: oldTitle }))
            }
          }
        }}
        onCancel={() => {
          setIsEditing(false)
        }}
        className="grow bg-transparent font-naming"
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
