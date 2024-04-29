/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import ConnectorIcon from 'enso-assets/connector.svg'

import * as setAssetHooks from '#/hooks/setAssetHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'

// ==========================
// === DataLinkNameColumn ===
// ==========================

/** Props for a {@link DataLinkNameColumn}. */
export interface DataLinkNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.DataLinkAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DataLinkAsset}.
 * This should never happen. */
export default function DataLinkNameColumn(props: DataLinkNameColumnProps) {
  const { item, setItem, selected, state, rowState, setRowState, isEditable } = props
  const { setIsAssetPanelTemporarilyVisible } = state
  const inputBindings = inputBindingsProvider.useInputBindings()
  if (item.type !== backendModule.AssetType.dataLink) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DataLinkNameColumn` can only display Data Links.')
  }
  const smartAsset = item.item
  const asset = smartAsset.value
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
  // context menu entry should be re-added.
  // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
  const doRename = async () => {
    await Promise.resolve(null)
  }

  const handleClick = inputBindings.handler({
    editName: () => {
      setIsEditing(true)
    },
  })

  return (
    <div
      className={`flex h-full min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y ${indent.indentClass(
        item.depth
      )}`}
      onKeyDown={event => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={event => {
        if (handleClick(event)) {
          // Already handled.
        } else if (eventModule.isSingleClick(event) && selected) {
          setIsEditing(true)
        } else if (eventModule.isDoubleClick(event)) {
          event.stopPropagation()
          setIsAssetPanelTemporarilyVisible(true)
        }
      }}
    >
      <img src={ConnectorIcon} className="m-name-column-icon size-icon" />
      <EditableSpan
        editable={false}
        onSubmit={async newTitle => {
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
        className="text grow bg-transparent"
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
