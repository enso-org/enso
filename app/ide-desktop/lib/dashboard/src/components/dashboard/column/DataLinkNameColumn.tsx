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
  const { item, setItem, selected, state, rowState, setRowState } = props
  const { setIsAssetPanelTemporarilyVisible } = state
  const inputBindings = inputBindingsProvider.useInputBindings()
  const smartAsset = item.item
  if (smartAsset.type !== backendModule.AssetType.dataLink) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DataLinkNameColumn` can only display Data Links.')
  }
  const asset = smartAsset.value
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
  // context menu entry should be re-added.
  // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
  const doRename = async () => {
    await Promise.resolve(null)
  }

  const handleClick = inputBindings.handler({
    editName: () => {
      setRowState(object.merger({ isEditingName: true }))
    },
  })

  return (
    <div
      className={`flex text-left items-center whitespace-nowrap rounded-l-full gap-1 px-1.5 py-1 min-w-max ${indent.indentClass(
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
          setRowState(object.merger({ isEditingName: true }))
        } else if (eventModule.isDoubleClick(event)) {
          event.stopPropagation()
          setIsAssetPanelTemporarilyVisible(true)
        }
      }}
    >
      <img src={ConnectorIcon} className="m-1" />
      <EditableSpan
        editable={false}
        onSubmit={async newTitle => {
          setRowState(object.merger({ isEditingName: false }))
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
          setRowState(object.merger({ isEditingName: false }))
        }}
        className="bg-transparent grow leading-170 h-6 py-px"
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
