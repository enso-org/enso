/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import ConnectorIcon from 'enso-assets/connector.svg'

import * as eventHooks from '#/hooks/eventHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import Visibility from '#/utilities/Visibility'

// =====================
// === ConnectorName ===
// =====================

/** Props for a {@link DataLinkNameColumn}. */
export interface DataLinkNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.DataLinkAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DataLinkAsset}.
 * This should never happen. */
export default function DataLinkNameColumn(props: DataLinkNameColumnProps) {
  const { item, setItem, selected, state, rowState, setRowState } = props
  const { assetEvents, dispatchAssetListEvent, setIsAssetPanelTemporarilyVisible } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { backend } = backendProvider.useBackend()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const asset = item.item
  if (asset.type !== backendModule.AssetType.dataLink) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DataLinkNameColumn` can only display Data Links.')
  }
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
  // context menu entry should be re-added.
  // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
  const doRename = async () => {
    await Promise.resolve(null)
  }

  eventHooks.useEventHandler(assetEvents, async event => {
    switch (event.type) {
      case AssetEventType.newProject:
      case AssetEventType.newFolder:
      case AssetEventType.uploadFiles:
      case AssetEventType.newSecret:
      case AssetEventType.openProject:
      case AssetEventType.updateFiles:
      case AssetEventType.closeProject:
      case AssetEventType.copy:
      case AssetEventType.cut:
      case AssetEventType.cancelCut:
      case AssetEventType.move:
      case AssetEventType.delete:
      case AssetEventType.deleteForever:
      case AssetEventType.restore:
      case AssetEventType.download:
      case AssetEventType.downloadSelected:
      case AssetEventType.removeSelf:
      case AssetEventType.temporarilyAddLabels:
      case AssetEventType.temporarilyRemoveLabels:
      case AssetEventType.addLabels:
      case AssetEventType.removeLabels:
      case AssetEventType.deleteLabel: {
        // Ignored. These events should all be unrelated to secrets.
        // `delete`, `deleteForever`, `restoreMultiple`, `download`, and `downloadSelected`
        // are handled by `AssetRow`.
        break
      }
      case AssetEventType.newDataLink: {
        if (item.key === event.placeholderId) {
          if (backend.type !== backendModule.BackendType.remote) {
            toastAndLog('localBackendDataLinkError')
          } else {
            rowState.setVisibility(Visibility.faded)
            try {
              const { id } = await backend.createConnector({
                parentDirectoryId: asset.parentId,
                connectorId: null,
                name: asset.title,
                value: event.value,
              })
              rowState.setVisibility(Visibility.visible)
              setAsset(object.merger({ id }))
            } catch (error) {
              dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
              toastAndLog('createDataLinkError', error)
            }
          }
        }
        break
      }
    }
  })

  const handleClick = inputBindings.handler({
    editName: () => {
      setRowState(object.merger({ isEditingName: true }))
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
          setRowState(object.merger({ isEditingName: true }))
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
        className="text grow bg-transparent"
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
