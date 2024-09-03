/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import DatalinkIcon from '#/assets/datalink.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import { isOnMacOS } from 'enso-common/src/detect'

// ====================
// === DatalinkName ===
// ====================

/** Props for a {@link DatalinkNameColumn}. */
export interface DatalinkNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.DatalinkAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DatalinkAsset}.
 * This should never happen. */
export default function DatalinkNameColumn(props: DatalinkNameColumnProps) {
  const { item, setItem, selected, state, rowState, setRowState, isEditable } = props
  const { backend, setIsAssetPanelTemporarilyVisible } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  if (item.type !== backendModule.AssetType.datalink) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DatalinkNameColumn` can only display Datalinks.')
  }
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  const createDatalinkMutation = useMutation(backendMutationOptions(backend, 'createDatalink'))

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

  eventListProvider.useAssetEventListener(async (event) => {
    if (isEditable) {
      switch (event.type) {
        case AssetEventType.newProject:
        case AssetEventType.newFolder:
        case AssetEventType.uploadFiles:
        case AssetEventType.newSecret:
        case AssetEventType.updateFiles:
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
        case AssetEventType.deleteLabel:
        case AssetEventType.setItem:
        case AssetEventType.projectClosed: {
          // Ignored. These events should all be unrelated to secrets.
          // `delete`, `deleteForever`, `restoreMultiple`, `download`, and `downloadSelected`
          // are handled by `AssetRow`.
          break
        }
        case AssetEventType.newDatalink: {
          if (item.key === event.placeholderId) {
            if (backend.type !== backendModule.BackendType.remote) {
              toastAndLog('localBackendDatalinkError')
            } else {
              rowState.setVisibility(Visibility.faded)
              try {
                const { id } = await createDatalinkMutation.mutateAsync([
                  {
                    parentDirectoryId: asset.parentId,
                    datalinkId: null,
                    name: asset.title,
                    value: event.value,
                  },
                ])
                rowState.setVisibility(Visibility.visible)
                setAsset(object.merger({ id }))
              } catch (error) {
                dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
                toastAndLog('createDatalinkError', error)
              }
            }
          }
          break
        }
      }
    }
  }, item.initialAssetEvents)

  const handleClick = inputBindings.handler({
    editName: () => {
      setIsEditing(true)
    },
  })

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
        if (handleClick(event)) {
          // Already handled.
        } else if (eventModule.isSingleClick(event) && isOnMacOS() && selected) {
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
        className="text grow bg-transparent font-naming"
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
