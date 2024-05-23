/** @file The icon and name of a {@link backendModule.FileAsset}. */
import * as React from 'react'

import * as eventHooks from '#/hooks/eventHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as fileIcon from '#/utilities/fileIcon'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as string from '#/utilities/string'
import Visibility from '#/utilities/Visibility'

// ================
// === FileName ===
// ================

/** Props for a {@link FileNameColumn}. */
export interface FileNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.FileAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.FileAsset}.
 * This should never happen. */
export default function FileNameColumn(props: FileNameColumnProps) {
  const { item, setItem, selected, state, rowState, setRowState, isEditable } = props
  const { nodeMap, assetEvents, dispatchAssetListEvent } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { backend } = backendProvider.useBackend()
  const inputBindings = inputBindingsProvider.useInputBindings()
  if (item.type !== backendModule.AssetType.file) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`FileNameColumn` can only display files.')
  }
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const isCloud = backend.type === backendModule.BackendType.remote

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
  // context menu entry should be re-added.
  // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
  const doRename = async (newTitle: string) => {
    if (isEditable) {
      setIsEditing(false)
      if (string.isWhitespaceOnly(newTitle)) {
        // Do nothing.
      } else if (!isCloud && newTitle !== asset.title) {
        const oldTitle = asset.title
        setAsset(object.merger({ title: newTitle }))
        try {
          await backend.updateFile(asset.id, { title: newTitle }, asset.title)
        } catch (error) {
          toastAndLog('renameFolderError', error)
          setAsset(object.merger({ title: oldTitle }))
        }
      }
    }
  }

  eventHooks.useEventHandler(
    assetEvents,
    async event => {
      switch (event.type) {
        case AssetEventType.newProject:
        case AssetEventType.newFolder:
        case AssetEventType.newDatalink:
        case AssetEventType.newSecret:
        case AssetEventType.openProject:
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
          // Ignored. These events should all be unrelated to projects.
          // `delete`, `deleteForever`, `restoreMultiple`, `download`, and `downloadSelected`
          // are handled by `AssetRow`.
          break
        }
        case AssetEventType.updateFiles:
        case AssetEventType.uploadFiles: {
          const file = event.files.get(item.item.id)
          if (file != null) {
            const fileId = event.type !== AssetEventType.updateFiles ? null : asset.id
            rowState.setVisibility(Visibility.faded)
            try {
              const createdFile = await backend.uploadFile(
                { fileId, fileName: asset.title, parentDirectoryId: asset.parentId },
                file
              )
              rowState.setVisibility(Visibility.visible)
              setAsset(object.merge(asset, { id: createdFile.id }))
            } catch (error) {
              switch (event.type) {
                case AssetEventType.uploadFiles: {
                  dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
                  toastAndLog(null, error)
                  break
                }
                case AssetEventType.updateFiles: {
                  toastAndLog(null, error)
                  break
                }
              }
            }
          }
          break
        }
      }
    },
    { isDisabled: !isEditable }
  )

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
          if (!isCloud) {
            setIsEditing(true)
          }
        }
      }}
    >
      <SvgMask src={fileIcon.fileIcon()} className="m-name-column-icon size-icon" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className="text grow bg-transparent"
        checkSubmittable={newTitle =>
          newTitle !== item.item.title &&
          (nodeMap.current.get(item.directoryKey)?.children ?? []).every(
            child =>
              // All siblings,
              child.key === item.key ||
              // that are not directories,
              backendModule.assetIsDirectory(child.item) ||
              // must have a different name.
              child.item.title !== newTitle
          )
        }
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
