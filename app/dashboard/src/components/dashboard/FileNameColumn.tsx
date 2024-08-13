/** @file The icon and name of a {@link backendModule.FileAsset}. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as fileIcon from '#/utilities/fileIcon'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as string from '#/utilities/string'
import * as tailwindMerge from '#/utilities/tailwindMerge'
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
  const { backend, nodeMap } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  if (item.type !== backendModule.AssetType.file) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`FileNameColumn` can only display files.')
  }
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const isCloud = backend.type === backendModule.BackendType.remote

  const updateFileMutation = useMutation(backendMutationOptions(backend, 'updateFile'))
  const uploadFileMutation = useMutation(
    backendMutationOptions(backend, 'uploadFile', {
      meta: {
        invalidates: [['assetVersions', item.item.id, item.item.title]],
        awaitInvalidates: true,
      },
    }),
  )

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
          await updateFileMutation.mutateAsync([asset.id, { title: newTitle }, asset.title])
        } catch (error) {
          toastAndLog('renameFolderError', error)
          setAsset(object.merger({ title: oldTitle }))
        }
      }
    }
  }

  eventListProvider.useAssetEventListener(async (event) => {
    if (isEditable) {
      switch (event.type) {
        case AssetEventType.newProject:
        case AssetEventType.newFolder:
        case AssetEventType.newDatalink:
        case AssetEventType.newSecret:
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
              const createdFile = await uploadFileMutation.mutateAsync([
                { fileId, fileName: asset.title, parentDirectoryId: asset.parentId },
                file,
              ])
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
        } else if (eventModule.isSingleClick(event) && selected) {
          if (!isCloud) {
            setIsEditing(true)
          }
        }
      }}
    >
      <SvgMask src={fileIcon.fileIcon()} className="m-name-column-icon size-4" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className="text grow bg-transparent font-naming"
        checkSubmittable={(newTitle) =>
          item.isNewTitleValid(newTitle, nodeMap.current.get(item.directoryKey)?.children)
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
