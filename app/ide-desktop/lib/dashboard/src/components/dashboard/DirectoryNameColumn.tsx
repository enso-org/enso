/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'

import FolderArrowIcon from 'enso-assets/folder_arrow.svg'
import FolderIcon from 'enso-assets/folder.svg'

import * as eventHooks from '#/hooks/eventHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as string from '#/utilities/string'
import Visibility from '#/utilities/Visibility'

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.DirectoryAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DirectoryAsset}.
 * This should never happen. */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
  const { item, setItem, selected, state, rowState, setRowState, isEditable } = props
  const { backend, selectedKeys, assetEvents, dispatchAssetListEvent, nodeMap } = state
  const { doToggleDirectoryExpansion } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  if (item.type !== backendModule.AssetType.directory) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DirectoryNameColumn` can only display folders.')
  }
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  const doRename = async (newTitle: string) => {
    if (isEditable) {
      setIsEditing(false)
      if (string.isWhitespaceOnly(newTitle)) {
        // Do nothing.
      } else if (newTitle !== asset.title) {
        const oldTitle = asset.title
        setAsset(object.merger({ title: newTitle }))
        try {
          await backend.updateDirectory(asset.id, { title: newTitle }, asset.title)
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
        case AssetEventType.uploadFiles:
        case AssetEventType.newDataLink:
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
          // Ignored. These events should all be unrelated to directories.
          // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
          // are handled by`AssetRow`.
          break
        }
        case AssetEventType.newFolder: {
          if (item.key === event.placeholderId) {
            rowState.setVisibility(Visibility.faded)
            try {
              const createdDirectory = await backend.createDirectory({
                parentId: asset.parentId,
                title: asset.title,
              })
              rowState.setVisibility(Visibility.visible)
              setAsset(object.merge(asset, createdDirectory))
            } catch (error) {
              dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
              toastAndLog('createFolderError', error)
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
      className={`group flex h-full min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y ${indent.indentClass(
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
        } else if (
          eventModule.isSingleClick(event) &&
          selected &&
          selectedKeys.current.size === 1
        ) {
          event.stopPropagation()
          setIsEditing(true)
        }
      }}
    >
      <SvgMask
        src={FolderArrowIcon}
        alt={item.children == null ? getText('expand') : getText('collapse')}
        className={`m-name-column-icon hidden size-icon cursor-pointer transition-transform duration-arrow group-hover:inline-block ${
          item.children != null ? 'rotate-90' : ''
        }`}
        onClick={event => {
          event.stopPropagation()
          doToggleDirectoryExpansion(asset.id, item.key, asset.title)
        }}
      />
      <SvgMask src={FolderIcon} className="m-name-column-icon size-icon group-hover:hidden" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={`text grow cursor-pointer bg-transparent ${
          rowState.isEditingName ? 'cursor-text' : 'cursor-pointer'
        }`}
        checkSubmittable={newTitle =>
          newTitle !== item.item.title &&
          (nodeMap.current.get(item.directoryKey)?.children ?? []).every(
            child =>
              // All siblings,
              child.key === item.key ||
              // that are directories,
              !backendModule.assetIsDirectory(child.item) ||
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
