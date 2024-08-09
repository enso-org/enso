/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import FolderIcon from '#/assets/folder.svg'
import FolderArrowIcon from '#/assets/folder_arrow.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import { useDriveStore } from '#/providers/DriveProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as string from '#/utilities/string'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import * as validation from '#/utilities/validation'
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
  const { backend, nodeMap } = state
  const { doToggleDirectoryExpansion } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const driveStore = useDriveStore()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  if (item.type !== backendModule.AssetType.directory) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DirectoryNameColumn` can only display folders.')
  }
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const isExpanded = item.children != null && item.isExpanded

  const createDirectoryMutation = useMutation(backendMutationOptions(backend, 'createDirectory'))
  const updateDirectoryMutation = useMutation(backendMutationOptions(backend, 'updateDirectory'))

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
          const updated = await updateDirectoryMutation.mutateAsync([
            asset.id,
            { title: newTitle },
            asset.title,
          ])
          setAsset(object.merger(updated))
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
        case AssetEventType.uploadFiles:
        case AssetEventType.newDatalink:
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
          // Ignored. These events should all be unrelated to directories.
          // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
          // are handled by`AssetRow`.
          break
        }
        case AssetEventType.newFolder: {
          if (item.key === event.placeholderId) {
            rowState.setVisibility(Visibility.faded)
            try {
              const createdDirectory = await createDirectoryMutation.mutateAsync([
                {
                  parentId: asset.parentId,
                  title: asset.title,
                },
              ])
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
        'group flex h-table-row min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y',
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
        } else if (
          eventModule.isSingleClick(event) &&
          selected &&
          driveStore.getState().selectedKeys.size === 1
        ) {
          event.stopPropagation()
          setIsEditing(true)
        }
      }}
    >
      <ariaComponents.Button
        icon={FolderArrowIcon}
        size="medium"
        variant="custom"
        aria-label={isExpanded ? getText('collapse') : getText('expand')}
        tooltipPlacement="left"
        className={tailwindMerge.twMerge(
          'm-0 hidden cursor-pointer border-0 transition-transform duration-arrow group-hover:m-name-column-icon group-hover:inline-block',
          isExpanded && 'rotate-90',
        )}
        onPress={() => {
          doToggleDirectoryExpansion(asset.id, item.key, asset.title)
        }}
      />
      <SvgMask src={FolderIcon} className="m-name-column-icon size-4 group-hover:hidden" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={tailwindMerge.twMerge(
          'text grow cursor-pointer bg-transparent font-naming',
          rowState.isEditingName ? 'cursor-text' : 'cursor-pointer',
        )}
        checkSubmittable={(newTitle) =>
          validation.DIRECTORY_NAME_REGEX.test(newTitle) &&
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
