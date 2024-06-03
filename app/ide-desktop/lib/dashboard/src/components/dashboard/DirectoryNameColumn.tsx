/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import FolderArrowIcon from 'enso-assets/folder_arrow.svg'
import FolderIcon from 'enso-assets/folder.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import Button from '#/components/styled/Button'
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
  const { item, setItem, depth, selected, state, rowState, setRowState, isEditable } = props
  const { backend, selectedKeys, nodeMap, doToggleDirectoryExpansion } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  if (item.type !== backendModule.AssetType.directory) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DirectoryNameColumn` can only display folders.')
  }
  const setAsset = setAssetHooks.useSetAsset(item, setItem)

  const createDirectoryMutation = backendHooks.useBackendMutation(backend, 'createDirectory')
  const updateDirectoryMutation = backendHooks.useBackendMutation(backend, 'updateDirectory')

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
      } else if (newTitle !== item.title) {
        const oldTitle = item.title
        setAsset(object.merger({ title: newTitle }))
        try {
          await updateDirectoryMutation.mutateAsync([item.id, { title: newTitle }, item.title])
        } catch (error) {
          toastAndLog('renameFolderError', error)
          setAsset(object.merger({ title: oldTitle }))
        }
      }
    }
  }

  const handleClick = inputBindings.handler({
    editName: () => {
      setIsEditing(true)
    },
  })

  return (
    <div
      className={tailwindMerge.twMerge(
        'group flex h-full min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y',
        indent.indentClass(depth)
      )}
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
      <Button
        image={FolderArrowIcon}
        alt={item.children == null ? getText('expand') : getText('collapse')}
        className={tailwindMerge.twMerge(
          'm-name-column-icon hidden size-icon cursor-pointer transition-transform duration-arrow group-hover:inline-block',
          item.children != null && 'rotate-90'
        )}
        onPress={() => {
          doToggleDirectoryExpansion(item.id, item.title)
        }}
      />
      <SvgMask src={FolderIcon} className="m-name-column-icon size-icon group-hover:hidden" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={tailwindMerge.twMerge(
          'text grow cursor-pointer bg-transparent',
          rowState.isEditingName ? 'cursor-text' : 'cursor-pointer'
        )}
        checkSubmittable={newTitle =>
          newTitle !== item.title &&
          (nodeMap.current.get(item.parentId)?.children ?? []).every(
            child =>
              // All siblings,
              child.key === item.id ||
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
        {item.title}
      </EditableSpan>
    </div>
  )
}
