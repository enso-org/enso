/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as tailwindMerge from 'tailwind-merge'

import FolderArrowIcon from 'enso-assets/folder_arrow.svg'
import FolderIcon from 'enso-assets/folder.svg'

import * as store from '#/store'

import * as backendHooks from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import Button from '#/components/styled/Button'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as string from '#/utilities/string'

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.DirectoryAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DirectoryAsset}.
 * This should never happen. */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
  const { item, depth, state, isEditable } = props
  const { backend } = state
  const queryClient = reactQuery.useQueryClient()
  const { user } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const isEditingName = store.useStore(
    storeState => storeState.getAssetState(backend.type, item.id).isEditingName
  )
  const setIsAssetEditingName = store.useStore(storeState => storeState.setIsAssetEditingName)
  if (item.type !== backendModule.AssetType.directory) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DirectoryNameColumn` can only display folders.')
  }
  const toggleIsAssetOpen = store.useStore(storeState => storeState.toggleIsAssetOpen)
  const isOpen = store.useStore(
    storeState => storeState.getAssetState(backend.type, item.id).isOpen
  )

  const updateDirectoryMutation = backendHooks.useBackendMutation(backend, 'updateDirectory')

  const setIsEditing = (editing: boolean) => {
    if (isEditable) {
      setIsAssetEditingName(backend.type, item.id, editing)
    }
  }

  const doRename = async (newTitle: string) => {
    if (isEditable) {
      setIsEditing(false)
      if (string.isWhitespaceOnly(newTitle)) {
        // Do nothing.
      } else if (newTitle !== item.title) {
        await updateDirectoryMutation.mutateAsync([item.id, { title: newTitle }])
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
        if (isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={event => {
        if (handleClick(event)) {
          // Already handled.
        } else if (
          eventModule.isSingleClick(event) &&
          store.useStore.getState().getIsAssetSoleSelected(item.id)
        ) {
          event.stopPropagation()
          setIsEditing(true)
        }
      }}
    >
      <Button
        image={FolderArrowIcon}
        alt={isOpen ? getText('collapse') : getText('expand')}
        className={tailwindMerge.twMerge(
          'm-name-column-icon hidden size-icon cursor-pointer transition-transform duration-arrow group-hover:inline-block',
          isOpen && 'rotate-90'
        )}
        onPress={() => {
          toggleIsAssetOpen(backend.type, item.id)
        }}
      />
      <SvgMask src={FolderIcon} className="m-name-column-icon size-icon group-hover:hidden" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={isEditingName}
        className={tailwindMerge.twMerge(
          'text grow cursor-pointer bg-transparent',
          isEditingName ? 'cursor-text' : 'cursor-pointer'
        )}
        checkSubmittable={newTitle =>
          newTitle !== item.title &&
          (
            backendHooks.getBackendListDirectory(queryClient, user, backend, item.parentId) ?? []
          ).every(
            child => child.id === item.id || child.type !== item.type || child.title !== newTitle
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
