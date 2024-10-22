/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import { useMutation } from '@tanstack/react-query'

import FolderIcon from '#/assets/folder.svg'
import FolderArrowIcon from '#/assets/folder_arrow.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'

import { useDriveStore } from '#/providers/DriveProvider'
import * as textProvider from '#/providers/TextProvider'

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

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps extends column.AssetColumnProps {
  readonly item: backendModule.DirectoryAsset
}

/**
 * The icon and name of a {@link backendModule.DirectoryAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DirectoryAsset}.
 * This should never happen.
 */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
  const { item, selected, state, rowState, setRowState, isEditable, depth } = props
  const { backend, nodeMap, doToggleDirectoryExpansion, expandedDirectoryIds } = state
  const { getText } = textProvider.useText()
  const driveStore = useDriveStore()
  const isExpanded = expandedDirectoryIds.includes(item.id)

  const updateDirectoryMutation = useMutation(backendMutationOptions(backend, 'updateDirectory'))

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }

    if (!isEditingName) {
      driveStore.setState({ newestFolderId: null })
    }
  }

  const doRename = async (newTitle: string) => {
    if (isEditable) {
      setIsEditing(false)
      if (!string.isWhitespaceOnly(newTitle) && newTitle !== item.title) {
        await updateDirectoryMutation.mutateAsync([item.id, { title: newTitle }, item.title])
      }
    }
  }

  return (
    <div
      className={tailwindMerge.twMerge(
        'group flex h-table-row min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y',
        indent.indentClass(depth),
      )}
      onKeyDown={(event) => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={(event) => {
        if (
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
          doToggleDirectoryExpansion(item.id, item.id)
        }}
      />
      <SvgMask src={FolderIcon} className="m-name-column-icon size-4 group-hover:hidden" />
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={tailwindMerge.twMerge(
          'grow cursor-pointer bg-transparent font-naming',
          rowState.isEditingName ? 'cursor-text' : 'cursor-pointer',
        )}
        checkSubmittable={(newTitle) =>
          validation.DIRECTORY_NAME_REGEX.test(newTitle) &&
          backendModule.isNewTitleValid(
            item,
            newTitle,
            nodeMap.current.get(item.parentId)?.children?.map((child) => child.item),
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
