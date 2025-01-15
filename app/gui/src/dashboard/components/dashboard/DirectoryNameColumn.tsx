/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import { useMutation } from '@tanstack/react-query'

import FolderIcon from '#/assets/folder.svg'
import FolderArrowIcon from '#/assets/folder_arrow.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'

import { useDriveStore, useToggleDirectoryExpansion } from '#/providers/DriveProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'

import * as backendModule from '#/services/Backend'

import { Button } from '#/components/AriaComponents'
import { useStore } from '#/hooks/storeHooks'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
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
  const { item, depth, selected, state, rowState, setRowState, isEditable } = props
  const { backend, nodeMap } = state
  const { getText } = textProvider.useText()
  const driveStore = useDriveStore()
  const toggleDirectoryExpansion = useToggleDirectoryExpansion()
  const isExpanded = useStore(driveStore, (storeState) =>
    storeState.expandedDirectoryIds.includes(item.id),
  )

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
    await updateDirectoryMutation.mutateAsync([item.id, { title: newTitle }, item.title])
    setIsEditing(false)
  }

  return (
    <div
      className={tailwindMerge.twJoin(
        'group flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child',
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
      <Button
        icon={({ isHovered }) => (isHovered || isExpanded ? FolderArrowIcon : FolderIcon)}
        size="medium"
        variant="icon"
        aria-label={isExpanded ? getText('collapse') : getText('expand')}
        tooltipPlacement="left"
        data-testid="directory-row-expand-button"
        data-expanded={isExpanded}
        className={tailwindMerge.twJoin(
          'mx-1 transition-transform duration-arrow',
          isExpanded && 'rotate-90',
        )}
        onPress={() => {
          toggleDirectoryExpansion(item.id)
        }}
      />

      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={tailwindMerge.twMerge(
          'cursor-pointer bg-transparent font-naming',
          rowState.isEditingName ? 'cursor-text' : 'cursor-pointer',
        )}
        schema={(z) =>
          z
            .refine((value) => !validation.isDirectoryNameContainInvalidCharacters(value), {
              message: getText('nameShouldNotContainInvalidCharacters'),
            })
            .refine(
              (value) =>
                backendModule.isNewTitleUnique(
                  item,
                  value,
                  nodeMap.current.get(item.parentId)?.children?.map((child) => child.item),
                ),
              { message: getText('nameShouldBeUnique') },
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
