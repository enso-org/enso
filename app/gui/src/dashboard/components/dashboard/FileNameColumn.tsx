/** @file The icon and name of a {@link backendModule.FileAsset}. */
import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions } from '#/hooks/backendHooks'

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

// ================
// === FileName ===
// ================

/** Props for a {@link FileNameColumn}. */
export interface FileNameColumnProps extends column.AssetColumnProps {
  readonly item: backendModule.FileAsset
}

/**
 * The icon and name of a {@link backendModule.FileAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.FileAsset}.
 * This should never happen.
 */
export default function FileNameColumn(props: FileNameColumnProps) {
  const { item, selected, state, rowState, setRowState, isEditable, depth } = props
  const { backend, nodeMap } = state
  const isCloud = backend.type === backendModule.BackendType.remote

  const updateFileMutation = useMutation(backendMutationOptions(backend, 'updateFile'))

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
      } else if (!isCloud && newTitle !== item.title) {
        await updateFileMutation.mutateAsync([item.id, { title: newTitle }, item.title])
      }
    }
  }

  return (
    <div
      className={tailwindMerge.twMerge(
        'flex h-table-row min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y',
        indent.indentClass(depth),
      )}
      onKeyDown={(event) => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={(event) => {
        if (eventModule.isSingleClick(event) && selected) {
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
        className="grow bg-transparent font-naming"
        checkSubmittable={(newTitle) =>
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
