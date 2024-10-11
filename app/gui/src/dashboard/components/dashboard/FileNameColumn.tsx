/** @file The icon and name of a {@link backendModule.FileAsset}. */
import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'
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
  readonly item: AssetTreeNode<backendModule.FileAsset>
}

/**
 * The icon and name of a {@link backendModule.FileAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.FileAsset}.
 * This should never happen.
 */
export default function FileNameColumn(props: FileNameColumnProps) {
  const { item, setItem, selected, state, rowState, setRowState, isEditable } = props
  const { backend, nodeMap } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
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
