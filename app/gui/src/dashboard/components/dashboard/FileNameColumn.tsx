/** @file The icon and name of a {@link backendModule.FileAsset}. */
import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions } from '#/hooks/backendHooks'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import { useText } from '#/providers/TextProvider'
import * as eventModule from '#/utilities/event'
import * as fileIcon from '#/utilities/fileIcon'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
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

  const { getText } = useText()
  const updateFileMutation = useMutation(backendMutationOptions(backend, 'updateFile'))

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  const doRename = async (newTitle: string) => {
    await updateFileMutation.mutateAsync([item.id, { title: newTitle }, item.title])
    setIsEditing(false)
  }

  return (
    <div
      className={tailwindMerge.twJoin(
        'flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child',
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
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        schema={(z) =>
          z.refine(
            (value) =>
              backendModule.isNewTitleUnique(
                item,
                value,
                nodeMap.current.get(item.parentId)?.children?.map((child) => child.item),
              ),
            { message: getText('nameShouldBeUnique') },
          )
        }
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
