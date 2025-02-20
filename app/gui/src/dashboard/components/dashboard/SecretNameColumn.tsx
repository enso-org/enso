/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import { useMutation } from '@tanstack/react-query'

import KeyIcon from '#/assets/key.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'

import type * as column from '#/components/dashboard/column'
import SvgMask from '#/components/SvgMask'

import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'

import EditableSpan from '#/components/EditableSpan'
import { useText } from '#/providers/TextProvider'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =====================
// === ConnectorName ===
// =====================

/** Props for a {@link SecretNameColumn}. */
export interface SecretNameColumnProps extends column.AssetColumnProps {
  readonly item: backendModule.SecretAsset
}

/**
 * The icon and name of a {@link backendModule.SecretAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.SecretAsset}.
 * This should never happen.
 */
export default function SecretNameColumn(props: SecretNameColumnProps) {
  const { item, selected, state, rowState, setRowState, isEditable, depth } = props
  const { backend, nodeMap } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = useText()
  const { setModal } = modalProvider.useSetModal()

  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))

  const doRename = async (newTitle: string) => {
    await updateSecretMutation.mutateAsync([item.id, { title: newTitle, value: null }, item.title])
    setIsEditing(false)
  }

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  return (
    <div
      className={tailwindMerge.twMerge(
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
          setIsEditing(true)
        } else if (eventModule.isDoubleClick(event) && isEditable) {
          event.stopPropagation()
          setModal(
            <UpsertSecretModal
              id={item.id}
              name={item.title}
              doCreate={async (title, value) => {
                try {
                  await updateSecretMutation.mutateAsync([item.id, { title, value }, item.title])
                } catch (error) {
                  toastAndLog(null, error)
                }
              }}
            />,
          )
        }
      }}
    >
      <SvgMask src={KeyIcon} className="m-name-column-icon size-4" />
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
