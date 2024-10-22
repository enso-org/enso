/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import { useMutation } from '@tanstack/react-query'

import KeyIcon from '#/assets/key.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'

import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import SvgMask from '#/components/SvgMask'

import UpsertSecretModal from '#/modals/UpsertSecretModal'

import type * as backendModule from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =====================
// === ConnectorName ===
// =====================

/** Props for a {@link SecretNameColumn}. */
export interface SecretNameColumnProps extends column.AssetColumnProps {
  readonly item: AssetTreeNode<backendModule.SecretAsset>
}

/**
 * The icon and name of a {@link backendModule.SecretAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.SecretAsset}.
 * This should never happen.
 */
export default function SecretNameColumn(props: SecretNameColumnProps) {
  const { item, selected, state, rowState, setRowState, isEditable } = props
  const { backend } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setModal } = modalProvider.useSetModal()
  const asset = item.item

  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
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
          setIsEditing(true)
        } else if (eventModule.isDoubleClick(event) && isEditable) {
          event.stopPropagation()
          setModal(
            <UpsertSecretModal
              id={asset.id}
              name={asset.title}
              doCreate={async (_name, value) => {
                try {
                  await updateSecretMutation.mutateAsync([asset.id, { value }, asset.title])
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
      {/* Secrets cannot be renamed. */}
      <ariaComponents.Text
        data-testid="asset-row-name"
        font="naming"
        className="grow bg-transparent"
      >
        {asset.title}
      </ariaComponents.Text>
    </div>
  )
}
