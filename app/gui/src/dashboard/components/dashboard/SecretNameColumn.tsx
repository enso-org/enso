/** @file The icon and name of a {@link SecretAsset}. */
import KeyIcon from '#/assets/key.svg'
import type { AssetColumnProps } from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { setModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { isAssetCredential, titleSchema, type SecretAsset } from '#/services/Backend'
import { isDoubleClick } from '#/utilities/event'
import { merger } from '#/utilities/object'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { toast } from 'react-toastify'

/** Props for a {@link SecretNameColumn}. */
export interface SecretNameColumnProps extends AssetColumnProps {
  readonly item: SecretAsset
}

/** The icon and name of a {@link SecretAsset}. */
export default function SecretNameColumn(props: SecretNameColumnProps) {
  const { item, rowState, state, setRowState, isEditable, renameAsset } = props
  const { backend } = state

  const toastAndLog = useToastAndLog()
  const { getText } = useText()
  const getAssetChildren = useGetAssetChildren()

  const updateSecretMutation = useMutationCallback(backendMutationOptions(backend, 'updateSecret'))

  const doRename = async (newTitle: string) => {
    await renameAsset(item.id, newTitle)
    setIsEditing(false)
  }

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(merger({ isEditingName }))
    }
  }

  return (
    <div
      className="flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={(event) => {
        if (isDoubleClick(event) && isEditable) {
          if (isAssetCredential(item)) {
            toast.warning(getText('cannotEditCredentialError'))
          } else {
            event.stopPropagation()
            setModal(
              <UpsertSecretModal
                id={item.id}
                name={item.title}
                doCreate={async (title, value) => {
                  try {
                    await updateSecretMutation([item.id, { title, value }, item.title])
                  } catch (error) {
                    toastAndLog(null, error)
                  }
                }}
              />,
            )
          }
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
        schema={() =>
          titleSchema({
            asset: item,
            siblings: getAssetChildren(item.parentId),
          })
        }
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
