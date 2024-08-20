/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import KeyIcon from '#/assets/key.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import SvgMask from '#/components/SvgMask'

import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'

// =====================
// === ConnectorName ===
// =====================

/** Props for a {@link SecretNameColumn}. */
export interface SecretNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.SecretAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.SecretAsset}.
 * This should never happen. */
export default function SecretNameColumn(props: SecretNameColumnProps) {
  const { item, setItem, selected, state, rowState, setRowState, isEditable } = props
  const { backend } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setModal } = modalProvider.useSetModal()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  if (item.type !== backendModule.AssetType.secret) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`SecretNameColumn` can only display secrets.')
  }
  const asset = item.item

  const createSecretMutation = useMutation(backendMutationOptions(backend, 'createSecret'))
  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  eventListProvider.useAssetEventListener(async (event) => {
    if (isEditable) {
      switch (event.type) {
        case AssetEventType.newProject:
        case AssetEventType.newFolder:
        case AssetEventType.uploadFiles:
        case AssetEventType.newDatalink:
        case AssetEventType.updateFiles:
        case AssetEventType.copy:
        case AssetEventType.cut:
        case AssetEventType.cancelCut:
        case AssetEventType.move:
        case AssetEventType.delete:
        case AssetEventType.deleteForever:
        case AssetEventType.restore:
        case AssetEventType.download:
        case AssetEventType.downloadSelected:
        case AssetEventType.removeSelf:
        case AssetEventType.temporarilyAddLabels:
        case AssetEventType.temporarilyRemoveLabels:
        case AssetEventType.addLabels:
        case AssetEventType.removeLabels:
        case AssetEventType.deleteLabel:
        case AssetEventType.setItem:
        case AssetEventType.projectClosed: {
          // Ignored. These events should all be unrelated to secrets.
          // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
          // are handled by`AssetRow`.
          break
        }
        case AssetEventType.newSecret: {
          if (item.key === event.placeholderId) {
            if (backend.type !== backendModule.BackendType.remote) {
              toastAndLog('localBackendSecretError')
            } else {
              rowState.setVisibility(Visibility.faded)
              try {
                const id = await createSecretMutation.mutateAsync([
                  {
                    parentDirectoryId: asset.parentId,
                    name: asset.title,
                    value: event.value,
                  },
                ])
                rowState.setVisibility(Visibility.visible)
                setAsset(object.merger({ id }))
              } catch (error) {
                dispatchAssetListEvent({
                  type: AssetListEventType.delete,
                  key: item.key,
                })
                toastAndLog('createSecretError', error)
              }
            }
          }
          break
        }
      }
    }
  }, item.initialAssetEvents)

  const handleClick = inputBindings.handler({
    editName: () => {
      setIsEditing(true)
    },
  })

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
        if (handleClick(event)) {
          // Already handled.
        } else if (eventModule.isSingleClick(event) && selected) {
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
