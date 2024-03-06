/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as toast from 'react-toastify'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as loggerProvider from '#/providers/LoggerProvider'
import * as modalProvider from '#/providers/ModalProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import Category from '#/layouts/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenus from '#/components/ContextMenus'
import ContextMenuSeparator from '#/components/ContextMenuSeparator'
import type * as assetRow from '#/components/dashboard/AssetRow'
import MenuEntry from '#/components/MenuEntry'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import RemoteBackend from '#/services/RemoteBackend'

import HttpClient from '#/utilities/HttpClient'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'

// ========================
// === AssetContextMenu ===
// ========================

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps {
  readonly hidden?: boolean
  readonly innerProps: assetRow.AssetRowInnerProps
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
  readonly eventTarget: HTMLElement | null
  readonly doDelete: () => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.AssetId,
    newParentId: backendModule.DirectoryId
  ) => void
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps) {
  const { innerProps, event, eventTarget, doCopy, doCut, doPaste, doDelete } = props
  const { hidden = false } = props
  const { item, setItem, state, setRowState } = innerProps
  const { category, hasPasteData, labels, dispatchAssetEvent, dispatchAssetListEvent } = state
  const { doCreateLabel } = state

  const logger = loggerProvider.useLogger()
  const { user, accessToken } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const asset = item.item
  const self = asset.permissions?.find(
    backendModule.isUserPermissionAnd(permission => permission.user.user_email === user?.email)
  )
  const isCloud = backend.type === backendModule.BackendType.remote
  const ownsThisAsset = !isCloud || self?.permission === permissions.PermissionAction.own
  const managesThisAsset = ownsThisAsset || self?.permission === permissions.PermissionAction.admin
  const canEditThisAsset =
    managesThisAsset || self?.permission === permissions.PermissionAction.edit
  const isRunningProject =
    asset.type === backendModule.AssetType.project &&
    backendModule.IS_OPENING_OR_OPENED[asset.projectState.type]
  const canExecute =
    backend.type === backendModule.BackendType.local ||
    (self?.permission != null && permissions.PERMISSION_ACTION_CAN_EXECUTE[self.permission])
  const isOtherUserUsingProject =
    backend.type !== backendModule.BackendType.local &&
    backendModule.assetIsProject(asset) &&
    asset.projectState.opened_by != null &&
    asset.projectState.opened_by !== user?.email
  const setAsset = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
      if (typeof valueOrUpdater === 'function') {
        setItem(oldItem =>
          oldItem.with({
            item: valueOrUpdater(oldItem.item),
          })
        )
      } else {
        setItem(oldItem => oldItem.with({ item: valueOrUpdater }))
      }
    },
    [/* should never change */ setItem]
  )

  return category === Category.trash ? (
    !ownsThisAsset ? null : (
      <ContextMenus hidden={hidden} key={asset.id} event={event}>
        <ContextMenu hidden={hidden}>
          <MenuEntry
            hidden={hidden}
            action="undelete"
            label="Restore From Trash"
            doAction={() => {
              unsetModal()
              dispatchAssetEvent({ type: AssetEventType.restore, ids: new Set([asset.id]) })
            }}
          />
          <MenuEntry
            hidden={hidden}
            action="delete"
            label="Delete Forever"
            doAction={() => {
              setModal(
                <ConfirmDeleteModal
                  actionText={`delete the ${asset.type} '${asset.title}' forever`}
                  doDelete={() => {
                    const ids = new Set([asset.id])
                    dispatchAssetEvent({ type: AssetEventType.deleteForever, ids })
                  }}
                />
              )
            }}
          />
        </ContextMenu>
      </ContextMenus>
    )
  ) : (
    <ContextMenus hidden={hidden} key={asset.id} event={event}>
      <ContextMenu hidden={hidden}>
        {asset.type === backendModule.AssetType.project &&
          canExecute &&
          !isRunningProject &&
          !isOtherUserUsingProject && (
            <MenuEntry
              hidden={hidden}
              action="open"
              doAction={() => {
                unsetModal()
                dispatchAssetEvent({
                  type: AssetEventType.openProject,
                  id: asset.id,
                  shouldAutomaticallySwitchPage: true,
                  runInBackground: false,
                })
              }}
            />
          )}
        {asset.type === backendModule.AssetType.project && isCloud && (
          <MenuEntry
            hidden={hidden}
            action="run"
            doAction={() => {
              unsetModal()
              dispatchAssetEvent({
                type: AssetEventType.openProject,
                id: asset.id,
                shouldAutomaticallySwitchPage: false,
                runInBackground: true,
              })
            }}
          />
        )}
        {asset.type === backendModule.AssetType.project &&
          canExecute &&
          isRunningProject &&
          !isOtherUserUsingProject && (
            <MenuEntry
              hidden={hidden}
              action="close"
              doAction={() => {
                unsetModal()
                dispatchAssetEvent({
                  type: AssetEventType.closeProject,
                  id: asset.id,
                })
              }}
            />
          )}
        {asset.type === backendModule.AssetType.project && !isCloud && (
          <MenuEntry
            hidden={hidden}
            action="uploadToCloud"
            doAction={async () => {
              unsetModal()
              if (accessToken == null) {
                toastAndLog('Cannot upload to cloud in offline mode')
              } else {
                try {
                  const client = new HttpClient([['Authorization', `Bearer ${accessToken}`]])
                  const remoteBackend = new RemoteBackend(client, logger)
                  const projectResponse = await fetch(
                    `./api/project-manager/projects/${asset.id}/enso-project`
                  )
                  // This DOES NOT update the cloud assets list when it
                  // completes, as the current backend is not the remote
                  // (cloud) backend. The user may change to the cloud backend
                  // while this request is in progress, however this is
                  // uncommon enough that it is not worth the added complexity.
                  await remoteBackend.uploadFile(
                    {
                      fileName: `${asset.title}.enso-project`,
                      fileId: null,
                      parentDirectoryId: null,
                    },
                    await projectResponse.blob()
                  )
                  toast.toast.success('Successfully uploaded local project to cloud!')
                } catch (error) {
                  toastAndLog('Could not upload local project to cloud', error)
                }
              }
            }}
          />
        )}
        {canExecute && !isRunningProject && !isOtherUserUsingProject && (
          <MenuEntry
            hidden={hidden}
            disabled={
              asset.type !== backendModule.AssetType.project &&
              asset.type !== backendModule.AssetType.directory
            }
            action="rename"
            doAction={() => {
              setRowState(object.merger({ isEditingName: true }))
              unsetModal()
            }}
          />
        )}
        {asset.type === backendModule.AssetType.secret && canEditThisAsset && (
          <MenuEntry
            hidden={hidden}
            action="edit"
            doAction={() => {
              setModal(
                <UpsertSecretModal
                  id={asset.id}
                  name={asset.title}
                  doCreate={async (_name, value) => {
                    try {
                      await backend.updateSecret(asset.id, { value }, asset.title)
                    } catch (error) {
                      toastAndLog(null, error)
                    }
                  }}
                />
              )
            }}
          />
        )}
        {isCloud && (
          <MenuEntry
            hidden={hidden}
            disabled
            action="snapshot"
            doAction={() => {
              // No backend support yet.
            }}
          />
        )}
        {ownsThisAsset && !isRunningProject && !isOtherUserUsingProject && (
          <MenuEntry
            hidden={hidden}
            action="delete"
            label={backend.type === backendModule.BackendType.local ? 'Delete' : 'Move To Trash'}
            doAction={() => {
              if (backend.type === backendModule.BackendType.remote) {
                unsetModal()
                doDelete()
              } else {
                setModal(
                  <ConfirmDeleteModal
                    actionText={`delete the ${asset.type} '${asset.title}'`}
                    doDelete={doDelete}
                  />
                )
              }
            }}
          />
        )}
        {isCloud && <ContextMenuSeparator hidden={hidden} />}
        {isCloud && managesThisAsset && self != null && (
          <MenuEntry
            hidden={hidden}
            action="share"
            doAction={() => {
              setModal(
                <ManagePermissionsModal
                  item={asset}
                  setItem={setAsset}
                  self={self}
                  eventTarget={eventTarget}
                  doRemoveSelf={() => {
                    dispatchAssetEvent({
                      type: AssetEventType.removeSelf,
                      id: asset.id,
                    })
                  }}
                />
              )
            }}
          />
        )}
        {isCloud && (
          <MenuEntry
            hidden={hidden}
            action="label"
            doAction={() => {
              setModal(
                <ManageLabelsModal
                  item={asset}
                  setItem={setAsset}
                  allLabels={labels}
                  doCreateLabel={doCreateLabel}
                  eventTarget={eventTarget}
                />
              )
            }}
          />
        )}
        {isCloud && managesThisAsset && self != null && <ContextMenuSeparator hidden={hidden} />}
        <MenuEntry
          hidden={hidden}
          disabled={!isCloud}
          action="duplicate"
          doAction={() => {
            unsetModal()
            dispatchAssetListEvent({
              type: AssetListEventType.copy,
              newParentId: item.directoryId,
              newParentKey: item.directoryKey,
              items: [asset],
            })
          }}
        />
        {isCloud && <MenuEntry hidden={hidden} action="copy" doAction={doCopy} />}
        {isCloud && !isOtherUserUsingProject && (
          <MenuEntry hidden={hidden} action="cut" doAction={doCut} />
        )}
        <MenuEntry
          hidden={hidden}
          disabled={isCloud && asset.type !== backendModule.AssetType.file}
          action="download"
          doAction={() => {
            unsetModal()
            dispatchAssetEvent({
              type: AssetEventType.download,
              ids: new Set([asset.id]),
            })
          }}
        />
        {hasPasteData && (
          <MenuEntry
            hidden={hidden}
            action="paste"
            doAction={() => {
              const [directoryKey, directoryId] =
                item.item.type === backendModule.AssetType.directory
                  ? [item.key, item.item.id]
                  : [item.directoryKey, item.directoryId]
              doPaste(directoryKey, directoryId)
            }}
          />
        )}
      </ContextMenu>
      {category === Category.home && (
        <GlobalContextMenu
          hidden={hidden}
          hasCopyData={hasPasteData}
          directoryKey={
            // This is SAFE, as both branches are guaranteed to be `DirectoryId`s
            // eslint-disable-next-line no-restricted-syntax
            (asset.type === backendModule.AssetType.directory
              ? item.key
              : item.directoryKey) as backendModule.DirectoryId
          }
          directoryId={
            asset.type === backendModule.AssetType.directory ? asset.id : item.directoryId
          }
          dispatchAssetListEvent={dispatchAssetListEvent}
          doPaste={doPaste}
        />
      )}
    </ContextMenus>
  )
}
