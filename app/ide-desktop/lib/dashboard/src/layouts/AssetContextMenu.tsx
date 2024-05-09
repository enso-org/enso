/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as toast from 'react-toastify'

import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'
import type * as assetRow from '#/components/dashboard/AssetRow'
import Separator from '#/components/styled/Separator'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import * as localBackend from '#/services/LocalBackend'

import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'

// ========================
// === AssetContextMenu ===
// ========================

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps {
  readonly hidden?: boolean
  readonly innerProps: assetRow.AssetRowInnerProps
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
  readonly eventTarget: HTMLElement | null
  readonly doDelete: () => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doTriggerDescriptionEdit: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId
  ) => void
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps) {
  const { innerProps, rootDirectoryId, event, eventTarget, hidden = false } = props
  const { doTriggerDescriptionEdit, doCopy, doCut, doPaste, doDelete } = props
  const { item, setItem, state, setRowState } = innerProps
  const { backend, category, hasPasteData, labels } = state
  const { dispatchAssetEvent, dispatchAssetListEvent, doCreateLabel } = state

  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const remoteBackend = backendProvider.useRemoteBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const asset = item.item
  const self = asset.permissions?.find(permission => permission.user.userId === user?.userId)
  const isCloud = categoryModule.isCloud(category)
  const ownsThisAsset = !isCloud || self?.permission === permissions.PermissionAction.own
  const managesThisAsset = ownsThisAsset || self?.permission === permissions.PermissionAction.admin
  const canEditThisAsset =
    managesThisAsset || self?.permission === permissions.PermissionAction.edit
  const isRunningProject =
    asset.type === backendModule.AssetType.project &&
    backendModule.IS_OPENING_OR_OPENED[asset.projectState.type]
  const canExecute =
    !isCloud ||
    (self?.permission != null && permissions.PERMISSION_ACTION_CAN_EXECUTE[self.permission])
  const isOtherUserUsingProject =
    isCloud &&
    backendModule.assetIsProject(asset) &&
    asset.projectState.openedBy != null &&
    asset.projectState.openedBy !== user?.email
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  return category === Category.trash ? (
    !ownsThisAsset ? null : (
      <ContextMenus hidden={hidden} key={asset.id} event={event}>
        <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden}>
          <ContextMenuEntry
            hidden={hidden}
            action="undelete"
            label={getText('restoreFromTrashShortcut')}
            doAction={() => {
              unsetModal()
              dispatchAssetEvent({ type: AssetEventType.restore, ids: new Set([asset.id]) })
            }}
          />
          <ContextMenuEntry
            hidden={hidden}
            action="delete"
            label={getText('deleteForeverShortcut')}
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
      <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden}>
        {asset.type === backendModule.AssetType.dataLink && (
          <ContextMenuEntry
            hidden={hidden}
            action="useInNewProject"
            doAction={() => {
              unsetModal()
              dispatchAssetListEvent({
                type: AssetListEventType.newProject,
                parentId: item.directoryId,
                parentKey: item.directoryKey,
                templateId: null,
                datalinkId: asset.id,
                preferredName: asset.title,
              })
            }}
          />
        )}
        {asset.type === backendModule.AssetType.project &&
          canExecute &&
          !isRunningProject &&
          !isOtherUserUsingProject && (
            <ContextMenuEntry
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
          <ContextMenuEntry
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
            <ContextMenuEntry
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
          <ContextMenuEntry
            hidden={hidden}
            action="uploadToCloud"
            doAction={async () => {
              unsetModal()
              if (remoteBackend == null) {
                toastAndLog('offlineUploadFilesError')
              } else {
                try {
                  const projectResponse = await fetch(
                    `./api/project-manager/projects/${localBackend.extractTypeAndId(asset.id).id}/enso-project`
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
                  toast.toast.success(getText('uploadProjectToCloudSuccess'))
                } catch (error) {
                  toastAndLog('uploadProjectToCloudError', error)
                }
              }
            }}
          />
        )}
        {canExecute && !isRunningProject && !isOtherUserUsingProject && (
          <ContextMenuEntry
            hidden={hidden}
            isDisabled={
              isCloud
                ? asset.type !== backendModule.AssetType.project &&
                  asset.type !== backendModule.AssetType.directory
                : false
            }
            action="rename"
            doAction={() => {
              setRowState(object.merger({ isEditingName: true }))
              unsetModal()
            }}
          />
        )}
        {asset.type === backendModule.AssetType.secret &&
          canEditThisAsset &&
          remoteBackend != null && (
            <ContextMenuEntry
              hidden={hidden}
              action="edit"
              doAction={() => {
                setModal(
                  <UpsertSecretModal
                    id={asset.id}
                    name={asset.title}
                    doCreate={async (_name, value) => {
                      try {
                        await remoteBackend.updateSecret(asset.id, { value }, asset.title)
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
          <ContextMenuEntry
            hidden={hidden}
            action="editDescription"
            label={getText('editDescriptionShortcut')}
            doAction={() => {
              doTriggerDescriptionEdit()
            }}
          />
        )}
        {isCloud && (
          <ContextMenuEntry
            hidden={hidden}
            isDisabled
            action="snapshot"
            doAction={() => {
              // No backend support yet.
            }}
          />
        )}
        {ownsThisAsset && !isRunningProject && !isOtherUserUsingProject && (
          <ContextMenuEntry
            hidden={hidden}
            action="delete"
            label={isCloud ? getText('moveToTrashShortcut') : getText('deleteShortcut')}
            doAction={() => {
              if (isCloud) {
                unsetModal()
                doDelete()
              } else {
                setModal(
                  <ConfirmDeleteModal
                    actionText={getText('deleteTheAssetTypeTitle', asset.type, asset.title)}
                    doDelete={doDelete}
                  />
                )
              }
            }}
          />
        )}
        {isCloud && <Separator hidden={hidden} />}
        {isCloud && managesThisAsset && self != null && (
          <ContextMenuEntry
            hidden={hidden}
            action="share"
            doAction={() => {
              setModal(
                <ManagePermissionsModal
                  backend={backend}
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
          <ContextMenuEntry
            hidden={hidden}
            action="label"
            doAction={() => {
              setModal(
                <ManageLabelsModal
                  backend={backend}
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
        {isCloud && managesThisAsset && self != null && <Separator hidden={hidden} />}
        <ContextMenuEntry
          hidden={hidden}
          isDisabled={!isCloud}
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
        {isCloud && <ContextMenuEntry hidden={hidden} action="copy" doAction={doCopy} />}
        {!isOtherUserUsingProject && (
          <ContextMenuEntry hidden={hidden} action="cut" doAction={doCut} />
        )}
        <ContextMenuEntry
          hidden={hidden}
          isDisabled={
            isCloud &&
            asset.type !== backendModule.AssetType.file &&
            asset.type !== backendModule.AssetType.dataLink &&
            asset.type !== backendModule.AssetType.project
          }
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
          <ContextMenuEntry
            hidden={hidden}
            action="paste"
            doAction={() => {
              const [directoryKey, directoryId] =
                item.type === backendModule.AssetType.directory
                  ? [item.key, item.item.id]
                  : [item.directoryKey, item.directoryId]
              doPaste(directoryKey, directoryId)
            }}
          />
        )}
      </ContextMenu>
      {(category === Category.cloud || category === Category.local) && (
        <GlobalContextMenu
          hidden={hidden}
          backend={backend}
          hasPasteData={hasPasteData}
          rootDirectoryId={rootDirectoryId}
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
