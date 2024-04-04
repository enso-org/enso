/** @file The context menu for an arbitrary {@link backendModule.AnyAsset}. */
import * as React from 'react'

import * as toast from 'react-toastify'

import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as loggerProvider from '#/providers/LoggerProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import Category from '#/layouts/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'
import ContextMenuSeparator from '#/components/ContextMenuSeparator'
import type * as assetRow from '#/components/dashboard/AssetRow'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import * as localBackend from '#/services/LocalBackend'
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
  readonly isCloud: boolean
  readonly innerProps: assetRow.AssetRowInnerProps
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
  readonly eventTarget: HTMLElement | null
  readonly doDelete: () => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (newParentKey: backendModule.AssetId) => void
  readonly doTriggerDescriptionEdit: () => void
}

/** The context menu for an arbitrary {@link backendModule.AnyAsset}. */
export default function AssetContextMenu(props: AssetContextMenuProps) {
  const { hidden = false, isCloud, innerProps, event, eventTarget } = props
  const { doCopy, doCut, doPaste, doDelete, doTriggerDescriptionEdit } = props
  const { item, setItem, state, setRowState } = innerProps
  const { category, hasPasteData, labels, dispatchAssetEvent, dispatchAssetListEvent } = state
  const { doCreateLabel } = state

  const logger = loggerProvider.useLogger()
  const { user, accessToken } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const smartAsset = item.item
  const asset = smartAsset.value
  const self = asset.permissions?.find(permission => permission.user.userId === user?.value.userId)
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
    asset.projectState.opened_by != null &&
    asset.projectState.opened_by !== user?.value.email
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  return category === Category.trash ? (
    !ownsThisAsset ? null : (
      <ContextMenus hidden={hidden} key={asset.id} event={event}>
        <ContextMenu hidden={hidden}>
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
      <ContextMenu hidden={hidden}>
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
              if (accessToken == null) {
                toastAndLog('offlineUploadFilesError')
              } else {
                try {
                  const client = new HttpClient(
                    [['Authorization', `Bearer ${accessToken}`]],
                    `${process.env.ENSO_CLOUD_API_URL}/`
                  )
                  const remoteBackend = new RemoteBackend(client, logger, getText)
                  const projectResponse = await fetch(
                    `./api/project-manager/projects/${localBackend.extractTypeAndId(asset.id).id}/enso-project`
                  )
                  const fileName = `${asset.title}.enso-project`
                  // This DOES NOT update the cloud assets list when it
                  // completes, as the current backend is not the remote
                  // (cloud) backend. The user may change to the cloud backend
                  // while this request is in progress, however this is
                  // uncommon enough that it is not worth the added complexity.
                  await remoteBackend.uploadFile(
                    {
                      fileName,
                      fileId: null,
                      parentDirectoryId: null,
                    },
                    new File([await projectResponse.blob()], fileName)
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
        {smartAsset.type === backendModule.AssetType.secret && canEditThisAsset && (
          <ContextMenuEntry
            hidden={hidden}
            action="edit"
            doAction={() => {
              setModal(
                <UpsertSecretModal
                  id={smartAsset.value.id}
                  name={asset.title}
                  doCreate={async (_name, value) => {
                    try {
                      await smartAsset.update({ value })
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
            disabled
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
        {isCloud && <ContextMenuSeparator hidden={hidden} />}
        {isCloud && managesThisAsset && self != null && (
          <ContextMenuEntry
            hidden={hidden}
            action="share"
            doAction={() => {
              setModal(
                <ManagePermissionsModal
                  item={smartAsset}
                  setItem={setAsset}
                  self={self}
                  eventTarget={eventTarget}
                  doRemoveSelf={() => {
                    dispatchAssetEvent({ type: AssetEventType.removeSelf, id: asset.id })
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
                  item={smartAsset}
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
        <ContextMenuEntry
          hidden={hidden}
          disabled={!isCloud}
          action="duplicate"
          doAction={() => {
            unsetModal()
            dispatchAssetListEvent({
              type: AssetListEventType.copy,
              newParentKey: item.directoryKey,
              newParent: item.directory,
              items: [smartAsset],
            })
          }}
        />
        {isCloud && <ContextMenuEntry hidden={hidden} action="copy" doAction={doCopy} />}
        {!isOtherUserUsingProject && (
          <ContextMenuEntry hidden={hidden} action="cut" doAction={doCut} />
        )}
        <ContextMenuEntry
          hidden={hidden}
          disabled={
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
              const directoryKey =
                item.item.type === backendModule.AssetType.directory ? item.key : item.directoryKey
              doPaste(directoryKey)
            }}
          />
        )}
      </ContextMenu>
      {category === Category.home && (
        <GlobalContextMenu
          hidden={hidden}
          isCloud={isCloud}
          hasPasteData={hasPasteData}
          directoryKey={
            // This is SAFE, as both branches are guaranteed to be `DirectoryId`s
            // eslint-disable-next-line no-restricted-syntax
            (asset.type === backendModule.AssetType.directory
              ? item.key
              : item.directoryKey) as backendModule.DirectoryId
          }
          directory={
            smartAsset.type === backendModule.AssetType.directory ? smartAsset : item.directory
          }
          dispatchAssetListEvent={dispatchAssetListEvent}
          doPaste={doPaste}
        />
      )}
    </ContextMenus>
  )
}
