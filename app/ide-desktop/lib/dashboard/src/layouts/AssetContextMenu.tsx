/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as toast from 'react-toastify'

import * as backendHooks from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

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
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doTriggerDescriptionEdit: () => void
  readonly doPaste: (newParentId: backendModule.DirectoryId) => void
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps) {
  const { innerProps, rootDirectoryId, event, eventTarget, hidden = false } = props
  const { doTriggerDescriptionEdit, doCopy, doCut, doPaste } = props
  const { item, state, setRowState } = innerProps
  const { backend, category, hasPasteData } = state

  const { session } = sessionProvider.useSession()
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const remoteBackend = backendProvider.useRemoteBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const self = item.permissions?.find(
    backendModule.isUserPermissionAnd(permission => permission.user.userId === user?.userId)
  )
  const isCloud = categoryModule.isCloud(category)
  const ownsThisAsset = !isCloud || self?.permission === permissions.PermissionAction.own
  const managesThisAsset = ownsThisAsset || self?.permission === permissions.PermissionAction.admin
  const canEditThisAsset =
    managesThisAsset || self?.permission === permissions.PermissionAction.edit
  const isRunningProject =
    item.type === backendModule.AssetType.project &&
    backendModule.IS_OPENING_OR_OPENED[item.projectState.type]
  const canExecute =
    !isCloud ||
    (self?.permission != null && permissions.PERMISSION_ACTION_CAN_EXECUTE[self.permission])
  const isOtherUserUsingProject =
    isCloud &&
    backendModule.assetIsProject(item) &&
    item.projectState.openedBy != null &&
    item.projectState.openedBy !== user?.email

  const deleteAssetMutation = backendHooks.useBackendMutation(backend, 'deleteAsset')
  const undoDeleteAssetMutation = backendHooks.useBackendMutation(backend, 'undoDeleteAsset')
  const createProjectMutation = backendHooks.useBackendCreateProjectMutation(backend)
  const openProjectMutation = backendHooks.useBackendMutation(backend, 'openProject')
  const closeProjectMutation = backendHooks.useBackendMutation(backend, 'closeProject')

  const doDelete = () => {
    deleteAssetMutation.mutate([item.id, { force: false, parentId: item.parentId }])
  }

  return category === Category.trash ? (
    !ownsThisAsset ? null : (
      <ContextMenus hidden={hidden} key={item.id} event={event}>
        <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden}>
          <ContextMenuEntry
            hidden={hidden}
            action="undelete"
            label={getText('restoreFromTrashShortcut')}
            doAction={() => {
              undoDeleteAssetMutation.mutate([item.id])
            }}
          />
          <ContextMenuEntry
            hidden={hidden}
            action="delete"
            label={getText('deleteForeverShortcut')}
            doAction={() => {
              setModal(
                <ConfirmDeleteModal
                  actionText={`delete the ${item.type} '${item.title}' forever`}
                  doDelete={() => {
                    deleteAssetMutation.mutate([item.id, { force: true, parentId: item.parentId }])
                  }}
                />
              )
            }}
          />
        </ContextMenu>
      </ContextMenus>
    )
  ) : (
    <ContextMenus hidden={hidden} key={item.id} event={event}>
      <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden}>
        {item.type === backendModule.AssetType.datalink && (
          <ContextMenuEntry
            hidden={hidden}
            action="useInNewProject"
            doAction={() => {
              createProjectMutation.mutate([
                { projectName: item.title, parentDirectoryId: item.parentId, datalinkId: item.id },
              ])
            }}
          />
        )}
        {item.type === backendModule.AssetType.project &&
          canExecute &&
          !isRunningProject &&
          !isOtherUserUsingProject && (
            <ContextMenuEntry
              hidden={hidden}
              action="open"
              doAction={() => {
                openProjectMutation.mutate([
                  item.id,
                  { parentId: item.parentId, executeAsync: false, cognitoCredentials: session },
                ])
              }}
            />
          )}
        {item.type === backendModule.AssetType.project && isCloud && (
          <ContextMenuEntry
            hidden={hidden}
            action="run"
            doAction={() => {
              openProjectMutation.mutate([
                item.id,
                { parentId: item.parentId, executeAsync: true, cognitoCredentials: session },
              ])
            }}
          />
        )}
        {item.type === backendModule.AssetType.project &&
          canExecute &&
          isRunningProject &&
          !isOtherUserUsingProject && (
            <ContextMenuEntry
              hidden={hidden}
              action="close"
              doAction={() => {
                closeProjectMutation.mutate([item.id])
              }}
            />
          )}
        {item.type === backendModule.AssetType.project && !isCloud && (
          <ContextMenuEntry
            hidden={hidden}
            action="uploadToCloud"
            doAction={async () => {
              if (remoteBackend == null) {
                toastAndLog('offlineUploadFilesError')
              } else {
                try {
                  const projectResponse = await fetch(
                    `./api/project-manager/projects/${localBackend.extractTypeAndId(item.id).id}/enso-project`
                  )
                  // This DOES NOT update the cloud assets list when it
                  // completes, as the current backend is not the remote
                  // (cloud) backend. The user may change to the cloud backend
                  // while this request is in progress, however this is
                  // uncommon enough that it is not worth the added complexity.
                  await remoteBackend.uploadFile(
                    {
                      fileName: `${item.title}.enso-project`,
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
                ? item.type !== backendModule.AssetType.project &&
                  item.type !== backendModule.AssetType.directory
                : false
            }
            action="rename"
            doAction={() => {
              setRowState(object.merger({ isEditingName: true }))
            }}
          />
        )}
        {item.type === backendModule.AssetType.secret &&
          canEditThisAsset &&
          remoteBackend != null && (
            <ContextMenuEntry
              hidden={hidden}
              action="edit"
              doAction={() => {
                setModal(
                  <UpsertSecretModal backend={backend} asset={item} parentDirectoryId={null} />
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
                doDelete()
              } else {
                setModal(
                  <ConfirmDeleteModal
                    actionText={getText('deleteTheAssetTypeTitle', item.type, item.title)}
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
                  item={item}
                  self={self}
                  eventTarget={eventTarget}
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
                <ManageLabelsModal backend={backend} item={item} eventTarget={eventTarget} />
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
            dispatchAssetListEvent({
              type: AssetListEventType.copy,
              newParentId: item.directoryId,
              items: [item],
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
            item.type !== backendModule.AssetType.file &&
            item.type !== backendModule.AssetType.datalink &&
            item.type !== backendModule.AssetType.project
          }
          action="download"
          doAction={() => {
            dispatchAssetEvent({
              type: AssetEventType.download,
              ids: new Set([item.id]),
            })
          }}
        />
        {hasPasteData && (
          <ContextMenuEntry
            hidden={hidden}
            action="paste"
            doAction={() => {
              const directoryId =
                item.type === backendModule.AssetType.directory ? item.id : item.parentId
              doPaste(directoryId)
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
          directoryId={item.type === backendModule.AssetType.directory ? item.id : item.parentId}
          doPaste={doPaste}
        />
      )}
    </ContextMenus>
  )
}
