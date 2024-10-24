/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as toast from 'react-toastify'

import * as billingHooks from '#/hooks/billing'
import * as copyHooks from '#/hooks/copyHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'
import type * as assetRow from '#/components/dashboard/AssetRow'
import * as paywall from '#/components/Paywall'
import Separator from '#/components/styled/Separator'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'

import { useNewProject, useUploadFileWithToastMutation } from '#/hooks/backendHooks'
import {
  usePasteData,
  useSetAssetPanelProps,
  useSetIsAssetPanelTemporarilyVisible,
} from '#/providers/DriveProvider'
import { normalizePath } from '#/utilities/fileInfo'
import { mapNonNullish } from '#/utilities/nullable'
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
  readonly triggerRef: React.MutableRefObject<HTMLElement | null>
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
  readonly eventTarget: HTMLElement | null
  readonly doDelete: () => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
  ) => void
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps) {
  const { innerProps, rootDirectoryId, event, eventTarget, hidden = false, triggerRef } = props
  const { doCopy, doCut, doPaste, doDelete } = props
  const { item, setItem, state, setRowState } = innerProps
  const { backend, category, nodeMap } = state

  const { user } = authProvider.useFullUserSession()
  const { setModal } = modalProvider.useSetModal()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()
  const setAssetPanelProps = useSetAssetPanelProps()
  const openProject = projectHooks.useOpenProject()
  const closeProject = projectHooks.useCloseProject()
  const openProjectMutation = projectHooks.useOpenProjectMutation()
  const asset = item.item
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const isCloud = categoryModule.isCloudCategory(category)
  const pathRaw =
    category.type === 'recent' || category.type === 'trash' ? null
    : isCloud ? `${item.path}${item.type === backendModule.AssetType.datalink ? '.datalink' : ''}`
    : asset.type === backendModule.AssetType.project ?
      mapNonNullish(localBackend?.getProjectPath(asset.id) ?? null, normalizePath)
    : normalizePath(localBackendModule.extractTypeAndId(asset.id).id)
  const path =
    pathRaw == null ? null
    : isCloud ? encodeURI(pathRaw)
    : pathRaw
  const copyMutation = copyHooks.useCopy({ copyText: path ?? '' })
  const uploadFileToCloudMutation = useUploadFileWithToastMutation(remoteBackend)

  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })
  const isUnderPaywall = isFeatureUnderPaywall('share')

  const newProject = useNewProject(backend, category)

  const systemApi = window.systemApi
  const ownsThisAsset = !isCloud || self?.permission === permissions.PermissionAction.own
  const managesThisAsset = ownsThisAsset || self?.permission === permissions.PermissionAction.admin
  const canEditThisAsset =
    managesThisAsset || self?.permission === permissions.PermissionAction.edit
  const canAddToThisDirectory =
    category.type !== 'recent' &&
    asset.type === backendModule.AssetType.directory &&
    canEditThisAsset
  const pasteData = usePasteData()
  const hasPasteData = (pasteData?.data.ids.size ?? 0) > 0
  const pasteDataParentKeys =
    !pasteData ? null : (
      new Map(
        Array.from(nodeMap.current.entries()).map(([id, otherAsset]) => [
          id,
          otherAsset.directoryKey,
        ]),
      )
    )
  const canPaste =
    !pasteData || !pasteDataParentKeys || !isCloud ?
      true
    : Array.from(pasteData.data.ids).every((key) => {
        const parentKey = pasteDataParentKeys.get(key)
        const parent = parentKey == null ? null : nodeMap.current.get(parentKey)
        if (!parent) {
          return false
        } else if (permissions.isTeamPath(parent.path)) {
          return true
        } else {
          // Assume user path; check permissions
          const permission = permissions.tryFindSelfPermission(user, item.item.permissions)
          return (
            permission != null &&
            permissions.canPermissionModifyDirectoryContents(permission.permission)
          )
        }
      })

  const { data } = reactQuery.useQuery(
    item.item.type === backendModule.AssetType.project ?
      projectHooks.createGetProjectDetailsQuery.createPassiveListener(item.item.id)
    : { queryKey: ['__IGNORED__'] },
  )

  const isRunningProject =
    (asset.type === backendModule.AssetType.project &&
      data &&
      backendModule.IS_OPENING_OR_OPENED[data.state.type]) ??
    false

  const canExecute =
    category.type !== 'trash' &&
    (!isCloud || (self != null && permissions.PERMISSION_ACTION_CAN_EXECUTE[self.permission]))

  const isOtherUserUsingProject =
    isCloud &&
    backendModule.assetIsProject(asset) &&
    asset.projectState.openedBy != null &&
    asset.projectState.openedBy !== user.email

  const pasteMenuEntry = hasPasteData && canPaste && (
    <ContextMenuEntry
      hidden={hidden}
      action="paste"
      doAction={() => {
        const [directoryKey, directoryId] =
          item.type === backendModule.AssetType.directory ?
            [item.key, item.item.id]
          : [item.directoryKey, item.directoryId]
        doPaste(directoryKey, directoryId)
      }}
    />
  )

  return (
    category.type === 'trash' ?
      !ownsThisAsset ? null
      : <ContextMenus hidden={hidden} key={asset.id} event={event}>
          <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden}>
            <ContextMenuEntry
              hidden={hidden}
              action="undelete"
              label={getText('restoreFromTrashShortcut')}
              doAction={() => {
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
                    defaultOpen
                    actionText={`delete the ${asset.type} '${asset.title}' forever`}
                    doDelete={() => {
                      const ids = new Set([asset.id])
                      dispatchAssetEvent({ type: AssetEventType.deleteForever, ids })
                    }}
                  />,
                )
              }}
            />
            {pasteMenuEntry}
          </ContextMenu>
        </ContextMenus>
    : <ContextMenus hidden={hidden} key={asset.id} event={event}>
        <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden}>
          {asset.type === backendModule.AssetType.datalink && (
            <ContextMenuEntry
              hidden={hidden}
              action="useInNewProject"
              doAction={() => {
                void newProject(
                  { templateName: asset.title, datalinkId: asset.id },
                  item.directoryId,
                  item.path,
                )
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
                  openProject({
                    id: asset.id,
                    title: asset.title,
                    parentId: item.directoryId,
                    type: state.backend.type,
                  })
                }}
              />
            )}
          {asset.type === backendModule.AssetType.project && isCloud && (
            <ContextMenuEntry
              hidden={hidden}
              action="run"
              doAction={() => {
                openProjectMutation.mutate({
                  id: asset.id,
                  title: asset.title,
                  parentId: item.directoryId,
                  type: state.backend.type,
                  inBackground: true,
                })
              }}
            />
          )}
          {!isCloud && path != null && systemApi && (
            <ContextMenuEntry
              hidden={hidden}
              action="openInFileBrowser"
              doAction={() => {
                systemApi.showItemInFolder(path)
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
                  closeProject({
                    id: asset.id,
                    title: asset.title,
                    parentId: item.directoryId,
                    type: state.backend.type,
                  })
                }}
              />
            )}
          {asset.type === backendModule.AssetType.project && !isCloud && (
            <ContextMenuEntry
              hidden={hidden}
              action="uploadToCloud"
              doAction={async () => {
                try {
                  const projectResponse = await fetch(
                    `./api/project-manager/projects/${localBackendModule.extractTypeAndId(asset.id).id}/enso-project`,
                  )
                  // This DOES NOT update the cloud assets list when it
                  // completes, as the current backend is not the remote
                  // (cloud) backend. The user may change to the cloud backend
                  // while this request is in progress, however this is
                  // uncommon enough that it is not worth the added complexity.
                  const fileName = `${asset.title}.enso-project`
                  await uploadFileToCloudMutation.mutateAsync(
                    {
                      fileName,
                      fileId: null,
                      parentDirectoryId: null,
                    },
                    new File([await projectResponse.blob()], fileName),
                  )
                  toast.toast.success(getText('uploadProjectToCloudSuccess'))
                } catch (error) {
                  toastAndLog('uploadProjectToCloudError', error)
                }
              }}
            />
          )}
          {canExecute &&
            !isRunningProject &&
            !isOtherUserUsingProject &&
            (!isCloud ||
              asset.type === backendModule.AssetType.project ||
              asset.type === backendModule.AssetType.directory) && (
              <ContextMenuEntry
                hidden={hidden}
                action="rename"
                doAction={() => {
                  setRowState(object.merger({ isEditingName: true }))
                }}
              />
            )}
          {(asset.type === backendModule.AssetType.secret ||
            asset.type === backendModule.AssetType.datalink) &&
            canEditThisAsset && (
              <ContextMenuEntry
                hidden={hidden}
                action="edit"
                doAction={() => {
                  setIsAssetPanelTemporarilyVisible(true)
                  const assetPanelProps = { backend, item, setItem }
                  switch (asset.type) {
                    case backendModule.AssetType.secret: {
                      setAssetPanelProps({ ...assetPanelProps, spotlightOn: 'secret' })
                      break
                    }
                    case backendModule.AssetType.datalink: {
                      setAssetPanelProps({ ...assetPanelProps, spotlightOn: 'datalink' })
                      break
                    }
                  }
                }}
              />
            )}
          {isCloud && (
            <ContextMenuEntry
              hidden={hidden}
              action="editDescription"
              label={getText('editDescriptionShortcut')}
              doAction={() => {
                setIsAssetPanelTemporarilyVisible(true)
                setAssetPanelProps({ backend, item, spotlightOn: 'description' })
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
                  if (asset.type === backendModule.AssetType.directory) {
                    setModal(
                      <ConfirmDeleteModal
                        defaultOpen
                        actionText={getText('trashTheAssetTypeTitle', asset.type, asset.title)}
                        doDelete={doDelete}
                      />,
                    )
                  } else {
                    doDelete()
                  }
                } else {
                  setModal(
                    <ConfirmDeleteModal
                      defaultOpen
                      actionText={getText('deleteTheAssetTypeTitle', asset.type, asset.title)}
                      doDelete={doDelete}
                    />,
                  )
                }
              }}
            />
          )}
          {isCloud && <Separator hidden={hidden} />}

          {isCloud && managesThisAsset && self != null && (
            <>
              {isUnderPaywall && (
                <paywall.ContextMenuEntry feature="share" action="share" hidden={hidden} />
              )}

              {!isUnderPaywall && (
                <ContextMenuEntry
                  hidden={hidden}
                  action="share"
                  doAction={() => {
                    setModal(
                      <ManagePermissionsModal
                        backend={backend}
                        category={category}
                        item={asset}
                        self={self}
                        eventTarget={eventTarget}
                        doRemoveSelf={() => {
                          dispatchAssetEvent({
                            type: AssetEventType.removeSelf,
                            id: asset.id,
                          })
                        }}
                      />,
                    )
                  }}
                />
              )}
            </>
          )}

          {isCloud && (
            <ContextMenuEntry
              hidden={hidden}
              action="label"
              doAction={() => {
                setModal(
                  <ManageLabelsModal backend={backend} item={asset} triggerRef={triggerRef} />,
                )
              }}
            />
          )}
          {isCloud && managesThisAsset && self != null && <Separator hidden={hidden} />}
          {asset.type === backendModule.AssetType.project && (
            <ContextMenuEntry
              hidden={hidden}
              action="duplicate"
              doAction={() => {
                dispatchAssetListEvent({
                  type: AssetListEventType.copy,
                  newParentId: item.directoryId,
                  newParentKey: item.directoryKey,
                  items: [asset],
                })
              }}
            />
          )}
          {isCloud && <ContextMenuEntry hidden={hidden} action="copy" doAction={doCopy} />}
          {path != null && (
            <ContextMenuEntry
              hidden={hidden}
              action="copyAsPath"
              doAction={copyMutation.mutateAsync}
            />
          )}
          {!isRunningProject && !isOtherUserUsingProject && (
            <ContextMenuEntry hidden={hidden} action="cut" doAction={doCut} />
          )}
          {(isCloud ?
            asset.type !== backendModule.AssetType.directory
          : asset.type === backendModule.AssetType.project) && (
            <ContextMenuEntry
              hidden={hidden}
              isDisabled={asset.type === backendModule.AssetType.secret}
              action="download"
              doAction={() => {
                dispatchAssetEvent({ type: AssetEventType.download, ids: new Set([asset.id]) })
              }}
            />
          )}
          {pasteMenuEntry}
        </ContextMenu>
        {canAddToThisDirectory && (
          <GlobalContextMenu
            hidden={hidden}
            backend={backend}
            category={category}
            rootDirectoryId={rootDirectoryId}
            directoryKey={
              // This is SAFE, as both branches are guaranteed to be `DirectoryId`s
              // eslint-disable-next-line no-restricted-syntax
              item.key as backendModule.DirectoryId
            }
            directoryId={asset.id}
            path={item.path}
            doPaste={doPaste}
          />
        )}
      </ContextMenus>
  )
}
