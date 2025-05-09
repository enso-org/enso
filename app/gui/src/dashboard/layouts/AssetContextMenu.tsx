/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import { useCopy } from '#/hooks/copyHooks'
import * as projectHooks from '#/hooks/projectHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import { GlobalContextMenu } from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import type * as assetRow from '#/components/dashboard/AssetRow'
import Separator from '#/components/styled/Separator'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ManageLabelsModal from '#/modals/ManageLabelsModal'

import * as backendModule from '#/services/Backend'

import { ContextMenuEntry as PaywallContextMenuEntry } from '#/components/Paywall'
import {
  copyAssetsMutationOptions,
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useNewProject } from '#/hooks/backendHooks'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { usePasteData } from '#/providers/DriveProvider'
import * as featureFlagsProvider from '#/providers/FeatureFlagsProvider'
import { setModal } from '#/providers/ModalProvider'
import { TEAMS_DIRECTORY_ID, USERS_DIRECTORY_ID } from '#/services/remoteBackendPaths'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import {
  isUploadableAsset,
  useUploadFileToCloudMutation,
  useUploadFileToLocal,
} from '../hooks/backendUploadFilesHooks'
import { useSetAssetPanelProps, useSetIsAssetPanelTemporarilyVisible } from './AssetPanel'
import { useCategories } from './Drive/Categories'

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps {
  readonly hidden?: boolean
  readonly innerProps: assetRow.AssetRowInnerProps
  readonly triggerRef: React.MutableRefObject<HTMLElement | null>
  readonly currentDirectoryId: backendModule.DirectoryId
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
  readonly eventTarget: HTMLElement | null
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
  ) => void
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps) {
  const { innerProps, event, hidden = false, triggerRef, currentDirectoryId } = props
  const { doCopy, doCut, doPaste } = props
  const { asset, state, setRowState } = innerProps
  const { backend, category } = state

  const isCloud = categoryModule.isCloudCategory(category)

  const { localCategories } = useCategories()

  const getAsset = useGetAsset()
  const canOpenProjects = projectHooks.useCanOpenProjects()
  const { user } = authProvider.useFullUserSession()
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()
  const setAssetPanelProps = useSetAssetPanelProps()
  const openProjectNatively = projectHooks.useOpenProjectNatively()
  const openProjectLocally = projectHooks.useOpenProjectLocally()
  const closeProject = projectHooks.useCloseProject()
  const deleteAssetsMutation = useMutationCallback(deleteAssetsMutationOptions(backend))
  const restoreAssetsMutation = useMutationCallback(restoreAssetsMutationOptions(backend))
  const copyAssetsMutation = useMutationCallback(copyAssetsMutationOptions(backend))
  const downloadAssetsMutation = useMutationCallback(downloadAssetsMutationOptions(backend))
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const path = asset.ensoPathValue
  const copyMutation = useCopy()
  const uploadFileToCloudMutation = useUploadFileToCloudMutation()
  const uploadFileToLocal = useUploadFileToLocal(category)
  const disabledTooltip = !canOpenProjects ? getText('downloadToOpenWorkflow') : undefined
  const showDeveloperIds = featureFlagsProvider.useFeatureFlag('showDeveloperIds')

  const newProject = useNewProject(backend, category)

  const systemApi = window.systemApi
  const ownsThisAsset = !isCloud || self?.permission === permissions.PermissionAction.own
  const canManageThisAsset = asset.id !== USERS_DIRECTORY_ID && asset.id !== TEAMS_DIRECTORY_ID
  const managesThisAsset = ownsThisAsset || self?.permission === permissions.PermissionAction.admin
  const canEditThisAsset =
    managesThisAsset || self?.permission === permissions.PermissionAction.edit
  const canAddToThisDirectory =
    category.type !== 'recent' &&
    asset.type === backendModule.AssetType.directory &&
    canEditThisAsset

  const pasteData = usePasteData()
  const hasPasteData = (pasteData?.data.assets.length ?? 0) > 0
  const [firstPasteDataId] = pasteData?.data.assets ?? []
  const pasteDataParentId =
    firstPasteDataId != null ? getAsset(firstPasteDataId.id)?.parentId : null
  const pasteDataParent = pasteDataParentId != null ? getAsset(pasteDataParentId) : null

  const canPaste =
    (
      !pasteDataParent ||
      !pasteData ||
      !isCloud ||
      permissions.isTeamPath(pasteDataParent.virtualParentsPath)
    ) ?
      true
    : pasteData.data.assets.every((pasteAsset) => {
        const otherAsset = getAsset(pasteAsset.id)
        if (!otherAsset) {
          return false
        }
        // Assume user path; check permissions
        const permission = permissions.tryFindSelfPermission(user, otherAsset.permissions)
        return (
          permission != null &&
          permissions.canPermissionModifyDirectoryContents(permission.permission)
        )
      })

  const isRunningProject =
    asset.type === backendModule.AssetType.project &&
    backendModule.IS_OPENING_OR_OPENED[asset.projectState.type]

  const canExecute =
    category.type !== 'trash' &&
    (!isCloud || (self != null && permissions.PERMISSION_ACTION_CAN_EXECUTE[self.permission]))

  const isOtherUserUsingProject =
    isCloud &&
    backendModule.assetIsProject(asset) &&
    asset.projectState.openedBy != null &&
    asset.projectState.openedBy !== user.email

  const enableHybridExecution = featureFlagsProvider.useFeatureFlag('enableHybridExecution')

  const pasteMenuEntry = hasPasteData && canPaste && (
    <ContextMenuEntry
      hidden={hidden}
      action="paste"
      doAction={() => {
        const directoryId =
          asset.type === backendModule.AssetType.directory ? asset.id : asset.parentId

        doPaste(directoryId, directoryId)
      }}
    />
  )

  const canUploadToCloud = user.plan !== backendModule.Plan.free

  const copyIdEntry = showDeveloperIds && (
    <ContextMenuEntry
      hidden={hidden}
      color="accent"
      action="copyId"
      doAction={() => copyMutation.mutateAsync(asset.id)}
    />
  )

  return (
    category.type === 'trash' ?
      !ownsThisAsset ? null
      : <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden} event={event}>
          {copyIdEntry}

          <ContextMenuEntry
            hidden={hidden}
            action="undelete"
            label={getText('restoreFromTrashShortcut')}
            doAction={() => {
              void restoreAssetsMutation({
                ids: [asset.id],
                parentId: null,
              })
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
                  cannotUndo
                  actionText={getText('deleteTheAssetTypeTitleForever', asset.type, asset.title)}
                  onConfirm={async () => {
                    await deleteAssetsMutation([[asset.id], true])
                  }}
                />,
              )
            }}
          />
          {pasteMenuEntry}
        </ContextMenu>
    : !canManageThisAsset ? null
    : <ContextMenu aria-label={getText('assetContextMenuLabel')} hidden={hidden} event={event}>
        {copyIdEntry}
        {(asset.type === backendModule.AssetType.datalink ||
          asset.type === backendModule.AssetType.file) && (
          <ContextMenuEntry
            hidden={hidden}
            action="useInNewProject"
            doAction={() => {
              void newProject(
                { templateName: asset.title, ensoPath: asset.ensoPath },
                asset.parentId,
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
              isDisabled={!canOpenProjects}
              tooltip={disabledTooltip}
              doAction={() => openProjectLocally(asset, backend.type)}
            />
          )}
        {asset.type === backendModule.AssetType.project && isCloud && enableHybridExecution && (
          <ContextMenuEntry
            hidden={hidden || localBackend == null}
            action="run"
            isDisabled={!canOpenProjects}
            tooltip={disabledTooltip}
            doAction={() => openProjectNatively(asset, backend.type)}
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
                void closeProject({
                  id: asset.id,
                  title: asset.title,
                  parentId: asset.parentId,
                  type: state.backend.type,
                })
              }}
            />
          )}
        {isUploadableAsset(asset) && !isCloud && localBackend != null && (
          <PaywallContextMenuEntry
            hidden={hidden}
            isUnderPaywall={!canUploadToCloud}
            feature="uploadToCloud"
            action="uploadToCloud"
            doAction={() =>
              uploadFileToCloudMutation(localBackend, {
                assets: [asset],
                targetDirectoryId: user.rootDirectoryId,
              })
            }
          />
        )}
        {isUploadableAsset(asset) && isCloud && localBackend != null && (
          <ContextMenuEntry
            hidden={hidden}
            action="downloadToLocal"
            doAction={() => uploadFileToLocal([asset])}
          />
        )}
        {canExecute && !isRunningProject && !isOtherUserUsingProject && (
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
                const assetPanelProps = { backend, item: asset }
                switch (asset.type) {
                  case backendModule.AssetType.secret: {
                    setAssetPanelProps({
                      ...assetPanelProps,
                      spotlightOn: 'secret',
                    })
                    break
                  }
                  case backendModule.AssetType.datalink: {
                    setAssetPanelProps({
                      ...assetPanelProps,
                      spotlightOn: 'datalink',
                    })
                    break
                  }
                }
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
                      onConfirm={async () => {
                        await deleteAssetsMutation([[asset.id], false])
                      }}
                    />,
                  )
                } else {
                  void deleteAssetsMutation([[asset.id], false])
                }
              } else {
                setModal(
                  <ConfirmDeleteModal
                    defaultOpen
                    actionText={getText('deleteTheAssetTypeTitle', asset.type, asset.title)}
                    onConfirm={async () => {
                      await deleteAssetsMutation([[asset.id], false])
                    }}
                  />,
                )
              }
            }}
          />
        )}
        {isCloud && <Separator hidden={hidden} />}

        {isCloud && (
          <ContextMenuEntry
            hidden={hidden}
            action="label"
            doAction={() => {
              setModal(<ManageLabelsModal backend={backend} item={asset} triggerRef={triggerRef} />)
            }}
          />
        )}
        {isCloud && managesThisAsset && self != null && <Separator hidden={hidden} />}
        {asset.type === backendModule.AssetType.project && (
          <ContextMenuEntry
            hidden={hidden}
            action="duplicate"
            doAction={async () => {
              await copyAssetsMutation([[asset.id], asset.parentId])
            }}
          />
        )}
        {<ContextMenuEntry hidden={hidden} action="copy" doAction={doCopy} />}
        {path != null && (
          <ContextMenuEntry
            hidden={hidden}
            action="copyAsPath"
            doAction={() => copyMutation.mutateAsync(path)}
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
              void downloadAssetsMutation({
                ids: [{ id: asset.id, title: asset.title }],
                targetDirectoryId:
                  !isCloud ? (localCategories.localCategory?.homeDirectoryId ?? null) : null,
                shouldUnpackProject: false,
              })
            }}
          />
        )}
        {pasteMenuEntry}
        {canAddToThisDirectory && <Separator hidden={hidden} />}
        {canAddToThisDirectory && (
          <GlobalContextMenu
            noWrapper
            hidden={hidden}
            backend={backend}
            category={category}
            currentDirectoryId={currentDirectoryId}
            directoryId={asset.id}
            doPaste={doPaste}
            event={event}
          />
        )}
      </ContextMenu>
  )
}
