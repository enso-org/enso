/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as toast from 'react-toastify'

import { useCopy } from '#/hooks/copyHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
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
import * as localBackendModule from '#/services/LocalBackend'

import { ContextMenuEntry as PaywallContextMenuEntry } from '#/components/Paywall'
import {
  copyAssetsMutationOptions,
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useNewProject } from '#/hooks/backendHooks'
import { useUploadFileWithToastMutation } from '#/hooks/backendUploadFilesHooks'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { usePasteData } from '#/providers/DriveProvider'
import * as featureFlagsProvider from '#/providers/FeatureFlagsProvider'
import { TEAMS_DIRECTORY_ID, USERS_DIRECTORY_ID } from '#/services/remoteBackendPaths'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import { useSetAssetPanelProps, useSetIsAssetPanelTemporarilyVisible } from './AssetPanel'

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

  const getAsset = useGetAsset()
  const canOpenProjects = projectHooks.useCanOpenProjects()
  const { user } = authProvider.useFullUserSession()
  const { setModal } = modalProvider.useSetModal()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()
  const setAssetPanelProps = useSetAssetPanelProps()
  const openProjectNatively = projectHooks.useOpenProjectNatively()
  const openProjectLocally = projectHooks.useOpenProjectLocally()
  const closeProject = projectHooks.useCloseProject()
  const deleteAssetsMutation = reactQuery.useMutation(deleteAssetsMutationOptions(backend))
  const restoreAssetsMutation = reactQuery.useMutation(restoreAssetsMutationOptions(backend))
  const copyAssetsMutation = reactQuery.useMutation(copyAssetsMutationOptions(backend))
  const downloadAssetsMutation = reactQuery.useMutation(downloadAssetsMutationOptions(backend))
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const path = asset.ensoPathValue
  const copyMutation = useCopy()
  const uploadFileToCloudMutation = useUploadFileWithToastMutation(remoteBackend)
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
  const hasPasteData = (pasteData?.data.ids.size ?? 0) > 0
  const [firstPasteDataId] = pasteData?.data.ids ?? []
  const pasteDataParentId = firstPasteDataId != null ? getAsset(firstPasteDataId)?.parentId : null
  const pasteDataParent = pasteDataParentId != null ? getAsset(pasteDataParentId) : null

  const canPaste =
    (
      !pasteDataParent ||
      !pasteData ||
      !isCloud ||
      permissions.isTeamPath(pasteDataParent.virtualParentsPath)
    ) ?
      true
    : Array.from(pasteData.data.ids).every((id) => {
        const otherAsset = getAsset(id)
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

  const { data } = reactQuery.useQuery({
    ...projectHooks.createGetProjectDetailsQuery({
      // This is safe because we disable the query when the asset is not a project.
      // see `enabled` property below.
      // eslint-disable-next-line no-restricted-syntax
      assetId: asset.id as backendModule.ProjectId,
      backend,
    }),
    enabled: asset.type === backendModule.AssetType.project && canOpenProjects,
  })

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
              restoreAssetsMutation.mutate([asset.id])
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
                  doDelete={async () => {
                    await deleteAssetsMutation.mutateAsync([[asset.id], true])
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
        {asset.type === backendModule.AssetType.datalink && (
          <ContextMenuEntry
            hidden={hidden}
            action="useInNewProject"
            doAction={() => {
              void newProject({ templateName: asset.title, datalinkId: asset.id }, asset.parentId)
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
            doAction={() => {
              openProjectNatively(asset, backend.type)
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
                  parentId: asset.parentId,
                  type: state.backend.type,
                })
              }}
            />
          )}
        {asset.type === backendModule.AssetType.project && !isCloud && (
          <PaywallContextMenuEntry
            hidden={hidden}
            isUnderPaywall={!canUploadToCloud}
            feature="uploadToCloud"
            action="uploadToCloud"
            doAction={async () => {
              try {
                // Folder's id matches the pattern `<type>-<Full Path>`, i.e. `directory-/Users/user/enso/folder 1`
                const parentDirectoryPath = localBackendModule.extractTypeAndId(asset.parentId).id

                const projectResponse = await fetch(
                  `./api/project-manager/projects/${localBackendModule.extractTypeAndId(asset.id).id}/enso-project?projectsDirectory=${parentDirectoryPath}`,
                )

                if (!projectResponse.ok) {
                  throw new Error('Something went wrong, please try again')
                }

                const fileName = `${asset.title}.enso-project`
                await uploadFileToCloudMutation.mutateAsync([
                  {
                    fileName,
                    fileId: null,
                    parentDirectoryId: null,
                  },
                  new File([await projectResponse.blob()], fileName),
                ])
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
            asset.type === backendModule.AssetType.directory ||
            asset.type === backendModule.AssetType.secret) && (
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
                      doDelete={async () => {
                        await deleteAssetsMutation.mutateAsync([[asset.id], false])
                      }}
                    />,
                  )
                } else {
                  deleteAssetsMutation.mutate([[asset.id], false])
                }
              } else {
                setModal(
                  <ConfirmDeleteModal
                    defaultOpen
                    actionText={getText('deleteTheAssetTypeTitle', asset.type, asset.title)}
                    doDelete={async () => {
                      await deleteAssetsMutation.mutateAsync([[asset.id], false])
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
            doAction={() => {
              copyAssetsMutation.mutate([[asset.id], asset.parentId])
            }}
          />
        )}
        {isCloud && <ContextMenuEntry hidden={hidden} action="copy" doAction={doCopy} />}
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
              downloadAssetsMutation.mutate([{ id: asset.id, title: asset.title }])
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
