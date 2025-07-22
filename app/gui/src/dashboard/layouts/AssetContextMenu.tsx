/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import type { ContextMenuApi } from '#/components/ContextMenu'
import { ContextMenu } from '#/components/ContextMenu'
import {
  copyAssetsMutationOptions,
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useCanRunProjects, useNewProject } from '#/hooks/backendHooks'
import {
  isUploadableAsset,
  useUploadFileToCloudMutation,
  useUploadFileToLocal,
} from '#/hooks/backendUploadFilesHooks'
import { useCopy } from '#/hooks/copyHooks'
import { defineMenuEntry, useMenuEntries } from '#/hooks/menuHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategories } from '#/layouts/Drive/Categories'
import { useGlobalContextMenuEntries } from '#/layouts/useGlobalContextMenuEntries'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import type * as assetRow from '#/pages/dashboard/components/AssetRow'
import { useExportArchive } from '#/pages/useExportArchive'
import { usePasteData } from '#/providers/DriveProvider'
import { setModal } from '#/providers/ModalProvider'
import * as backendModule from '#/services/Backend'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import * as authProvider from '$/providers/react'
import { useBackends, useText } from '$/providers/react'
import * as featureFlagsProvider from '$/providers/react/featureFlags'
import type { RightPanelData } from '$/providers/rightPanel'
import {
  TEAMS_DIRECTORY_ID,
  USERS_DIRECTORY_ID,
} from 'enso-common/src/services/Backend/remoteBackendPaths'
import * as React from 'react'

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps {
  readonly innerProps: assetRow.AssetRowInnerProps
  readonly triggerRef: React.MutableRefObject<HTMLElement | null>
  readonly currentDirectoryId: backendModule.DirectoryId
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
  ) => void
  readonly rightPanel: RightPanelData
  readonly initialPosition?: Pick<MouseEvent, 'pageX' | 'pageY'> | null | undefined
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export const AssetContextMenu = React.forwardRef(function AssetContextMenu(
  props: AssetContextMenuProps,
  ref: React.ForwardedRef<ContextMenuApi>,
) {
  const { innerProps, triggerRef, currentDirectoryId, rightPanel, initialPosition } = props
  const { doCopy, doCut, doPaste } = props
  const { asset, state, setRowState } = innerProps
  const { backend, category } = state

  const isCloud = categoryModule.isCloudCategory(category)

  const { localCategories } = useCategories()

  const getAsset = useGetAsset()
  const canRunProjects = useCanRunProjects()
  const { user } = authProvider.useFullUserSession()
  const { localBackend } = useBackends()
  const { getText } = useText()
  const openProjectNatively = projectHooks.useOpenProjectNatively()
  const openProjectLocally = projectHooks.useOpenProjectLocally()
  const closeProject = projectHooks.useCloseProject()
  const deleteAssetsMutation = useMutationCallback(deleteAssetsMutationOptions(backend))
  const restoreAssetsMutation = useMutationCallback(restoreAssetsMutationOptions(backend))
  const copyAssetsMutation = useMutationCallback(copyAssetsMutationOptions(backend))
  const downloadAssetsMutation = useMutationCallback(downloadAssetsMutationOptions(backend))
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const encodedEnsoPath = asset.ensoPath ? encodeURI(asset.ensoPath) : undefined
  const copyMutation = useCopy()
  const uploadFileToCloudMutation = useUploadFileToCloudMutation()
  const uploadFileToLocal = useUploadFileToLocal(category)
  const exportArchive = useExportArchive({ backend })
  const disabledTooltip =
    !canRunProjects.locally[backend.type] ? getText('downloadToOpenWorkflow') : undefined
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

  const globalContextMenuEntries = useGlobalContextMenuEntries({
    backend,
    category,
    currentDirectoryId,
    directoryId: canAddToThisDirectory ? asset.id : null,
    doPaste,
  })

  const canPaste =
    (
      !pasteDataParent ||
      !pasteData ||
      !isCloud ||
      (pasteDataParent.ensoPath != null && permissions.isTeamPath(pasteDataParent.ensoPath))
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

  const pasteMenuEntry = defineMenuEntry(
    hasPasteData &&
      canPaste && {
        action: 'paste',
        doAction: () => {
          const directoryId =
            asset.type === backendModule.AssetType.directory ? asset.id : asset.parentId
          doPaste(directoryId, directoryId)
        },
      },
  )

  const canUploadToCloud = user.plan !== backendModule.Plan.free

  const copyIdEntry = defineMenuEntry(
    showDeveloperIds && {
      color: 'accent',
      action: 'copyId',
      doAction: () => {
        void copyMutation.mutateAsync(asset.id)
      },
    },
  )

  const entries = useMenuEntries(
    category.type === 'trash' ?
      !ownsThisAsset ? []
      : [
          copyIdEntry,
          {
            action: 'undelete',
            label: getText('restoreFromTrashShortcut'),
            doAction: () => {
              void restoreAssetsMutation({
                ids: [asset.id],
                parentId: null,
              })
            },
          },
          {
            action: 'delete',
            label: getText('deleteForeverShortcut'),
            doAction: () => {
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
            },
          },
          pasteMenuEntry,
        ]
    : !canManageThisAsset ? []
    : [
        copyIdEntry,
        (asset.type === backendModule.AssetType.datalink ||
          asset.type === backendModule.AssetType.file) && {
          action: 'useInNewProject',
          doAction: () => {
            void newProject({ templateName: asset.title, ensoPath: asset.ensoPath }, asset.parentId)
          },
        },
        asset.type === backendModule.AssetType.project &&
          canExecute &&
          !isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'open',
            isDisabled: !canRunProjects.locally[backend.type],
            tooltip: disabledTooltip,
            doAction: () => {
              void openProjectLocally(asset, backend.type)
            },
          },
        asset.type === backendModule.AssetType.project &&
          isCloud &&
          localBackend != null && {
            action: 'run',
            isDisabled: !canRunProjects.natively[backend.type],
            tooltip: disabledTooltip,
            doAction: () => {
              void openProjectNatively(asset, backend.type)
            },
          },
        !isCloud &&
          encodedEnsoPath != null &&
          systemApi && {
            action: 'openInFileBrowser',
            doAction: () => {
              systemApi.showItemInFolder(encodedEnsoPath)
            },
          },
        asset.type === backendModule.AssetType.project &&
          canExecute &&
          isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'close',
            doAction: () => {
              void closeProject({
                id: asset.id,
                title: asset.title,
                parentId: asset.parentId,
                type: state.backend.type,
              })
            },
          },
        isUploadableAsset(asset) &&
          !isCloud &&
          localBackend != null && {
            isUnderPaywall: !canUploadToCloud,
            action: 'uploadToCloud',
            feature: 'uploadToCloud',
            doAction: () => {
              void uploadFileToCloudMutation(localBackend, {
                assets: [asset],
                targetDirectoryId: user.rootDirectoryId,
              })
            },
          },
        isUploadableAsset(asset) &&
          isCloud &&
          localBackend != null && {
            action: 'downloadToLocal',
            doAction: () => {
              void uploadFileToLocal([asset])
            },
          },
        {
          action: 'exportArchive',
          doAction: () => {
            void exportArchive()
          },
        },
        canExecute &&
          !isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'rename',
            doAction: () => {
              setRowState(object.merger({ isEditingName: true }))
            },
          },
        (asset.type === backendModule.AssetType.secret ||
          asset.type === backendModule.AssetType.datalink) &&
          canEditThisAsset && {
            action: 'edit',
            doAction: () => {
              rightPanel.setTemporaryTab('settings')
              rightPanel.updateContext('drive', (ctx) => {
                ctx.category = category
                ctx.item = asset
                switch (asset.type) {
                  case backendModule.AssetType.secret:
                  case backendModule.AssetType.datalink:
                    ctx.spotlightOn = asset.type
                    break
                }
                return ctx
              })
            },
          },
        ownsThisAsset &&
          !isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'delete',
            label: isCloud ? getText('moveToTrashShortcut') : getText('deleteShortcut'),
            doAction: () => {
              const textId = isCloud ? 'trashTheAssetTypeTitle' : 'deleteTheAssetTypeTitle'
              setModal(
                <ConfirmDeleteModal
                  defaultOpen
                  actionText={getText(
                    textId,
                    getText(backendModule.ASSET_TYPE_TO_TEXT_ID[asset.type]),
                    asset.title,
                  )}
                  onConfirm={async () => {
                    await deleteAssetsMutation([[asset.id], false])
                  }}
                />,
              )
            },
          },
        isCloud && {
          action: 'label',
          doAction: () => {
            setModal(<ManageLabelsModal backend={backend} item={asset} triggerRef={triggerRef} />)
          },
        },
        asset.type === backendModule.AssetType.project && {
          action: 'duplicate',
          doAction: () => {
            void copyAssetsMutation([[asset.id], asset.parentId])
          },
        },
        { action: 'copy', doAction: doCopy },
        encodedEnsoPath != null && {
          action: 'copyAsPath',
          doAction: () => {
            void copyMutation.mutateAsync(encodedEnsoPath)
          },
        },
        !isRunningProject && !isOtherUserUsingProject && { action: 'cut', doAction: doCut },
        (isCloud ?
          asset.type !== backendModule.AssetType.directory
        : asset.type === backendModule.AssetType.project) && {
          isDisabled: asset.type === backendModule.AssetType.secret,
          action: 'download',
          doAction: () => {
            void downloadAssetsMutation({
              ids: [{ id: asset.id, title: asset.title }],
              targetDirectoryId:
                !isCloud ? (localCategories.localCategory?.homeDirectoryId ?? null) : null,
              shouldUnpackProject: false,
            })
          },
        },
        pasteMenuEntry,
        ...(canAddToThisDirectory ? globalContextMenuEntries : []),
      ],
  )

  return (
    <ContextMenu
      ref={ref}
      aria-label={getText('assetContextMenuLabel')}
      entries={entries}
      initialPosition={initialPosition}
    />
  )
})
