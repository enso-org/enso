/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import type { ContextMenuApi } from '#/components/ContextMenu'
import { ContextMenu } from '#/components/ContextMenu'
import {
  copyAssetsMutationOptions,
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useNewProject } from '#/hooks/backendHooks'
import {
  isUploadableAsset,
  useUploadFileToCloud,
  useUploadFileToLocal,
} from '#/hooks/backendUploadFilesHooks'
import { useCopy } from '#/hooks/copyHooks'
import { defineMenuEntry, useMenuEntries } from '#/hooks/menuHooks'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategories, useCategoriesAPI } from '#/layouts/Drive/Categories'
import { useGlobalContextMenuEntries } from '#/layouts/useGlobalContextMenuEntries'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ManageLabelsModal from '#/modals/ManageLabelsModal'
import { useExportArchive } from '#/pages/useExportArchive'
import { useDriveStore, usePasteData } from '#/providers/DriveProvider'
import { setModal } from '#/providers/ModalProvider'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends, useFullUserSession, useRouter, useText } from '$/providers/react'
import { useVueValue } from '$/providers/react/common'
import { useRightPanelData } from '$/providers/react/container'
import * as featureFlagsProvider from '$/providers/react/featureFlags'
import { useOpenedProjects } from '$/providers/react/openedProjects'
import * as backendModule from 'enso-common/src/services/Backend'
import {
  TEAMS_DIRECTORY_ID,
  USERS_DIRECTORY_ID,
} from 'enso-common/src/services/Backend/remoteBackendPaths'
import * as permissions from 'enso-common/src/utilities/permissions'
import * as React from 'react'

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps {
  readonly asset: backendModule.AnyAsset
  readonly triggerRef: React.MutableRefObject<HTMLElement | null>
  readonly currentDirectoryId: backendModule.DirectoryId
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (newParentId: backendModule.DirectoryId) => void
  readonly initialPosition?: Pick<MouseEvent, 'pageX' | 'pageY'> | null | undefined
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export const AssetContextMenu = React.forwardRef(function AssetContextMenu(
  props: AssetContextMenuProps,
  ref: React.ForwardedRef<ContextMenuApi>,
) {
  const { asset, triggerRef, currentDirectoryId, initialPosition, doCopy, doCut, doPaste } = props

  const { category, associatedBackend: backend } = useCategoriesAPI()
  const isCloud = categoryModule.isCloudCategory(category)
  const rightPanel = useRightPanelData()
  const { router } = useRouter()
  const { localCategories } = useCategories()
  const driveStore = useDriveStore()

  const getAsset = useGetAsset()
  const { user } = useFullUserSession()
  const { localBackend } = useBackends()
  const { getText } = useText()
  const {
    openProjectLocally,
    openProjectNatively,
    canOpenProjectLocally,
    canOpenProjectNatively,
    closeProject,
  } = useOpenedProjects()
  const canOpenLocally = useVueValue(
    React.useCallback(
      () => canOpenProjectLocally(backend.type),
      [canOpenProjectLocally, backend.type],
    ),
  )
  const canOpenNatively = useVueValue(
    React.useCallback(
      () => canOpenProjectNatively(backend.type),
      [canOpenProjectNatively, backend.type],
    ),
  )
  const deleteAssets = useMutationCallback(deleteAssetsMutationOptions(backend))
  const restoreAssets = useMutationCallback(restoreAssetsMutationOptions(backend))
  const copyAssets = useMutationCallback(copyAssetsMutationOptions(backend))
  const downloadAssets = useMutationCallback(downloadAssetsMutationOptions(backend))
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const encodedEnsoPath = encodeURI(asset.ensoPath)
  const copyMutation = useCopy()
  const uploadFileToCloud = useUploadFileToCloud()
  const uploadFileToLocal = useUploadFileToLocal(category)
  const exportArchive = useExportArchive({ backend })
  const disabledTooltip = !canOpenLocally ? getText('downloadToOpenWorkflow') : undefined
  const showDeveloperIds = featureFlagsProvider.useFeatureFlag('showDeveloperIds')

  const newProject = useNewProject(backend, category)

  const systemApi = window.api?.system
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
    !pasteDataParent || !pasteData || !isCloud || permissions.isTeamPath(pasteDataParent.ensoPath) ?
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

  const goToDrive = async () => {
    if (router.currentRoute.value.path === '/drive') return
    await router.push({ ...router.currentRoute.value, path: '/drive' })
  }

  const pasteMenuEntry = defineMenuEntry(
    hasPasteData &&
      canPaste && {
        action: 'paste',
        doAction: () => {
          void goToDrive()
          const directoryId =
            asset.type === backendModule.AssetType.directory ? asset.id : asset.parentId
          doPaste(directoryId)
        },
      },
  )

  const canUploadToCloud = user.plan !== backendModule.Plan.free

  const copyIdEntry = defineMenuEntry(
    showDeveloperIds && {
      color: 'accent',
      action: 'copyId',
      doAction: () => {
        void goToDrive()
        void copyMutation.mutateAsync(asset.id)
      },
    },
  )

  const entries = useMenuEntries(
    category.type === 'trash' ?
      !ownsThisAsset ? []
      : [
          pasteMenuEntry,
          {
            action: 'undelete',
            label: getText('restoreFromTrashShortcut'),
            doAction: () => {
              void goToDrive()
              void restoreAssets({
                ids: [asset.id],
                parentId: null,
              })
            },
          },
          {
            action: 'delete',
            label: getText('deleteForeverShortcut'),
            doAction: () => {
              void goToDrive()
              setModal(
                <ConfirmDeleteModal
                  defaultOpen
                  cannotUndo
                  actionText={getText('deleteTheAssetTypeTitleForever', asset.type, asset.title)}
                  onConfirm={async () => {
                    await deleteAssets([[asset.id], true])
                  }}
                />,
              )
            },
          },
          copyIdEntry,
        ]
    : !canManageThisAsset ? []
    : [
        (asset.type === backendModule.AssetType.datalink ||
          asset.type === backendModule.AssetType.file) && {
          action: 'useInNewProject',
          doAction: () => {
            void goToDrive()
            void newProject({ templateName: asset.title, ensoPath: asset.ensoPath }, asset.parentId)
          },
        },
        asset.type === backendModule.AssetType.project &&
          canExecute &&
          !isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'open',
            isDisabled: !canOpenLocally,
            tooltip: disabledTooltip,
            doAction: () => {
              void goToDrive()
              openProjectLocally(asset, backend.type)
            },
          },
        asset.type === backendModule.AssetType.project &&
          isCloud &&
          localBackend != null && {
            action: 'run',
            isDisabled: !canOpenNatively,
            tooltip: disabledTooltip,
            doAction: () => {
              void goToDrive()
              openProjectNatively(asset, backend.type)
            },
          },
        asset.type === backendModule.AssetType.project &&
          canExecute &&
          isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'close',
            doAction: () => {
              void goToDrive()
              closeProject(asset.id, { asset, backendType: backend.type })
            },
          },
        isCloud && {
          action: 'label',
          doAction: () => {
            void goToDrive()
            setModal(
              <ManageLabelsModal backend={backend} items={[asset]} triggerRef={triggerRef} />,
            )
          },
        },
        isUploadableAsset(asset) &&
          !isCloud &&
          localBackend != null && {
            isUnderPaywall: !canUploadToCloud,
            action: 'uploadToCloud',
            feature: 'uploadToCloud',
            doAction: () => {
              void goToDrive()
              void uploadFileToCloud(localBackend, {
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
              void goToDrive()
              void uploadFileToLocal([asset])
            },
          },
        {
          action: 'copy',
          doAction: () => {
            void goToDrive()
            doCopy()
          },
        },
        !isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'cut',
            doAction: () => {
              void goToDrive()
              doCut()
            },
          },
        pasteMenuEntry,
        (isCloud ?
          asset.type !== backendModule.AssetType.directory
        : asset.type === backendModule.AssetType.project) && {
          isDisabled: asset.type === backendModule.AssetType.secret,
          action: 'download',
          doAction: () => {
            void goToDrive()
            void downloadAssets({
              ids: [{ id: asset.id, title: asset.title }],
              targetDirectoryId:
                !isCloud ? (localCategories.localCategory?.homeDirectoryId ?? null) : null,
              shouldUnpackProject: false,
            })
          },
        },
        canExecute &&
          !isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'rename',
            doAction: () => {
              void goToDrive()
              driveStore.setState({ assetToRename: asset.id })
            },
          },
        (asset.type === backendModule.AssetType.secret ||
          asset.type === backendModule.AssetType.datalink) &&
          canEditThisAsset && {
            action: 'edit',
            doAction: () => {
              void goToDrive()
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
        asset.type === backendModule.AssetType.project && {
          action: 'duplicate',
          doAction: () => {
            void goToDrive()
            void copyAssets([[asset.id], asset.parentId])
          },
        },
        {
          action: 'exportArchive',
          doAction: () => {
            void goToDrive()
            void exportArchive()
          },
        },
        ...(canAddToThisDirectory ? globalContextMenuEntries : []),
        ownsThisAsset &&
          !isRunningProject &&
          !isOtherUserUsingProject && {
            action: 'delete',
            label: isCloud ? getText('moveToTrashShortcut') : getText('deleteShortcut'),
            doAction: () => {
              void goToDrive()
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
                    await deleteAssets([[asset.id], false])
                  }}
                />,
              )
            },
          },
        !isCloud &&
          systemApi && {
            action: 'openInFileBrowser',
            doAction: () => {
              void goToDrive()
              systemApi.showItemInFolder(encodedEnsoPath)
            },
          },
        {
          action: 'copyAsPath',
          doAction: () => {
            void goToDrive()
            void copyMutation.mutateAsync(encodedEnsoPath)
          },
        },
        copyIdEntry,
      ],
  )

  return (
    <ContextMenu
      ref={ref}
      aria-label={getText('assetContextMenuLabel')}
      entries={entries}
      initialPosition={initialPosition}
      onClose={() => {
        driveStore.setState({ contextMenuData: null })
      }}
    />
  )
})
