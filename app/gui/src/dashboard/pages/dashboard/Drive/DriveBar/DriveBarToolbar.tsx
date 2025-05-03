/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import * as React from 'react'

import Plus2Icon from '#/assets/plus2.svg'
import {
  Button,
  ButtonGroup,
  DialogTrigger,
  IconDisplay,
  useVisualTooltip,
  VisualTooltip,
} from '#/components/AriaComponents'
import { ErrorBoundary, InlineErrorDisplay } from '#/components/ErrorBoundary'
import { PaywallDialog } from '#/components/Paywall'
import {
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  getAllTrashedItems,
} from '#/hooks/backendBatchedHooks'
import {
  backendMutationOptions,
  listDirectoryQueryOptions,
  useNewFolder,
  useNewProject,
} from '#/hooks/backendHooks'
import { useUploadFiles, useUploadFileToCloudMutation } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline } from '#/hooks/offlineHooks'
import { useStore } from '#/hooks/storeHooks'
import { AssetPanelToggle } from '#/layouts/AssetPanel'
import AssetSearchBar from '#/layouts/AssetSearchBar'
import type { TrashCategory } from '#/layouts/CategorySwitcher/Category'
import {
  canTransferBetweenCategories,
  isCloudCategory,
  type Category,
} from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategories, useTransferBetweenCategories } from '#/layouts/Drive/Categories'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { CreateCredentialModal } from '#/modals/CreateCredentialModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useUser } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useCanDownload, useDriveStore, usePasteData } from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { setModal, useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { AssetType, getAssetTypeFromId, Plan, type CredentialConfig } from '#/services/Backend'
import type AssetQuery from '#/utilities/AssetQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useQueryClient, useSuspenseQuery } from '@tanstack/react-query'
import { readUserSelectedFile } from 'enso-common/src/utilities/file'
import type { PropsWithChildren } from 'react'
import invariant from 'tiny-invariant'

/** Props for a {@link DriveBar}. */
export interface DriveBarToolbarProps {
  readonly backend: Backend
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly category: Category
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBarToolbar(props: DriveBarToolbarProps) {
  const { backend, query, setQuery, category } = props

  const { unsetModal } = useSetModal()
  const { getText } = useText()
  const driveStore = useDriveStore()
  const inputBindings = useInputBindings()
  const createAssetButtonsRef = React.useRef<HTMLDivElement>(null)
  const isCloud = isCloudCategory(category)
  const { isOffline } = useOffline()
  const canDownload = useCanDownload()

  const { currentDirectoryId } = useDirectoryIds({ category })

  const shouldBeDisabled = isCloud && isOffline

  const error = shouldBeDisabled ? getText('cannotCreateAssetsHere') : null

  const createAssetsVisualTooltip = useVisualTooltip({
    isDisabled: error == null,
    children: error,
    targetRef: createAssetButtonsRef,
    overlayPositionProps: { placement: 'top' },
  })

  const pasteData = usePasteData()
  const effectivePasteData =
    (
      pasteData?.data.backendType === backend.type &&
      canTransferBetweenCategories(pasteData.data.category, category)
    ) ?
      pasteData
    : null

  const downloadAssetsMutation = useMutationCallback(downloadAssetsMutationOptions(backend))
  const newFolder = useNewFolder(backend, category)
  const uploadFilesRaw = useUploadFiles(backend, category)
  const uploadFiles = useEventCallback(async (files: readonly File[]) => {
    await uploadFilesRaw(files, currentDirectoryId)
  })
  const newSecret = useMutationCallback(backendMutationOptions(backend, 'createSecret'))
  const newCredential = useMutationCallback(backendMutationOptions(backend, 'createCredential'))
  const newDatalink = useMutationCallback(backendMutationOptions(backend, 'createDatalink'))
  const newProjectRaw = useNewProject(backend, category)

  const newProjectMutation = useMutationCallback({
    mutationKey: ['newProject'],
    mutationFn: async ([templateId, templateName]: [
      templateId: string | null | undefined,
      templateName: string | null | undefined,
    ]) => await newProjectRaw({ templateName, templateId }, currentDirectoryId),
  })

  const attachEventListeners = useEventCallback(() =>
    inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
      ...(isCloud ?
        {
          newFolder: () => {
            void newFolder(currentDirectoryId)
          },
        }
      : {}),
      newProject: () => {
        void newProjectMutation([null, null])
      },
      uploadFiles: () => {
        void readUserSelectedFile().then((files) => uploadFiles(Array.from(files)))
      },
    }),
  )

  React.useEffect(() => attachEventListeners(), [attachEventListeners])

  const newProject = useEventCallback(async () => {
    await newProjectMutation([null, null])
  })

  const newFolderCallback = useEventCallback(async () => {
    await newFolder(currentDirectoryId)
  })

  const newCredentialCallback = useEventCallback(async (name: string, value: CredentialConfig) => {
    return await newCredential([{ name, value, parentDirectoryId: currentDirectoryId }])
  })

  const newSecretCallback = useEventCallback(async (name: string, value: string) => {
    await newSecret([{ name, value, parentDirectoryId: currentDirectoryId }])
  })

  const newDatalinkCallback = useEventCallback(async (name: string, value: unknown) => {
    await newDatalink([
      {
        name,
        value,
        parentDirectoryId: currentDirectoryId,
        datalinkId: null,
      },
    ])
  })

  const uploadFilesCallback = useEventCallback(async () => {
    const files = await readUserSelectedFile()
    await uploadFiles(Array.from(files))
  })

  const downloadFilesCallback = useEventCallback(async () => {
    unsetModal()
    const { selectedAssets } = driveStore.getState()
    await downloadAssetsMutation({
      ids: selectedAssets,
      targetDirectoryId: null,
    })
  })

  const searchBar = (
    <AssetSearchBar backend={backend} isCloud={isCloud} query={query} setQuery={setQuery} />
  )

  const assetPanelToggle = (
    <>
      {/* Spacing. */}
      <div className="ml-auto" />
      <AssetPanelToggle showWhen="collapsed" className="my-auto" />
    </>
  )

  const pasteDataStatus = effectivePasteData && (
    <div className="flex items-center">
      <VisualTooltip
        tooltip={
          effectivePasteData.type === 'copy' ?
            getText('xItemsCopied', effectivePasteData.data.assets.length)
          : getText('xItemsCut', effectivePasteData.data.assets.length)
        }
        tooltipPlacement="top"
      >
        <IconDisplay icon={effectivePasteData.type === 'copy' ? 'copy' : 'scissors'}>
          {String(effectivePasteData.data.assets.length)}
        </IconDisplay>
      </VisualTooltip>
    </div>
  )

  switch (category.type) {
    case 'recent': {
      return (
        <ButtonGroup className="grow-0">
          {pasteDataStatus}
          {searchBar}
          {assetPanelToggle}
        </ButtonGroup>
      )
    }
    case 'trash': {
      return (
        <ErrorBoundary FallbackComponent={InlineErrorDisplay}>
          <TrashFolderToolbar
            shouldBeDisabled={shouldBeDisabled}
            backend={backend}
            category={category}
          >
            {pasteDataStatus}
            {searchBar}
            {assetPanelToggle}
          </TrashFolderToolbar>
        </ErrorBoundary>
      )
    }
    case 'cloud':
    case 'local':
    case 'user':
    case 'team':
    case 'local-directory': {
      return (
        <div className="flex w-full flex-1 shrink-0 gap-2">
          <ButtonGroup
            ref={createAssetButtonsRef}
            className="grow-0"
            buttonVariants={{ isDisabled: shouldBeDisabled }}
            {...createAssetsVisualTooltip.targetProps}
          >
            <Button variant="accent" icon={Plus2Icon} loaderPosition="icon" onPress={newProject}>
              {getText('newEmptyProject')}
            </Button>

            <div className="flex h-row items-center gap-4 rounded-full border-0.5 border-primary/20 px-[11px]">
              <Button
                variant="icon"
                size="medium"
                icon="folder_add"
                aria-label={getText('newFolder')}
                onPress={newFolderCallback}
              />
              <DialogTrigger>
                <Button
                  isDisabled={!isCloud}
                  variant="icon"
                  size="medium"
                  icon="key_add"
                  aria-label={isCloud ? getText('newSecret') : getText('newSecretOnlyCloud')}
                />
                <UpsertSecretModal id={null} name={null} doCreate={newSecretCallback} />
              </DialogTrigger>
              <DialogTrigger>
                <Button
                  isDisabled={!isCloud}
                  variant="icon"
                  size="medium"
                  icon="credential_add"
                  aria-label={
                    isCloud ? getText('newCredential') : getText('newCredentialOnlyCloud')
                  }
                />
                <CreateCredentialModal doCreate={newCredentialCallback} />
              </DialogTrigger>
              <DialogTrigger>
                <Button
                  isDisabled={!isCloud}
                  variant="icon"
                  size="medium"
                  icon="connector_add"
                  aria-label={isCloud ? getText('newDatalink') : getText('newDatalinkOnlyCloud')}
                />
                <UpsertDatalinkModal doCreate={newDatalinkCallback} />
              </DialogTrigger>
            </div>

            <div className="flex h-row items-center gap-4 rounded-full border-0.5 border-primary/20 px-[11px]">
              <Button
                variant="icon"
                size="medium"
                icon="data_upload"
                aria-label={getText('uploadFiles')}
                onPress={uploadFilesCallback}
              />
              <UploadFilesToCloudButton category={category} />
              <DownloadFilesToLocalButton category={category} />
              <Button
                isDisabled={!canDownload}
                variant="icon"
                size="medium"
                icon="data_download"
                aria-label={getText('downloadFiles')}
                onPress={downloadFilesCallback}
              />
            </div>
            {createAssetsVisualTooltip.tooltip}
          </ButtonGroup>
          {pasteDataStatus}
          {searchBar}
        </div>
      )
    }
  }
}

/**
 * Props for a {@link TrashFolderToolbar}.
 */
interface TrashFolderToolbarProps extends PropsWithChildren {
  readonly shouldBeDisabled: boolean
  readonly backend: Backend
  readonly category: TrashCategory
}

/**
 * A toolbar for the trash folder.
 */
function TrashFolderToolbar(props: TrashFolderToolbarProps) {
  const { shouldBeDisabled, backend, category, children } = props
  const { getText } = useText()

  const rootDirectoryQueryOptions = listDirectoryQueryOptions({
    backend,
    category,
    parentId: category.homeDirectoryId,
    refetchInterval: null,
  })

  const { data: isEmpty } = useSuspenseQuery({
    ...rootDirectoryQueryOptions,
    select: (data) => data.length === 0,
  })

  const queryClient = useQueryClient()
  const deleteAssetsMutation = useMutationCallback(deleteAssetsMutationOptions(backend))

  const clearTrash = useEventCallback(async () => {
    const allTrashedItems = await getAllTrashedItems(queryClient, backend, category)
    await deleteAssetsMutation([allTrashedItems.map((item) => item.id), true])
  })

  return (
    <ButtonGroup className="grow-0" buttonVariants={{ isDisabled: shouldBeDisabled }}>
      <DialogTrigger>
        <Button size="medium" variant="outline" isDisabled={isEmpty}>
          {getText('clearTrash')}
        </Button>

        <ConfirmDeleteModal
          actionText={getText('allTrashedItemsForever')}
          onConfirm={async () => {
            await clearTrash()
          }}
        />
      </DialogTrigger>

      {children}
    </ButtonGroup>
  )
}

/** Props for {@link UploadFilesToCloudButton}. */
export interface UploadFilesToCloudButtonProps {
  readonly category: Category
}

/** A button to upload assets to the cloud. */
function UploadFilesToCloudButton(props: UploadFilesToCloudButtonProps) {
  const { category } = props

  const user = useUser()
  const getAsset = useGetAsset()
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const uploadFileToCloudMutation = useUploadFileToCloudMutation()
  const isCloud = isCloudCategory(category)
  const driveStore = useDriveStore()
  const isDisabled = useStore(
    driveStore,
    (state) =>
      isCloud ||
      state.selectedIds.size === 0 ||
      [...state.selectedIds].some((id) => getAssetTypeFromId(id) !== AssetType.project),
  )
  const canUploadToCloud = user.plan !== Plan.free
  const isUnderPaywall = !canUploadToCloud

  const uploadFilesToCloud = useEventCallback(async () => {
    invariant(localBackend != null, 'Cannot upload to cloud without Local backend')
    const selectedIds = [...driveStore.getState().selectedIds]
    const files = selectedIds.flatMap((id) => {
      const asset = getAsset(id)
      return asset ? [asset] : []
    })
    await uploadFileToCloudMutation(localBackend, {
      assets: Array.from(files),
      targetDirectoryId: user.rootDirectoryId,
    })
  })

  if (isCloud) {
    return
  }

  return (
    <Button
      variant="icon"
      size="medium"
      icon={isUnderPaywall ? 'icon/lock' : 'cloud_to'}
      isDisabled={isDisabled}
      aria-label={getText('uploadFilesToCloud')}
      onPress={async () => {
        if (isUnderPaywall) {
          setModal(<PaywallDialog modalProps={{ defaultOpen: true }} feature="uploadToCloud" />)
        } else {
          await uploadFilesToCloud()
        }
      }}
    />
  )
}

/** Props for {@link DownloadFilesToLocalButton}. */
export interface DownloadFilesToLocalButtonProps {
  readonly category: Category
}

/** A button to upload assets to the cloud. */
function DownloadFilesToLocalButton(props: DownloadFilesToLocalButtonProps) {
  const { category } = props

  const getAsset = useGetAsset()
  const { getText } = useText()
  const { localCategories } = useCategories()
  const localBackend = useLocalBackend()
  const transferBetweenCategories = useTransferBetweenCategories(category)
  const isCloud = isCloudCategory(category)
  const driveStore = useDriveStore()
  const isDisabled = useStore(
    driveStore,
    (state) =>
      !isCloud ||
      state.selectedIds.size === 0 ||
      [...state.selectedIds].some((id) => getAssetTypeFromId(id) !== AssetType.project),
  )

  const downloadFilesToLocal = useEventCallback(async () => {
    invariant(localBackend != null, 'Cannot download to local without Local backend')
    const localHomeCategory = localCategories.categories.find(
      (otherCategory) => otherCategory.type === 'local',
    )
    invariant(localHomeCategory, 'Local home category must exist to download to local')

    const selectedIds = [...driveStore.getState().selectedIds]
    const files = selectedIds.flatMap((id) => {
      const asset = getAsset(id)
      return asset ? [asset] : []
    })

    await transferBetweenCategories(category, localHomeCategory, files)
  })

  if (!isCloud) {
    return
  }

  return (
    <Button
      variant="icon"
      size="medium"
      icon="cloud_from"
      isDisabled={isDisabled}
      aria-label={getText('downloadFilesToLocal')}
      onPress={downloadFilesToLocal}
    />
  )
}
