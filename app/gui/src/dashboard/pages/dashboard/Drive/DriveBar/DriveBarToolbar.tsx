/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import Plus2Icon from '#/assets/plus2.svg'
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { ErrorBoundary, InlineErrorDisplay } from '#/components/ErrorBoundary'
import { IconDisplay } from '#/components/IconDisplay'
import { useVisualTooltip, VisualTooltip } from '#/components/VisualTooltip'
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
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline } from '#/hooks/offlineHooks'
import { AssetSearchBar } from '#/layouts/AssetSearchBar'
import type { TrashCategory } from '#/layouts/CategorySwitcher/Category'
import { canTransferBetweenCategories } from '#/layouts/CategorySwitcher/Category'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { CreateCredentialModal } from '#/modals/CreateCredentialModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useExportArchive } from '#/pages/useExportArchive'
import { useCanDownload, useDriveStore, usePasteData } from '#/providers/DriveProvider'
import { unsetModal } from '#/providers/ModalProvider'
import type AssetQuery from '#/utilities/AssetQuery'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { useQueryClient, useSuspenseQuery } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import {
  BackendType,
  isDirectoryId,
  isProjectId,
  type CredentialConfig,
} from 'enso-common/src/services/Backend'
import { readUserSelectedFile } from 'enso-common/src/utilities/file'
import type { PropsWithChildren } from 'react'
import * as React from 'react'

/** Props for a {@link DriveBar}. */
export interface DriveBarToolbarProps {
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBarToolbar(props: DriveBarToolbarProps) {
  const { query, setQuery } = props

  const { category, associatedBackend: backend } = useCategoriesAPI()
  const { getText } = useText()
  const driveStore = useDriveStore()
  const createAssetButtonsRef = React.useRef<HTMLDivElement>(null)
  const isCloud = backend.type === BackendType.remote
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
  const exportArchive = useExportArchive({ backend })

  const newProjectMutation = useMutationCallback({
    mutationKey: ['newProject'],
    mutationFn: async () => await newProjectRaw({}, currentDirectoryId),
  })

  const newProject = useEventCallback(async () => {
    await newProjectMutation()
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
    const files = await readUserSelectedFile({ multiple: true })
    await uploadFiles(Array.from(files))
  })

  const downloadFilesCallback = useEventCallback(() => {
    unsetModal()
    const { selectedAssets } = driveStore.getState()
    if (
      selectedAssets.length === 1 &&
      selectedAssets[0] != null &&
      (isCloud ? !isDirectoryId(selectedAssets[0].id) : isProjectId(selectedAssets[0].id))
    ) {
      void downloadAssetsMutation({
        ids: selectedAssets,
        targetDirectoryId: null,
      })
    } else {
      void exportArchive()
    }
  })

  const searchBar = (
    <AssetSearchBar backend={backend} isCloud={isCloud} query={query} setQuery={setQuery} />
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
        <Button.Group className="grow-0">
          {pasteDataStatus}
          {searchBar}
        </Button.Group>
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
          <Button.Group
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
              <Dialog.Trigger>
                <Button
                  isDisabled={!isCloud}
                  variant="icon"
                  size="medium"
                  icon="key_add"
                  aria-label={isCloud ? getText('newSecret') : getText('newSecret.cloudOnly')}
                />
                <UpsertSecretModal doCreate={newSecretCallback} />
              </Dialog.Trigger>
              <Dialog.Trigger>
                <Button
                  isDisabled={!isCloud}
                  variant="icon"
                  size="medium"
                  icon="credential_add"
                  aria-label={
                    isCloud ? getText('newCredential') : getText('newCredential.cloudOnly')
                  }
                />
                <CreateCredentialModal doCreate={newCredentialCallback} />
              </Dialog.Trigger>
              <Dialog.Trigger>
                <Button
                  isDisabled={!isCloud}
                  variant="icon"
                  size="medium"
                  icon="connector_add"
                  aria-label={isCloud ? getText('newDatalink') : getText('newDatalink.cloudOnly')}
                />
                <UpsertDatalinkModal doCreate={newDatalinkCallback} />
              </Dialog.Trigger>
            </div>

            <div className="flex h-row items-center gap-4 rounded-full border-0.5 border-primary/20 px-[11px]">
              <Button
                variant="icon"
                size="medium"
                icon="data_upload"
                aria-label={getText('uploadFiles')}
                onPress={uploadFilesCallback}
              />
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
          </Button.Group>
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
    labels: null,
    sortDirection: null,
    sortExpression: null,
  })

  const { data: isEmpty } = useSuspenseQuery({
    ...rootDirectoryQueryOptions,
    select: (data) => data.assets.length === 0,
  })

  const queryClient = useQueryClient()
  const deleteAssetsMutation = useMutationCallback(deleteAssetsMutationOptions(backend))

  const clearTrash = useEventCallback(async () => {
    const allTrashedItems = await getAllTrashedItems(queryClient, backend, category)
    await deleteAssetsMutation([allTrashedItems.map((item) => item.id), true])
  })

  return (
    <Button.Group className="grow-0" buttonVariants={{ isDisabled: shouldBeDisabled }}>
      <Dialog.Trigger>
        <Button size="medium" variant="outline" isDisabled={isEmpty}>
          {getText('clearTrash')}
        </Button>

        <ConfirmDeleteModal
          actionText={getText('allTrashedItemsForever')}
          onConfirm={async () => {
            await clearTrash()
          }}
        />
      </Dialog.Trigger>

      {children}
    </Button.Group>
  )
}
