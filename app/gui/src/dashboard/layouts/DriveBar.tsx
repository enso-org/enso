/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import * as React from 'react'

import AddDatalinkIcon from '#/assets/add_datalink.svg'
import AddFolderIcon from '#/assets/add_folder.svg'
import AddKeyIcon from '#/assets/add_key.svg'
import DataDownloadIcon from '#/assets/data_download.svg'
import DataUploadIcon from '#/assets/data_upload.svg'
import BackIcon from '#/assets/expand_arrow_left.svg'
import Plus2Icon from '#/assets/plus2.svg'
import * as aria from '#/components/aria'
import {
  Button,
  ButtonGroup,
  DialogTrigger,
  Text,
  useVisualTooltip,
} from '#/components/AriaComponents'
import {
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  getAllTrashedItems,
} from '#/hooks/backendBatchedHooks'
import {
  listDirectoryQueryOptions,
  useNewDatalink,
  useNewFolder,
  useNewProject,
  useNewSecret,
} from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline } from '#/hooks/offlineHooks'
import AssetSearchBar from '#/layouts/AssetSearchBar'
import {
  canTransferBetweenCategories,
  isCloudCategory,
  type Category,
} from '#/layouts/CategorySwitcher/Category'
import StartModal from '#/layouts/StartModal'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import {
  useCanCreateAssets,
  useCanDownload,
  useDriveStore,
  usePasteData,
} from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { isDirectoryId } from '#/services/Backend'
import type AssetQuery from '#/utilities/AssetQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { readUserSelectedFile } from 'enso-common/src/utilities/file'
import { useMutation, useQueryClient, useSuspenseQuery } from '@tanstack/react-query'
import { Breadcrumbs } from '../components/Breadcrumbs'
import { useFullUserSession } from '../providers/AuthProvider'
import { parseDirectoriesPath } from '../services/utilities'
import { AssetPanelToggle } from './AssetPanel'
import { useCategories } from './Drive/Categories/categoriesHooks'
import { useDirectoryIds } from './Drive/directoryIdsHooks'

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly backend: Backend
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly category: Category
  readonly isEmpty: boolean
  readonly shouldDisplayStartModal: boolean
  readonly isDisabled: boolean
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export default function DriveBar(props: DriveBarProps) {
  const { backend, query, setQuery, category, isEmpty, shouldDisplayStartModal, isDisabled } = props

  const queryClient = useQueryClient()
  const { unsetModal } = useSetModal()
  const { getText } = useText()
  const driveStore = useDriveStore()
  const inputBindings = useInputBindings()
  const canCreateAssets = useCanCreateAssets()
  const createAssetButtonsRef = React.useRef<HTMLDivElement>(null)
  const isCloud = isCloudCategory(category)
  const { isOffline } = useOffline()
  const { user } = useFullUserSession()
  const canDownload = useCanDownload()

  const { rootDirectoryId, currentDirectoryId, parentDirectoryId, setCurrentDirectoryId } =
    useDirectoryIds({ category })

  const { data: directoryData } = useSuspenseQuery({
    ...listDirectoryQueryOptions({
      backend,
      parentId: parentDirectoryId,
      category,
    }),
    select: (data) => {
      if (parentDirectoryId === currentDirectoryId) {
        return null
      }

      const directory = data.find((item) => item.id === currentDirectoryId)

      if (directory == null) {
        return null
      }

      const virtualParentsPath = () => {
        if (directory.virtualParentsPath.length === 0) {
          return directory.title
        }

        return directory.virtualParentsPath + '/' + directory.title
      }

      return {
        parentsPath: directory.parentsPath + '/' + currentDirectoryId,
        virtualParentsPath: virtualParentsPath(),
      }
    },
  })

  const shouldBeDisabled = (isCloud && isOffline) || !canCreateAssets || isDisabled

  const error =
    !shouldBeDisabled ? null
    : isCloud && isOffline ? getText('youAreOffline')
    : getText('cannotCreateAssetsHere')
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
      canTransferBetweenCategories(pasteData.data.category, category, user)
    ) ?
      pasteData
    : null

  const downloadAssetsMutation = useMutation(downloadAssetsMutationOptions(backend))
  const deleteAssetsMutation = useMutation(deleteAssetsMutationOptions(backend))
  const newFolder = useNewFolder(backend, category)
  const uploadFilesRaw = useUploadFiles(backend, category)
  const uploadFiles = useEventCallback(async (files: readonly File[]) => {
    await uploadFilesRaw(files, currentDirectoryId)
  })
  const newSecretRaw = useNewSecret(backend)
  const newSecret = useEventCallback(async (name: string, value: string) => {
    return await newSecretRaw(name, value, currentDirectoryId)
  })
  const newDatalinkRaw = useNewDatalink(backend)
  const newDatalink = useEventCallback(async (name: string, value: unknown) => {
    return await newDatalinkRaw(name, value, currentDirectoryId)
  })
  const newProjectRaw = useNewProject(backend, category)

  const newProjectMutation = useMutation({
    mutationKey: ['newProject'],
    mutationFn: async ([templateId, templateName]: [
      templateId: string | null | undefined,
      templateName: string | null | undefined,
    ]) => await newProjectRaw({ templateName, templateId }, currentDirectoryId),
  })

  const isCreatingProject = newProjectMutation.isPending

  const clearTrash = useEventCallback(async () => {
    const allTrashedItems = await getAllTrashedItems(queryClient, backend)
    await deleteAssetsMutation.mutateAsync([allTrashedItems.map((item) => item.id), true])
  })

  const navigateToDirectory = useEventCallback((id: React.Key) => {
    const parentId = finalPath.findIndex((item) => item.id === id) - 1

    if (!isDirectoryId(id)) {
      return
    }

    setCurrentDirectoryId({
      current: id,
      // This is safe, because we know the index is present in the array.
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      parent: parentId < 0 ? null : finalPath[parentId]!.id,
    })
  })

  const navigateToParent = useEventCallback(() => {
    navigateToDirectory(parentDirectoryId)
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
        void newProjectMutation.mutateAsync([null, null])
      },
      uploadFiles: () => {
        void readUserSelectedFile().then((files) => uploadFiles(Array.from(files)))
      },
    }),
  )

  React.useEffect(() => attachEventListeners(), [attachEventListeners])

  const { getCategoryByDirectoryId } = useCategories()
  const { finalPath } = parseDirectoriesPath({
    parentsPath: directoryData?.parentsPath ?? '',
    virtualParentsPath: directoryData?.virtualParentsPath ?? '',
    rootDirectoryId,
    getCategoryByDirectoryId,
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
      <Text>
        {effectivePasteData.type === 'copy' ?
          getText('xItemsCopied', effectivePasteData.data.ids.size)
        : getText('xItemsCut', effectivePasteData.data.ids.size)}
      </Text>
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
        <ButtonGroup className="grow-0">
          <DialogTrigger>
            <Button size="medium" variant="outline" isDisabled={shouldBeDisabled || isEmpty}>
              {getText('clearTrash')}
            </Button>

            <ConfirmDeleteModal
              actionText={getText('allTrashedItemsForever')}
              doDelete={async () => {
                await clearTrash()
              }}
            />
          </DialogTrigger>
          {pasteDataStatus}
          {searchBar}
          {assetPanelToggle}
        </ButtonGroup>
      )
    }
    case 'cloud':
    case 'local':
    case 'user':
    case 'team':
    case 'local-directory': {
      return (
        <div className="flex flex-col gap-2">
          <div className="flex w-auto flex-none items-center">
            <ButtonGroup
              className="mr-4 w-auto flex-none"
              buttonVariants={{ variant: 'icon', size: 'small' }}
            >
              <Button
                icon={BackIcon}
                aria-label={getText('back')}
                isDisabled={currentDirectoryId === user.rootDirectoryId}
                onPress={navigateToParent}
              />
            </ButtonGroup>

            <Breadcrumbs className="mr-2" onAction={navigateToDirectory}>
              {finalPath.map((pathItem) => {
                const isCurrent = pathItem.id === currentDirectoryId

                return (
                  <Breadcrumbs.Item
                    key={pathItem.id}
                    id={pathItem.id}
                    icon={pathItem.icon}
                    isCurrent={isCurrent}
                  >
                    {pathItem.label}
                  </Breadcrumbs.Item>
                )
              })}
            </Breadcrumbs>

            <div className="ml-auto">{assetPanelToggle}</div>
          </div>

          <ButtonGroup>
            <ButtonGroup
              ref={createAssetButtonsRef}
              className="grow-0"
              {...createAssetsVisualTooltip.targetProps}
            >
              <DialogTrigger defaultOpen={shouldDisplayStartModal}>
                <Button
                  size="medium"
                  variant="accent"
                  isDisabled={shouldBeDisabled || isCreatingProject}
                  icon={Plus2Icon}
                  loaderPosition="icon"
                >
                  {getText('startWithATemplate')}
                </Button>

                <StartModal
                  createProject={(templateId, templateName) => {
                    void newProjectMutation.mutateAsync([templateId, templateName])
                  }}
                />
              </DialogTrigger>
              <Button
                size="medium"
                variant="outline"
                isDisabled={shouldBeDisabled}
                icon={Plus2Icon}
                loaderPosition="icon"
                onPress={() => newProjectMutation.mutateAsync([null, null])}
              >
                {getText('newEmptyProject')}
              </Button>
              <div className="flex h-row items-center gap-4 rounded-full border-0.5 border-primary/20 px-[11px]">
                <Button
                  variant="icon"
                  size="medium"
                  icon={AddFolderIcon}
                  isDisabled={shouldBeDisabled}
                  aria-label={getText('newFolder')}
                  onPress={() => newFolder(currentDirectoryId)}
                />
                {isCloud && (
                  <DialogTrigger>
                    <Button
                      variant="icon"
                      size="medium"
                      icon={AddKeyIcon}
                      isDisabled={shouldBeDisabled}
                      aria-label={getText('newSecret')}
                    />
                    <UpsertSecretModal
                      id={null}
                      name={null}
                      doCreate={async (name, value) => {
                        await newSecret(name, value)
                      }}
                    />
                  </DialogTrigger>
                )}

                {isCloud && (
                  <DialogTrigger>
                    <Button
                      variant="icon"
                      size="medium"
                      icon={AddDatalinkIcon}
                      isDisabled={shouldBeDisabled}
                      aria-label={getText('newDatalink')}
                    />
                    <UpsertDatalinkModal
                      doCreate={async (name, value) => {
                        await newDatalink(name, value)
                      }}
                    />
                  </DialogTrigger>
                )}
                <Button
                  variant="icon"
                  size="medium"
                  icon={DataUploadIcon}
                  isDisabled={shouldBeDisabled}
                  aria-label={getText('uploadFiles')}
                  onPress={async () => {
                    const files = await readUserSelectedFile()
                    await uploadFiles(Array.from(files))
                  }}
                />
                <Button
                  isDisabled={!canDownload || shouldBeDisabled}
                  variant="icon"
                  size="medium"
                  icon={DataDownloadIcon}
                  aria-label={getText('downloadFiles')}
                  onPress={() => {
                    unsetModal()
                    const { selectedAssets } = driveStore.getState()
                    downloadAssetsMutation.mutate(selectedAssets)
                  }}
                />
              </div>
              {createAssetsVisualTooltip.tooltip}
            </ButtonGroup>
            {pasteDataStatus}
            {searchBar}
          </ButtonGroup>
        </div>
      )
    }
  }
}
