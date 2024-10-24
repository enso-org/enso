/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import AddDatalinkIcon from '#/assets/add_datalink.svg'
import AddFolderIcon from '#/assets/add_folder.svg'
import AddKeyIcon from '#/assets/add_key.svg'
import DataDownloadIcon from '#/assets/data_download.svg'
import DataUploadIcon from '#/assets/data_upload.svg'
import Plus2Icon from '#/assets/plus2.svg'
import RightPanelIcon from '#/assets/right_panel.svg'
import {
  Button,
  ButtonGroup,
  DialogTrigger,
  Text,
  useVisualTooltip,
} from '#/components/AriaComponents'
import AssetEventType from '#/events/AssetEventType'
import {
  useNewDatalink,
  useNewFolder,
  useNewProject,
  useNewSecret,
  useRootDirectoryId,
  useUploadFiles,
} from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline } from '#/hooks/offlineHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import AssetSearchBar from '#/layouts/AssetSearchBar'
import { useDispatchAssetEvent } from '#/layouts/AssetsTable/EventListProvider'
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
  useIsAssetPanelVisible,
  usePasteData,
  useSetIsAssetPanelPermanentlyVisible,
  useSetIsAssetPanelTemporarilyVisible,
} from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import type AssetQuery from '#/utilities/AssetQuery'
import { inputFiles } from '#/utilities/input'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly backend: Backend
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly category: Category
  readonly doEmptyTrash: () => void
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export default function DriveBar(props: DriveBarProps) {
  const { backend, query, setQuery, category, doEmptyTrash } = props

  const [startModalDefaultOpen, , resetStartModalDefaultOpen] = useSearchParamsState(
    'startModalDefaultOpen',
    false,
  )

  const driveStore = useDriveStore()
  const { unsetModal } = useSetModal()
  const { getText } = useText()
  const inputBindings = useInputBindings()
  const dispatchAssetEvent = useDispatchAssetEvent()
  const canCreateAssets = useCanCreateAssets()
  const isAssetPanelVisible = useIsAssetPanelVisible()
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()
  const setIsAssetPanelPermanentlyVisible = useSetIsAssetPanelPermanentlyVisible()
  const createAssetButtonsRef = React.useRef<HTMLDivElement>(null)
  const isCloud = isCloudCategory(category)
  const { isOffline } = useOffline()
  const canDownload = useCanDownload()
  const shouldBeDisabled = (isCloud && isOffline) || !canCreateAssets
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
      canTransferBetweenCategories(pasteData.data.category, category)
    ) ?
      pasteData
    : null

  const getTargetDirectory = useEventCallback(() => driveStore.getState().targetDirectory)
  const rootDirectoryId = useRootDirectoryId(backend, category)

  const newFolderRaw = useNewFolder(backend, category)
  const newFolder = useEventCallback(async () => {
    const parent = getTargetDirectory()
    return await newFolderRaw(parent?.directoryId ?? rootDirectoryId, parent?.path)
  })
  const uploadFilesRaw = useUploadFiles(backend, category)
  const uploadFiles = useEventCallback(async (files: readonly File[]) => {
    const parent = getTargetDirectory()
    await uploadFilesRaw(files, parent?.directoryId ?? rootDirectoryId, parent?.path)
  })
  const newSecretRaw = useNewSecret(backend, category)
  const newSecret = useEventCallback(async (name: string, value: string) => {
    const parent = getTargetDirectory()
    return await newSecretRaw(name, value, parent?.directoryId ?? rootDirectoryId, parent?.path)
  })
  const newDatalinkRaw = useNewDatalink(backend, category)
  const newDatalink = useEventCallback(async (name: string, value: unknown) => {
    const parent = getTargetDirectory()
    return await newDatalinkRaw(name, value, parent?.directoryId ?? rootDirectoryId, parent?.path)
  })
  const newProjectRaw = useNewProject(backend, category)
  const newProjectMutation = useMutation({
    mutationKey: ['newProject'],
    mutationFn: async ([templateId, templateName]: [
      templateId: string | null | undefined,
      templateName: string | null | undefined,
    ]) => {
      const parent = getTargetDirectory()
      return await newProjectRaw(
        { templateName, templateId },
        parent?.directoryId ?? rootDirectoryId,
        parent?.path,
      )
    },
  })
  const newProject = newProjectMutation.mutateAsync
  const isCreatingProject = newProjectMutation.isPending

  React.useEffect(() => {
    return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
      ...(isCloud ?
        {
          newFolder: () => {
            void newFolder()
          },
        }
      : {}),
      newProject: () => {
        void newProject([null, null])
      },
      uploadFiles: () => {
        void inputFiles().then((files) => uploadFiles(Array.from(files)))
      },
    })
  }, [inputBindings, isCloud, newFolder, newProject, uploadFiles])

  const searchBar = (
    <AssetSearchBar backend={backend} isCloud={isCloud} query={query} setQuery={setQuery} />
  )

  const assetPanelToggle = (
    <>
      {/* Spacing. */}
      <div className={!isAssetPanelVisible ? 'w-5' : 'hidden'} />
      <div className="absolute right-[15px] top-[27px] z-1">
        <Button
          size="medium"
          variant="custom"
          isActive={isAssetPanelVisible}
          icon={RightPanelIcon}
          aria-label={isAssetPanelVisible ? getText('openAssetPanel') : getText('closeAssetPanel')}
          onPress={() => {
            const isAssetPanelTemporarilyVisible =
              driveStore.getState().isAssetPanelTemporarilyVisible
            if (isAssetPanelTemporarilyVisible) {
              setIsAssetPanelTemporarilyVisible(false)
              setIsAssetPanelPermanentlyVisible(false)
            } else {
              setIsAssetPanelPermanentlyVisible(!isAssetPanelVisible)
            }
          }}
        />
      </div>
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
        <ButtonGroup className="my-0.5 grow-0">
          {pasteDataStatus}
          {searchBar}
          {assetPanelToggle}
        </ButtonGroup>
      )
    }
    case 'trash': {
      return (
        <ButtonGroup className="my-0.5 grow-0">
          <DialogTrigger>
            <Button size="medium" variant="outline" isDisabled={shouldBeDisabled}>
              {getText('clearTrash')}
            </Button>
            <ConfirmDeleteModal
              actionText={getText('allTrashedItemsForever')}
              doDelete={doEmptyTrash}
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
        <ButtonGroup className="my-0.5 grow-0">
          <ButtonGroup
            ref={createAssetButtonsRef}
            className="grow-0"
            {...createAssetsVisualTooltip.targetProps}
          >
            <DialogTrigger
              defaultOpen={startModalDefaultOpen}
              onClose={() => {
                resetStartModalDefaultOpen(true)
              }}
            >
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
                  void newProject([templateId, templateName])
                }}
              />
            </DialogTrigger>
            <Button
              size="medium"
              variant="outline"
              isDisabled={shouldBeDisabled || isCreatingProject}
              icon={Plus2Icon}
              loaderPosition="icon"
              onPress={async () => {
                await newProject([null, null])
              }}
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
                onPress={async () => {
                  await newFolder()
                }}
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
                  const files = await inputFiles()
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
                  dispatchAssetEvent({ type: AssetEventType.downloadSelected })
                }}
              />
            </div>
            {createAssetsVisualTooltip.tooltip}
          </ButtonGroup>
          {pasteDataStatus}
          {searchBar}
          {assetPanelToggle}
        </ButtonGroup>
      )
    }
  }
}
