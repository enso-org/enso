/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import { skipToken, useQuery } from '@tanstack/react-query'

import AddDatalinkIcon from '#/assets/add_datalink.svg'
import AddFolderIcon from '#/assets/add_folder.svg'
import AddKeyIcon from '#/assets/add_key.svg'
import DataDownloadIcon from '#/assets/data_download.svg'
import DataUploadIcon from '#/assets/data_upload.svg'
import Plus2Icon from '#/assets/plus2.svg'
import RightPanelIcon from '#/assets/right_panel.svg'

import * as offlineHooks from '#/hooks/offlineHooks'
import { createGetProjectDetailsQuery } from '#/hooks/projectHooks'

import * as authProvider from '#/providers/AuthProvider'
import { useCanDownload, useTargetDirectory } from '#/providers/DriveProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'

import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetSearchBar from '#/layouts/AssetSearchBar'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import {
  isCloudCategory,
  isLocalCategory,
  type Category,
} from '#/layouts/CategorySwitcher/Category'
import StartModal from '#/layouts/StartModal'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import type Backend from '#/services/Backend'
import {
  Plan,
  ProjectState,
  type CreatedProject,
  type Project,
  type ProjectId,
} from '#/services/Backend'

import type AssetQuery from '#/utilities/AssetQuery'
import {
  canPermissionModifyDirectoryContents,
  tryFindSelfPermission,
} from '#/utilities/permissions'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly backend: Backend
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly suggestions: readonly assetSearchBar.Suggestion[]
  readonly category: Category
  readonly isAssetPanelOpen: boolean
  readonly setIsAssetPanelOpen: React.Dispatch<React.SetStateAction<boolean>>
  readonly doEmptyTrash: () => void
  readonly doCreateProject: (
    templateId?: string | null,
    templateName?: string | null,
    onCreated?: (project: CreatedProject) => void,
    onError?: () => void,
  ) => void
  readonly doCreateDirectory: () => void
  readonly doCreateSecret: (name: string, value: string) => void
  readonly doCreateDatalink: (name: string, value: unknown) => void
  readonly doUploadFiles: (files: File[]) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
  const { backend, query, setQuery, suggestions, category } = props
  const { doEmptyTrash, doCreateProject, doCreateDirectory } = props
  const { doCreateSecret, doCreateDatalink, doUploadFiles } = props
  const { isAssetPanelOpen, setIsAssetPanelOpen } = props
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const { user } = authProvider.useFullUserSession()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const targetDirectory = useTargetDirectory()
  const createAssetButtonsRef = React.useRef<HTMLDivElement>(null)
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const isCloud = isCloudCategory(category)
  const { isOffline } = offlineHooks.useOffline()
  const canDownload = useCanDownload()
  const targetDirectorySelfPermission =
    targetDirectory == null ? null : tryFindSelfPermission(user, targetDirectory.item.permissions)
  const canCreateAssets =
    targetDirectory == null ?
      category.type !== 'cloud' || user.plan == null || user.plan === Plan.solo
    : isLocalCategory(category) ||
      (targetDirectorySelfPermission != null &&
        canPermissionModifyDirectoryContents(targetDirectorySelfPermission.permission))
  const shouldBeDisabled = (isCloud && isOffline) || !canCreateAssets
  const error =
    !shouldBeDisabled ? null
    : isCloud && isOffline ? getText('youAreOffline')
    : getText('cannotCreateAssetsHere')
  const createAssetsVisualTooltip = ariaComponents.useVisualTooltip({
    isDisabled: error == null,
    children: error,
    targetRef: createAssetButtonsRef,
    overlayPositionProps: { placement: 'top' },
  })
  const [isCreatingProjectFromTemplate, setIsCreatingProjectFromTemplate] = React.useState(false)
  const [isCreatingProject, setIsCreatingProject] = React.useState(false)
  const [createdProjectId, setCreatedProjectId] = React.useState<ProjectId | null>(null)

  React.useEffect(() => {
    return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
      ...(isCloud ?
        {
          newFolder: () => {
            doCreateDirectory()
          },
        }
      : {}),
      newProject: () => {
        setIsCreatingProject(true)
        doCreateProject(
          null,
          null,
          (project) => {
            setCreatedProjectId(project.projectId)
          },
          () => {
            setIsCreatingProject(false)
          },
        )
      },
      uploadFiles: () => {
        uploadFilesRef.current?.click()
      },
    })
  }, [isCloud, doCreateDirectory, doCreateProject, inputBindings])

  const createdProjectQuery = useQuery<Project | null>(
    createdProjectId ?
      createGetProjectDetailsQuery.createPassiveListener(createdProjectId)
    : { queryKey: ['__IGNORE__'], queryFn: skipToken },
  )

  const isFetching =
    (createdProjectQuery.isLoading ||
      (createdProjectQuery.data &&
        createdProjectQuery.data.state.type !== ProjectState.opened &&
        createdProjectQuery.data.state.type !== ProjectState.closing)) ??
    false

  React.useEffect(() => {
    if (!isFetching) {
      setIsCreatingProject(false)
      setIsCreatingProjectFromTemplate(false)
    }
  }, [isFetching])

  const searchBar = (
    <AssetSearchBar
      backend={backend}
      isCloud={isCloud}
      query={query}
      setQuery={setQuery}
      suggestions={suggestions}
    />
  )

  const assetPanelToggle = (
    <>
      {/* Spacing. */}
      <div className={!isAssetPanelOpen ? 'w-5' : 'hidden'} />
      <div className="absolute right-[15px] top-[27px] z-1">
        <ariaComponents.Button
          size="medium"
          variant="custom"
          isActive={isAssetPanelOpen}
          icon={RightPanelIcon}
          aria-label={isAssetPanelOpen ? getText('openAssetPanel') : getText('closeAssetPanel')}
          onPress={() => {
            setIsAssetPanelOpen((isOpen) => !isOpen)
          }}
        />
      </div>
    </>
  )

  switch (category.type) {
    case 'recent': {
      return (
        <ariaComponents.ButtonGroup className="my-0.5 grow-0">
          {searchBar}
          {assetPanelToggle}
        </ariaComponents.ButtonGroup>
      )
    }
    case 'trash': {
      return (
        <ariaComponents.ButtonGroup className="my-0.5 grow-0">
          <ariaComponents.Button
            size="medium"
            variant="outline"
            isDisabled={shouldBeDisabled}
            onPress={() => {
              setModal(
                <ConfirmDeleteModal
                  actionText={getText('allTrashedItemsForever')}
                  doDelete={doEmptyTrash}
                />,
              )
            }}
          >
            {getText('clearTrash')}
          </ariaComponents.Button>
          {searchBar}
          {assetPanelToggle}
        </ariaComponents.ButtonGroup>
      )
    }
    case 'cloud':
    case 'local':
    case 'user':
    case 'team':
    case 'local-directory': {
      return (
        <ariaComponents.ButtonGroup className="my-0.5 grow-0">
          <ariaComponents.ButtonGroup
            ref={createAssetButtonsRef}
            className="grow-0"
            {...createAssetsVisualTooltip.targetProps}
          >
            <aria.DialogTrigger>
              <ariaComponents.Button
                size="medium"
                variant="accent"
                isDisabled={shouldBeDisabled || isCreatingProject || isCreatingProjectFromTemplate}
                icon={Plus2Icon}
                loading={isCreatingProjectFromTemplate}
                loaderPosition="icon"
              >
                {getText('startWithATemplate')}
              </ariaComponents.Button>

              <StartModal
                createProject={(templateId, templateName) => {
                  setIsCreatingProjectFromTemplate(true)
                  doCreateProject(
                    templateId,
                    templateName,
                    (project) => {
                      setCreatedProjectId(project.projectId)
                    },
                    () => {
                      setIsCreatingProjectFromTemplate(false)
                    },
                  )
                }}
              />
            </aria.DialogTrigger>
            <ariaComponents.Button
              size="medium"
              variant="outline"
              isDisabled={shouldBeDisabled || isCreatingProject || isCreatingProjectFromTemplate}
              icon={Plus2Icon}
              loading={isCreatingProject}
              loaderPosition="icon"
              onPress={() => {
                setIsCreatingProject(true)
                doCreateProject(
                  null,
                  null,
                  (project) => {
                    setCreatedProjectId(project.projectId)
                    setIsCreatingProject(false)
                  },
                  () => {
                    setIsCreatingProject(false)
                  },
                )
              }}
            >
              {getText('newEmptyProject')}
            </ariaComponents.Button>
            <div className="flex h-row items-center gap-4 rounded-full border-0.5 border-primary/20 px-[11px]">
              <ariaComponents.Button
                variant="icon"
                size="medium"
                icon={AddFolderIcon}
                isDisabled={shouldBeDisabled}
                aria-label={getText('newFolder')}
                onPress={() => {
                  doCreateDirectory()
                }}
              />
              {isCloud && (
                <ariaComponents.Button
                  variant="icon"
                  size="medium"
                  icon={AddKeyIcon}
                  isDisabled={shouldBeDisabled}
                  aria-label={getText('newSecret')}
                  onPress={() => {
                    setModal(<UpsertSecretModal id={null} name={null} doCreate={doCreateSecret} />)
                  }}
                />
              )}
              {isCloud && (
                <ariaComponents.Button
                  variant="icon"
                  size="medium"
                  icon={AddDatalinkIcon}
                  isDisabled={shouldBeDisabled}
                  aria-label={getText('newDatalink')}
                  onPress={() => {
                    setModal(<UpsertDatalinkModal doCreate={doCreateDatalink} />)
                  }}
                />
              )}
              <aria.Input
                ref={uploadFilesRef}
                type="file"
                multiple
                className="hidden"
                onInput={(event) => {
                  if (event.currentTarget.files != null) {
                    doUploadFiles(Array.from(event.currentTarget.files))
                  }
                  // Clear the list of selected files, otherwise `onInput` will not be
                  // dispatched again if the same file is selected.
                  event.currentTarget.value = ''
                }}
              />
              <ariaComponents.Button
                variant="icon"
                size="medium"
                icon={DataUploadIcon}
                isDisabled={shouldBeDisabled}
                aria-label={getText('uploadFiles')}
                onPress={() => {
                  unsetModal()
                  uploadFilesRef.current?.click()
                }}
              />
              <ariaComponents.Button
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
          </ariaComponents.ButtonGroup>
          {searchBar}
          {assetPanelToggle}
        </ariaComponents.ButtonGroup>
      )
    }
  }
}
