/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import AddDatalinkIcon from 'enso-assets/add_datalink.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as backendHooks from '#/hooks/backendHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import StartModal from '#/layouts/StartModal'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Button from '#/components/styled/Button'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import CreateDatalinkModal from '#/modals/CreateDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly backend: Backend
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly targetDirectoryIdRef: Readonly<React.MutableRefObject<backendModule.DirectoryId>>
  readonly category: Category
  readonly canDownload: boolean
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
  const { backend, rootDirectoryId, targetDirectoryIdRef, category, canDownload } = props
  const queryClient = reactQuery.useQueryClient()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const isCloud = categoryModule.isCloud(category)

  const createDirectoryMutation = backendHooks.useBackendCreateDirectoryMutation(backend)
  const createProjectMutation = backendHooks.useBackendCreateProjectMutation(backend)
  const uploadFilesMutation = backendHooks.useBackendUploadFilesMutation(backend)
  const deleteAssetMutation = backendHooks.useBackendMutation(backend, 'deleteAsset')
  const createDirectoryMutate = createDirectoryMutation.mutate
  const createProjectMutate = createProjectMutation.mutate

  React.useEffect(() => {
    return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
      ...(isCloud
        ? {
            newFolder: () => {
              createDirectoryMutate([
                { title: 'New Folder', parentId: targetDirectoryIdRef.current },
              ])
            },
          }
        : {}),
      newProject: () => {
        createProjectMutate([
          { projectName: 'New Project', parentDirectoryId: targetDirectoryIdRef.current },
        ])
      },
      uploadFiles: () => {
        uploadFilesRef.current?.click()
      },
    })
  }, [
    isCloud,
    createDirectoryMutate,
    createProjectMutate,
    targetDirectoryIdRef,
    /* should never change */ inputBindings,
  ])

  switch (category) {
    case Category.recent: {
      // It is INCORRECT to have a "New Project" button here as it requires a full list of projects
      // in the given directory, to avoid name collisions.
      return (
        <div className="flex h-row py-drive-bar-y">
          <HorizontalMenuBar />
        </div>
      )
    }
    case Category.trash: {
      return (
        <div className="flex h-row py-drive-bar-y">
          <HorizontalMenuBar>
            <ariaComponents.Button
              size="custom"
              variant="custom"
              className="flex h-row items-center rounded-full border-0.5 border-primary/20 px-new-project-button-x transition-colors hover:bg-primary/10"
              onPress={() => {
                setModal(
                  <ConfirmDeleteModal
                    actionText={getText('allTrashedItemsForever')}
                    doDelete={async () => {
                      const assetsInTrash = await backendHooks.ensureBackendQueryData(
                        queryClient,
                        backend,
                        'listDirectory',
                        [
                          {
                            filterBy: backendModule.FilterBy.trashed,
                            parentId: null,
                            labels: [],
                            recentProjects: false,
                          },
                        ]
                      )
                      for (const asset of assetsInTrash) {
                        deleteAssetMutation.mutate([
                          asset.id,
                          { force: true, parentId: asset.parentId },
                        ])
                      }
                    }}
                  />
                )
              }}
            >
              <aria.Text className="text whitespace-nowrap font-semibold">
                {getText('clearTrash')}
              </aria.Text>
            </ariaComponents.Button>
          </HorizontalMenuBar>
        </div>
      )
    }
    case Category.cloud:
    case Category.local: {
      return (
        <div className="flex h-row py-drive-bar-y">
          <HorizontalMenuBar>
            <aria.DialogTrigger>
              <ariaComponents.Button
                size="custom"
                variant="custom"
                className="relative flex h-row items-center rounded-full px-new-project-button-x text-white before:absolute before:inset before:rounded-full before:bg-accent before:transition-all hover:before:brightness-90"
                onPress={() => {}}
              >
                <aria.Text className="text relative whitespace-nowrap font-bold">
                  {getText('startWithATemplate')}
                </aria.Text>
              </ariaComponents.Button>
              <StartModal backend={backend} rootDirectoryId={rootDirectoryId} />
            </aria.DialogTrigger>
            <ariaComponents.Button
              size="custom"
              variant="bar"
              onPress={() => {
                createProjectMutation.mutate([
                  { projectName: 'New Project', parentDirectoryId: rootDirectoryId },
                ])
              }}
            >
              <aria.Text className="text whitespace-nowrap font-bold">
                {getText('newEmptyProject')}
              </aria.Text>
            </ariaComponents.Button>
            <div className="flex h-row items-center gap-icons rounded-full border-0.5 border-primary/20 px-drive-bar-icons-x text-primary/50">
              <Button
                active
                image={AddFolderIcon}
                alt={getText('newFolder')}
                onPress={() => {
                  createDirectoryMutation.mutate([
                    { title: 'New Folder', parentId: rootDirectoryId },
                  ])
                }}
              />
              {isCloud && (
                <Button
                  active
                  image={AddKeyIcon}
                  alt={getText('newSecret')}
                  onPress={() => {
                    setModal(
                      <UpsertSecretModal
                        backend={backend}
                        asset={null}
                        parentDirectoryId={targetDirectoryIdRef.current}
                      />
                    )
                  }}
                />
              )}
              {isCloud && (
                <Button
                  active
                  image={AddDatalinkIcon}
                  alt={getText('newDatalink')}
                  onPress={() => {
                    setModal(
                      <CreateDatalinkModal
                        backend={backend}
                        parentDirectoryId={targetDirectoryIdRef.current}
                      />
                    )
                  }}
                />
              )}
              <aria.Input
                ref={uploadFilesRef}
                type="file"
                multiple
                id="upload_files_input"
                name="upload_files_input"
                className="hidden"
                onInput={event => {
                  if (event.currentTarget.files != null) {
                    uploadFilesMutation.mutate([event.currentTarget.files, rootDirectoryId])
                  }
                  // Clear the list of selected files. Otherwise, `onInput` will not be
                  // dispatched again if the same file is selected.
                  event.currentTarget.value = ''
                }}
              />
              <Button
                active
                image={DataUploadIcon}
                alt={getText('uploadFiles')}
                onPress={() => {
                  uploadFilesRef.current?.click()
                }}
              />
              <Button
                active={canDownload}
                isDisabled={!canDownload}
                image={DataDownloadIcon}
                alt={getText('downloadFiles')}
                error={
                  isCloud ? getText('canOnlyDownloadFilesError') : getText('noProjectSelectedError')
                }
                onPress={() => {
                  dispatchAssetEvent({ type: AssetEventType.downloadSelected })
                }}
              />
            </div>
          </HorizontalMenuBar>
        </div>
      )
    }
  }
}
