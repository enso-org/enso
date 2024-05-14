/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddDataLinkIcon from 'enso-assets/add_datalink.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import Button from '#/components/styled/Button'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import UnstyledButton from '#/components/UnstyledButton'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import UpsertDataLinkModal from '#/modals/UpsertDataLinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly category: Category
  readonly canDownload: boolean
  readonly doEmptyTrash: () => void
  readonly doCreateProject: () => void
  readonly doCreateDirectory: () => void
  readonly doCreateSecret: (name: string, value: string) => void
  readonly doCreateDataLink: (name: string, value: unknown) => void
  readonly doUploadFiles: (files: File[]) => void
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
  const { category, canDownload, doEmptyTrash, doCreateProject, doCreateDirectory } = props
  const { doCreateSecret, doCreateDataLink, doUploadFiles, dispatchAssetEvent } = props
  const { backend } = backendProvider.useBackend()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const isCloud = backend.type === backendModule.BackendType.remote
  const effectiveCategory = isCloud ? category : Category.home

  React.useEffect(() => {
    return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
      ...(backend.type !== backendModule.BackendType.local
        ? {
            newFolder: () => {
              doCreateDirectory()
            },
          }
        : {}),
      newProject: () => {
        doCreateProject()
      },
      uploadFiles: () => {
        uploadFilesRef.current?.click()
      },
    })
  }, [backend.type, doCreateDirectory, doCreateProject, /* should never change */ inputBindings])

  switch (effectiveCategory) {
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
            <UnstyledButton
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onPress={() => {
                setModal(
                  <ConfirmDeleteModal
                    actionText={getText('allTrashedItemsForever')}
                    doDelete={doEmptyTrash}
                  />
                )
              }}
            >
              <aria.Text className="text whitespace-nowrap font-semibold">
                {getText('clearTrash')}
              </aria.Text>
            </UnstyledButton>
          </HorizontalMenuBar>
        </div>
      )
    }
    case Category.home: {
      return (
        <div className="flex h-row py-drive-bar-y">
          <HorizontalMenuBar>
            <UnstyledButton
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onPress={() => {
                unsetModal()
                doCreateProject()
              }}
            >
              <aria.Text className="text whitespace-nowrap font-semibold">
                {getText('newProject')}
              </aria.Text>
            </UnstyledButton>
            <div className="flex h-row items-center gap-icons rounded-full bg-frame px-drive-bar-icons-x text-black/50">
              <Button
                active
                image={AddFolderIcon}
                alt={getText('newFolder')}
                onPress={() => {
                  unsetModal()
                  doCreateDirectory()
                }}
              />
              {isCloud && (
                <Button
                  active
                  image={AddKeyIcon}
                  alt={getText('newSecret')}
                  onPress={() => {
                    setModal(<UpsertSecretModal id={null} name={null} doCreate={doCreateSecret} />)
                  }}
                />
              )}
              {isCloud && (
                <Button
                  active
                  image={AddDataLinkIcon}
                  alt={getText('newDataLink')}
                  onPress={() => {
                    setModal(<UpsertDataLinkModal doCreate={doCreateDataLink} />)
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
                    doUploadFiles(Array.from(event.currentTarget.files))
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
                  unsetModal()
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
                  unsetModal()
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
