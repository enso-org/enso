/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/CategorySwitcher/Category'

import Button from '#/components/Button'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import UpsertDataLinkModal from '#/modals/UpsertDataLinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly category: Category
  readonly isCloud: boolean
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
  const { category, isCloud, canDownload, doEmptyTrash, doCreateProject, doCreateDirectory } = props
  const { doCreateSecret, doCreateDataLink, doUploadFiles, dispatchAssetEvent } = props
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const effectiveCategory = isCloud ? category : Category.home

  React.useEffect(() => {
    return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
      ...(isCloud
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
  }, [isCloud, doCreateDirectory, doCreateProject, /* should never change */ inputBindings])

  switch (effectiveCategory) {
    case Category.recent: {
      // It is INCORRECT to have a "New Project" button here as it requires a full list of projects
      // in the given directory, to avoid name collisions.
      return (
        <div className="flex h-row py-drive-bar-y">
          <div className="flex gap-drive-bar" />
        </div>
      )
    }
    case Category.trash: {
      return (
        <div className="flex h-row py-drive-bar-y">
          <div className="flex gap-drive-bar">
            <button
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onClick={event => {
                event.stopPropagation()
                setModal(
                  <ConfirmDeleteModal
                    actionText="all trashed items forever"
                    doDelete={doEmptyTrash}
                  />
                )
              }}
            >
              <span className="text whitespace-nowrap font-semibold">Clear Trash</span>
            </button>
          </div>
        </div>
      )
    }
    case Category.home: {
      return (
        <div className="flex h-row py-drive-bar-y">
          <div className="flex gap-drive-bar">
            <button
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onClick={() => {
                unsetModal()
                doCreateProject()
              }}
            >
              <span className="text whitespace-nowrap font-semibold">New Project</span>
            </button>
            <div className="flex h-row items-center gap-icons rounded-full bg-frame px-drive-bar-icons-x text-black/50">
              {isCloud && (
                <Button
                  active
                  image={AddFolderIcon}
                  alt="New Folder"
                  onClick={() => {
                    unsetModal()
                    doCreateDirectory()
                  }}
                />
              )}
              {isCloud && (
                <Button
                  active
                  image={AddKeyIcon}
                  alt="New Secret"
                  onClick={event => {
                    event.stopPropagation()
                    setModal(<UpsertSecretModal id={null} name={null} doCreate={doCreateSecret} />)
                  }}
                />
              )}
              {isCloud && (
                <Button
                  active
                  image={AddConnectorIcon}
                  alt="New Data Link"
                  onClick={event => {
                    event.stopPropagation()
                    setModal(<UpsertDataLinkModal doCreate={doCreateDataLink} />)
                  }}
                />
              )}
              <input
                ref={uploadFilesRef}
                type="file"
                multiple
                id="upload_files_input"
                name="upload_files_input"
                {...(isCloud ? {} : { accept: '.enso-project' })}
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
                alt="Upload Files"
                onClick={() => {
                  unsetModal()
                  uploadFilesRef.current?.click()
                }}
              />
              <Button
                active={canDownload}
                disabled={!canDownload}
                image={DataDownloadIcon}
                alt="Download Files"
                error={
                  isCloud
                    ? 'You currently can only download files.'
                    : 'First select a project to download.'
                }
                onClick={event => {
                  event.stopPropagation()
                  unsetModal()
                  dispatchAssetEvent({ type: AssetEventType.downloadSelected })
                }}
              />
            </div>
          </div>
        </div>
      )
    }
  }
}
