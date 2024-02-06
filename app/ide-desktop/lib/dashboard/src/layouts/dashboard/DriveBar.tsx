/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import UpsertSecretModal from '#/layouts/dashboard/UpsertSecretModal'

import Button from '#/components/Button'

import * as backendModule from '#/services/Backend'

import * as shortcutManagerModule from '#/utilities/ShortcutManager'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  category: Category
  canDownloadFiles: boolean
  doCreateProject: () => void
  doCreateDirectory: () => void
  doCreateSecret: (name: string, value: string) => void
  doUploadFiles: (files: File[]) => void
  dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
  const { category, canDownloadFiles, doCreateProject, doCreateDirectory } = props
  const { doCreateSecret, doUploadFiles, dispatchAssetEvent } = props
  const { backend } = backendProvider.useBackend()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { shortcutManager } = shortcutManagerProvider.useShortcutManager()
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const isCloud = backend.type === backendModule.BackendType.remote
  const isHomeCategory = category === Category.home || !isCloud

  React.useEffect(() => {
    return shortcutManager.registerKeyboardHandlers({
      ...(backend.type !== backendModule.BackendType.local
        ? {
            [shortcutManagerModule.KeyboardAction.newFolder]: () => {
              doCreateDirectory()
            },
          }
        : {}),
      [shortcutManagerModule.KeyboardAction.newProject]: () => {
        doCreateProject()
      },
      [shortcutManagerModule.KeyboardAction.uploadFiles]: () => {
        uploadFilesRef.current?.click()
      },
    })
  }, [backend.type, doCreateDirectory, doCreateProject, /* should never change */ shortcutManager])

  return (
    <div className="flex h-8 py-0.5">
      <div className="flex gap-2.5">
        <button
          disabled={!isHomeCategory}
          className="flex items-center bg-frame rounded-full h-8 px-2.5"
          {...(!isHomeCategory
            ? {
                title: 'You can only create a new project in Home.',
              }
            : {})}
          onClick={() => {
            unsetModal()
            doCreateProject()
          }}
        >
          <span
            className={`font-semibold whitespace-nowrap leading-5 h-6 py-px ${
              !isHomeCategory ? 'opacity-50' : ''
            }`}
          >
            New Project
          </span>
        </button>
        <div className="flex items-center text-black/50 bg-frame rounded-full gap-3 h-8 px-3">
          {backend.type !== backendModule.BackendType.local && (
            <Button
              active={isHomeCategory}
              disabled={!isHomeCategory}
              error="You can only create a new folder in Home."
              image={AddFolderIcon}
              alt="New Folder"
              disabledOpacityClassName="opacity-20"
              onClick={() => {
                unsetModal()
                doCreateDirectory()
              }}
            />
          )}
          {backend.type !== backendModule.BackendType.local && (
            <Button
              active={isHomeCategory}
              disabled={!isHomeCategory}
              error="You can only create a secret in Home."
              image={AddConnectorIcon}
              alt="New Secret"
              disabledOpacityClassName="opacity-20"
              onClick={event => {
                event.stopPropagation()
                setModal(<UpsertSecretModal id={null} name={null} doCreate={doCreateSecret} />)
              }}
            />
          )}
          <input
            ref={uploadFilesRef}
            type="file"
            multiple
            id="upload_files_input"
            name="upload_files_input"
            {...(backend.type !== backendModule.BackendType.local
              ? {}
              : { accept: '.enso-project' })}
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
            active={isHomeCategory}
            disabled={!isHomeCategory}
            error="You can only upload files to Home."
            image={DataUploadIcon}
            alt="Upload Files"
            disabledOpacityClassName="opacity-20"
            onClick={() => {
              unsetModal()
              uploadFilesRef.current?.click()
            }}
          />
          <Button
            active={canDownloadFiles}
            disabled={!canDownloadFiles}
            image={DataDownloadIcon}
            alt="Download Files"
            error={
              category === Category.trash
                ? 'You cannot download files from Trash.'
                : 'You currently can only download files.'
            }
            disabledOpacityClassName="opacity-20"
            onClick={event => {
              event.stopPropagation()
              unsetModal()
              dispatchAssetEvent({
                type: AssetEventType.downloadSelected,
              })
            }}
          />
        </div>
      </div>
    </div>
  )
}
