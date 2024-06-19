/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddDatalinkIcon from 'enso-assets/add_datalink.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import StartModal from '#/layouts/StartModal'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Button from '#/components/styled/Button'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

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
  readonly doCreateDatalink: (name: string, value: unknown) => void
  readonly doUploadFiles: (files: File[]) => void
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
  const { category, canDownload, doEmptyTrash, doCreateProject, doCreateDirectory } = props
  const { doCreateSecret, doCreateDatalink, doUploadFiles, dispatchAssetEvent } = props
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const isCloud = categoryModule.isCloud(category)

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

  switch (category) {
    case Category.recent: {
      // It is INCORRECT to have a "New Project" button here as it requires a full list of projects
      // in the given directory, to avoid name collisions.
      return <ariaComponents.ButtonGroup className="grow-0" />
    }
    case Category.trash: {
      return (
        <ariaComponents.ButtonGroup className="grow-0">
          <ariaComponents.Button
            variant="bar"
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
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      )
    }
    case Category.cloud:
    case Category.local: {
      return (
        <ariaComponents.ButtonGroup className="grow-0">
          <aria.DialogTrigger>
            <ariaComponents.Button
              size="medium"
              variant="tertiary"
              className="px-2.5"
              onPress={() => {}}
            >
              {getText('startWithATemplate')}
            </ariaComponents.Button>
            <StartModal createProject={doCreateProject} />
          </aria.DialogTrigger>
          <ariaComponents.Button
            size="medium"
            variant="bar"
            onPress={() => {
              doCreateProject()
            }}
          >
            {getText('newEmptyProject')}
          </ariaComponents.Button>
          <div className="flex h-row items-center gap-icons rounded-full border-0.5 border-primary/20 px-drive-bar-icons-x text-primary/50">
            <Button
              active
              image={AddFolderIcon}
              alt={getText('newFolder')}
              onPress={() => {
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
                image={AddDatalinkIcon}
                alt={getText('newDatalink')}
                onPress={() => {
                  setModal(<UpsertDatalinkModal doCreate={doCreateDatalink} />)
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
        </ariaComponents.ButtonGroup>
      )
    }
  }
}
