/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'
import RightPanelIcon from 'enso-assets/right_panel.svg'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetSearchBar from '#/layouts/AssetSearchBar'
import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import Start from '#/layouts/Start'

import * as aria from '#/components/aria'
import Button from '#/components/styled/Button'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import UnstyledButton from '#/components/UnstyledButton'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import UpsertDataLinkModal from '#/modals/UpsertDataLinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import type * as backend from '#/services/Backend'

import type AssetQuery from '#/utilities/AssetQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly labels: readonly backend.Label[]
  readonly suggestions: readonly assetSearchBar.Suggestion[]
  readonly category: Category
  readonly canDownload: boolean
  readonly isAssetPanelOpen: boolean
  readonly setIsAssetPanelOpen: React.Dispatch<React.SetStateAction<boolean>>
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
  const { query, setQuery, labels, suggestions, category, canDownload } = props
  const { doEmptyTrash, doCreateProject, doCreateDirectory } = props
  const { doCreateSecret, doCreateDataLink, doUploadFiles, dispatchAssetEvent } = props
  const { isAssetPanelOpen, setIsAssetPanelOpen } = props
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
              className="flex h-row items-center rounded-full border-0.5 border-primary/20 px-new-project-button-x transition-colors hover:bg-primary/10"
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
    case Category.cloud:
    case Category.local: {
      return (
        <div className="flex h-row py-drive-bar-y">
          <HorizontalMenuBar grow>
            <aria.DialogTrigger>
              <UnstyledButton variant="accent" onPress={() => {}}>
                <aria.Text className="text relative whitespace-nowrap font-bold">
                  {getText('startWithATemplate')}
                </aria.Text>
              </UnstyledButton>
              <Start createProject={doCreateProject} />
            </aria.DialogTrigger>
            <UnstyledButton
              variant="regular"
              onPress={() => {
                doCreateProject()
              }}
            >
              <aria.Text className="text whitespace-nowrap font-bold">
                {getText('newEmptyProject')}
              </aria.Text>
            </UnstyledButton>
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
                  image={AddConnectorIcon}
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
            <AssetSearchBar
              isCloud={isCloud}
              query={query}
              setQuery={setQuery}
              labels={labels}
              suggestions={suggestions}
              className="mx-auto"
            />
            <div
              className={`transition-all duration-side-panel ${isAssetPanelOpen ? '' : 'w-5'}`}
            />
            <div
              className={`absolute right top z-1 m-[15px] transition-all duration-side-panel ${isAssetPanelOpen ? '' : 'mt-[26px]'}`}
            >
              <Button
                image={RightPanelIcon}
                active={isAssetPanelOpen}
                onPress={() => {
                  setIsAssetPanelOpen(isOpen => !isOpen)
                }}
              />
            </div>
          </HorizontalMenuBar>
        </div>
      )
    }
  }
}
