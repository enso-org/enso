/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import Button from '#/components/styled/Button'

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
  const inputBindings = inputBindingsProvider.useInputBindings()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const uploadFilesRef = React.useRef<HTMLInputElement>(null)
  const rootRef = React.useRef<HTMLDivElement>(null)
  const isCloud = backend.type === backendModule.BackendType.remote
  const effectiveCategory = isCloud ? category : Category.home
  const visibleButtonIds = React.useMemo(() => {
    switch (effectiveCategory) {
      case Category.recent: {
        return [] as const
      }
      case Category.trash: {
        return ['clearTrash'] as const
      }
      case Category.home: {
        if (isCloud) {
          return [
            'newProject',
            'newFolder',
            'newSecret',
            'newDataLink',
            'uploadFiles',
            ...(canDownload ? ['downloadFiles'] : []),
          ] as const
        } else {
          return ['newProject', 'uploadFiles', ...(canDownload ? ['downloadFiles'] : [])] as const
        }
      }
    }
  }, [canDownload, effectiveCategory, isCloud])

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, {
      axis: keyboardNavigationHooks.Axis.horizontal,
      length: visibleButtonIds.length,
    })
  const keyboardSelectedId =
    keyboardSelectedIndex == null ? null : visibleButtonIds[keyboardSelectedIndex]

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      return navigator2D.register(root, {
        focusPrimaryChild: setKeyboardSelectedIndex.bind(null, 0),
      })
    }
  }, [navigator2D, setKeyboardSelectedIndex])

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
        <aria.FocusScope>
          <div ref={rootRef} className="flex h-row py-drive-bar-y">
            <div className="flex gap-drive-bar" />
          </div>
        </aria.FocusScope>
      )
    }
    case Category.trash: {
      return (
        <aria.FocusScope>
          <div ref={rootRef} className="flex h-row py-drive-bar-y">
            <div className="flex gap-drive-bar">
              <button
                ref={element => {
                  if (keyboardSelectedId === 'clearTrash') {
                    element?.focus()
                  }
                }}
                className={`flex h-row items-center rounded-full bg-frame px-new-project-button-x ${keyboardSelectedId === 'clearTrash' ? 'focus-ring' : ''}`}
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
        </aria.FocusScope>
      )
    }
    case Category.home: {
      return (
        <aria.FocusScope>
          <div ref={rootRef} className="flex h-row py-drive-bar-y">
            <div className="flex gap-drive-bar">
              <button
                ref={element => {
                  if (keyboardSelectedId === 'newProject') {
                    element?.focus()
                  }
                }}
                className={`flex h-row items-center rounded-full bg-frame px-new-project-button-x ${keyboardSelectedId === 'newProject' ? 'focus-ring' : ''}`}
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
                    onPress={() => {
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
                    onPress={() => {
                      setModal(
                        <UpsertSecretModal id={null} name={null} doCreate={doCreateSecret} />
                      )
                    }}
                  />
                )}
                {isCloud && (
                  <Button
                    active
                    image={AddConnectorIcon}
                    alt="New Data Link"
                    onPress={() => {
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
                  onPress={() => {
                    unsetModal()
                    uploadFilesRef.current?.click()
                  }}
                />
                <Button
                  active={canDownload}
                  isDisabled={!canDownload}
                  image={DataDownloadIcon}
                  alt="Download Files"
                  error={
                    isCloud
                      ? 'You currently can only download files.'
                      : 'First select a project to download.'
                  }
                  onPress={() => {
                    unsetModal()
                    dispatchAssetEvent({ type: AssetEventType.downloadSelected })
                  }}
                />
              </div>
            </div>
          </div>
        </aria.FocusScope>
      )
    }
  }
}
