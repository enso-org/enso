/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as events from '#/events'
import * as categorySwitcherConstants from '#/layouts/dashboard/categorySwitcher/categorySwitcherConstants'
import NewDataConnectorModal from '#/layouts/dashboard/newDataConnectorModal'
import * as providers from '#/providers'
import * as backendModule from '#/services/backend'
import * as shortcutsModule from '#/util/shortcuts'

import Button from '#/components/button'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
    category: categorySwitcherConstants.Category
    doCreateProject: (templateId: string | null) => void
    doCreateDirectory: () => void
    doCreateDataConnector: (name: string, value: string) => void
    doUploadFiles: (files: File[]) => void
    dispatchAssetEvent: (event: events.AssetEvent) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
    const {
        category,
        doCreateProject,
        doCreateDirectory,
        doCreateDataConnector,
        doUploadFiles,
        dispatchAssetEvent,
    } = props
    const { backend } = providers.useBackend()
    const { setModal, unsetModal } = providers.useSetModal()
    const { shortcuts } = providers.useShortcuts()
    const uploadFilesRef = React.useRef<HTMLInputElement>(null)
    const isHomeCategory =
        category === categorySwitcherConstants.Category.home ||
        backend.type === backendModule.BackendType.local

    React.useEffect(() => {
        return shortcuts.registerKeyboardHandlers({
            ...(backend.type !== backendModule.BackendType.local
                ? {
                      [shortcutsModule.KeyboardAction.newFolder]: () => {
                          doCreateDirectory()
                      },
                  }
                : {}),
            [shortcutsModule.KeyboardAction.newProject]: () => {
                doCreateProject(null)
            },
            [shortcutsModule.KeyboardAction.uploadFiles]: () => {
                uploadFilesRef.current?.click()
            },
        })
    }, [backend.type, doCreateDirectory, doCreateProject, /* should never change */ shortcuts])

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
                        doCreateProject(null)
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
                            error="You can only create a new data connector in Home."
                            image={AddConnectorIcon}
                            disabledOpacityClassName="opacity-20"
                            onClick={event => {
                                event.stopPropagation()
                                setModal(<NewDataConnectorModal doCreate={doCreateDataConnector} />)
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
                        disabledOpacityClassName="opacity-20"
                        onClick={() => {
                            unsetModal()
                            uploadFilesRef.current?.click()
                        }}
                    />
                    <Button
                        active={
                            category !== categorySwitcherConstants.Category.trash &&
                            backend.type === backendModule.BackendType.local
                        }
                        disabled={
                            category === categorySwitcherConstants.Category.trash ||
                            backend.type !== backendModule.BackendType.local
                        }
                        image={DataDownloadIcon}
                        error={
                            category === categorySwitcherConstants.Category.trash
                                ? 'You cannot download files from Trash.'
                                : 'Not implemented yet.'
                        }
                        disabledOpacityClassName="opacity-20"
                        onClick={event => {
                            event.stopPropagation()
                            unsetModal()
                            dispatchAssetEvent({
                                type: events.AssetEventType.downloadSelected,
                            })
                        }}
                    />
                </div>
            </div>
        </div>
    )
}
