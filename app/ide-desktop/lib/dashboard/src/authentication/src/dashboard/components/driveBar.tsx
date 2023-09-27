/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'
import * as shortcutsModule from '../shortcuts'
import * as shortcutsProvider from '../../providers/shortcuts'

import * as categorySwitcher from './categorySwitcher'
import Button from './button'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
    category: categorySwitcher.Category
    doCreateProject: (templateId: string | null) => void
    doCreateDirectory: () => void
    doUploadFiles: (files: File[]) => void
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
    const { category, doCreateProject, doCreateDirectory, doUploadFiles, dispatchAssetEvent } =
        props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const uploadFilesRef = React.useRef<HTMLInputElement>(null)

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
                    disabled={category === categorySwitcher.Category.trash}
                    className="flex items-center bg-frame rounded-full h-8 px-2.5"
                    {...(category === categorySwitcher.Category.trash
                        ? {
                              title: 'Cannot create a new project in Trash.',
                          }
                        : {})}
                    onClick={() => {
                        unsetModal()
                        doCreateProject(null)
                    }}
                >
                    <span
                        className={`font-semibold whitespace-nowrap leading-5 h-6 py-px ${
                            category === categorySwitcher.Category.trash ? 'opacity-50' : ''
                        }`}
                    >
                        New Project
                    </span>
                </button>
                <div className="flex items-center text-black-a50 bg-frame rounded-full gap-3 h-8 px-3">
                    {backend.type !== backendModule.BackendType.local && (
                        <Button
                            active={category !== categorySwitcher.Category.trash}
                            disabled={category === categorySwitcher.Category.trash}
                            error="Cannot create a new folder in Trash."
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
                            disabled
                            image={AddConnectorIcon}
                            error={
                                category === categorySwitcher.Category.trash
                                    ? 'Cannot create a new data connector in Trash.'
                                    : 'Not implemented yet.'
                            }
                            disabledOpacityClassName="opacity-20"
                            onClick={() => {
                                // No backend support yet.
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
                        active={category !== categorySwitcher.Category.trash}
                        disabled={category === categorySwitcher.Category.trash}
                        error="Cannot upload files to Trash."
                        image={DataUploadIcon}
                        disabledOpacityClassName="opacity-20"
                        onClick={() => {
                            unsetModal()
                            uploadFilesRef.current?.click()
                        }}
                    />
                    <Button
                        active={
                            category !== categorySwitcher.Category.trash &&
                            backend.type === backendModule.BackendType.local
                        }
                        disabled={
                            category === categorySwitcher.Category.trash ||
                            backend.type !== backendModule.BackendType.local
                        }
                        image={DataDownloadIcon}
                        error={
                            category === categorySwitcher.Category.trash
                                ? 'Cannot download files from Trash.'
                                : 'Not implemented yet.'
                        }
                        disabledOpacityClassName="opacity-20"
                        onClick={event => {
                            event.stopPropagation()
                            unsetModal()
                            dispatchAssetEvent({
                                type: assetEventModule.AssetEventType.downloadSelected,
                            })
                        }}
                    />
                </div>
            </div>
        </div>
    )
}
