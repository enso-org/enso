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
import Button from './button'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
    doCreateProject: (templateId: string | null) => void
    doCreateDirectory: () => void
    doUploadFiles: (files: File[]) => void
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
    const { doCreateProject, doCreateDirectory, doUploadFiles, dispatchAssetEvent } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const uploadFilesRef = React.useRef<HTMLInputElement>(null)

    return (
        <div className="flex py-0.5">
            <div className="flex gap-2.5">
                <button
                    className="flex items-center bg-frame-bg rounded-full h-8 px-2.5"
                    onClick={() => {
                        unsetModal()
                        doCreateProject(null)
                    }}
                >
                    <span className="font-semibold leading-5 h-6 py-px">New Project</span>
                </button>
                <div className="flex items-center bg-frame-bg rounded-full gap-3 h-8 px-3">
                    {backend.type !== backendModule.BackendType.local && (
                        <>
                            <Button
                                active
                                image={AddFolderIcon}
                                onClick={() => {
                                    unsetModal()
                                    doCreateDirectory()
                                }}
                            />
                            <Button
                                active
                                disabled
                                image={AddConnectorIcon}
                                error="Not implemented yet."
                                onClick={() => {
                                    // No backend support yet.
                                }}
                            />
                        </>
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
                        active
                        image={DataUploadIcon}
                        onClick={() => {
                            unsetModal()
                            uploadFilesRef.current?.click()
                        }}
                    />
                    <Button
                        active
                        disabled={backend.type !== backendModule.BackendType.local}
                        image={DataDownloadIcon}
                        error="Not implemented yet."
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
