/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import DownloadIcon from 'enso-assets/download.svg'
import UploadIcon from 'enso-assets/upload.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
    doUploadFiles: (files: FileList) => void
    doCreateProject: () => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
export default function DriveBar(props: DriveBarProps) {
    const { doUploadFiles: doUploadFilesRaw, doCreateProject } = props
    const { backend } = backendProvider.useBackend()

    const doUploadFiles = React.useCallback(
        (event: React.FormEvent<HTMLInputElement>) => {
            if (event.currentTarget.files != null) {
                doUploadFilesRaw(event.currentTarget.files)
            }
        },
        [/* should never change */ doUploadFilesRaw]
    )

    return (
        <div className="flex flex-row flex-nowrap my-2">
            <div className="flex flex-row flex-nowrap mx-4">
                <button
                    className="flex items-center bg-white rounded-full h-8 px-2.5"
                    onClick={doCreateProject}
                >
                    <span className="h-6 py-px">New Project</span>
                </button>
                <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap px-1.5 py-1 mx-4">
                    <input
                        type="file"
                        multiple
                        disabled={backend.type === backendModule.BackendType.local}
                        id="upload_files_input"
                        name="upload_files_input"
                        className="w-0 h-0"
                        onInput={doUploadFiles}
                    />
                    <label
                        htmlFor="upload_files_input"
                        className={`mx-1 ${
                            backend.type === backendModule.BackendType.local
                                ? 'opacity-50'
                                : 'cursor-pointer'
                        }`}
                    >
                        <img src={UploadIcon} />
                    </label>
                    <button
                        className={`mx-1 opacity-50`}
                        disabled={true}
                        onClick={event => {
                            event.stopPropagation()
                            /* TODO */
                        }}
                    >
                        <img src={DownloadIcon} />
                    </button>
                </div>
            </div>
        </div>
    )
}
