/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import ArrowRightSmallIcon from 'enso-assets/arrow_right_small.svg'
import DownloadIcon from 'enso-assets/download.svg'
import UploadIcon from 'enso-assets/upload.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as column from '../column'
import * as featureFlags from '../featureFlags'
import * as fileListEventModule from '../events/fileListEvent'
import * as loggerProvider from '../../providers/logger'

import ColumnDisplayModeSwitcher from './columnDisplayModeSwitcher'

// ================
// === DriveBar ===
// ================

/** Props for a {@link DriveBar}. */
export interface DriveBarProps {
    directoryId: backendModule.DirectoryId | null
    directory: backendModule.DirectoryAsset | null
    parentDirectory: backendModule.DirectoryAsset | null
    columnDisplayMode: column.ColumnDisplayMode
    setColumnDisplayMode: (columnDisplayMode: column.ColumnDisplayMode) => void
    dispatchFileListEvent: (fileListEvent: fileListEventModule.FileListEvent) => void
    exitDirectory: () => void
}

/** Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher. */
function DriveBar(props: DriveBarProps) {
    const {
        directoryId,
        directory,
        parentDirectory,
        columnDisplayMode,
        setColumnDisplayMode,
        dispatchFileListEvent,
        exitDirectory,
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()

    const uploadFiles = React.useCallback(
        (event: React.FormEvent<HTMLInputElement>) => {
            if (backend.type === backendModule.BackendType.local) {
                // TODO[sb]: Allow uploading `.enso-project`s
                // https://github.com/enso-org/cloud-v2/issues/510
                const message = 'Files cannot be uploaded to the local backend.'
                toastify.toast.error(message)
                logger.error(message)
            } else if (
                event.currentTarget.files == null ||
                event.currentTarget.files.length === 0
            ) {
                toastify.toast.success('No files selected to upload.')
            } else if (directoryId == null) {
                // This should never happen, however display a nice error message in case
                // it somehow does.
                const message = 'Files cannot be uploaded while offline.'
                toastify.toast.error(message)
                logger.error(message)
            } else {
                dispatchFileListEvent({
                    type: fileListEventModule.FileListEventType.uploadMultiple,
                    files: event.currentTarget.files,
                })
            }
        },
        [
            backend.type,
            directoryId,
            /* should not change */ logger,
            /* should never change */ dispatchFileListEvent,
        ]
    )

    return (
        <div className="flex flex-row flex-nowrap my-2">
            <h1 className="text-xl font-bold mx-4 self-center">Drive</h1>
            <div className="flex flex-row flex-nowrap mx-4">
                <div className="bg-gray-100 rounded-l-full flex flex-row flex-nowrap items-center p-1 mx-0.5">
                    {directory && (
                        <>
                            <button className="mx-2" onClick={exitDirectory}>
                                {parentDirectory?.title ?? '/'}
                            </button>
                            <img src={ArrowRightSmallIcon} />
                        </>
                    )}
                    <span className="mx-2">{directory?.title ?? '/'}</span>
                </div>
                <div className="bg-gray-100 rounded-r-full flex flex-row flex-nowrap items-center mx-0.5">
                    <div className="m-2">Shared with</div>
                    <div></div>
                </div>
                <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap px-1.5 py-1 mx-4">
                    <input
                        type="file"
                        multiple
                        disabled={backend.type === backendModule.BackendType.local}
                        id="upload_files_input"
                        name="upload_files_input"
                        className="w-0 h-0"
                        onInput={uploadFiles}
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
                {featureFlags.FEATURE_FLAGS.columnDisplayModeSwitcher && (
                    <ColumnDisplayModeSwitcher
                        columnDisplayMode={columnDisplayMode}
                        setColumnDisplayMode={setColumnDisplayMode}
                    />
                )}
            </div>
        </div>
    )
}

export default DriveBar
