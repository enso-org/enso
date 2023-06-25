/** @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options. */
import * as React from 'react'

import ArrowRightSmallIcon from 'enso-assets/arrow_right_small.svg'
import DownloadIcon from 'enso-assets/download.svg'
import UploadIcon from 'enso-assets/upload.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as column from '../column'
import * as featureFlags from '../featureFlags'
import * as modalProvider from '../../providers/modal'

import ColumnDisplayModeSwitcher from './columnDisplayModeSwitcher'
import UploadFileModal from './uploadFileModal'

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
    onUpload: () => void
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
        onUpload,
        exitDirectory,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

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
                    <button
                        className={`mx-1 ${
                            backend.type === backendModule.BackendType.local ? 'opacity-50' : ''
                        }`}
                        disabled={backend.type === backendModule.BackendType.local}
                        onClick={event => {
                            event.stopPropagation()
                            setModal(
                                <UploadFileModal directoryId={directoryId} onSuccess={onUpload} />
                            )
                        }}
                    >
                        <img src={UploadIcon} />
                    </button>
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
