/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'

import * as pageSwitcher from './pageSwitcher'
import AssetsTable from './assetsTable'
import DriveBar from './driveBar'

// =================
// === DriveView ===
// =================

/** Props for a {@link DriveView}. */
export interface DriveViewProps {
    page: pageSwitcher.Page
    hidden: boolean
    initialProjectName: string | null
    assetListEvents: assetListEventModule.AssetListEvent[]
    dispatchAssetListEvent: (directoryEvent: assetListEventModule.AssetListEvent) => void
    query: string
    doCreateProject: (templateId: string | null) => void
    doOpenEditor: (project: backendModule.ProjectAsset) => void
    doCloseEditor: () => void
    appRunner: AppRunner | null
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export default function DriveView(props: DriveViewProps) {
    const {
        page,
        hidden,
        initialProjectName,
        query,
        assetListEvents,
        dispatchAssetListEvent,
        doCreateProject,
        doOpenEditor,
        doCloseEditor,
        appRunner,
        loadingProjectManagerDidFail,
        isListingRemoteDirectoryWhileOffline,
        isListingLocalDirectoryAndWillFail,
        isListingRemoteDirectoryAndWillFail,
    } = props
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)
    const [assetEvents, dispatchAssetEvent] = hooks.useEvent<assetEventModule.AssetEvent>()

    React.useEffect(() => {
        const onBlur = () => {
            setIsFileBeingDragged(false)
        }
        window.addEventListener('blur', onBlur)
        return () => {
            window.removeEventListener('blur', onBlur)
        }
    }, [])

    const doUploadFiles = React.useCallback(
        (files: File[]) => {
            if (backend.type !== backendModule.BackendType.local && organization == null) {
                // This should never happen, however display a nice error message in case it does.
                toastAndLog('Files cannot be uploaded while offline')
            } else {
                dispatchAssetListEvent({
                    type: assetListEventModule.AssetListEventType.uploadFiles,
                    parentKey: null,
                    parentId: null,
                    files,
                })
            }
        },
        [backend, organization, toastAndLog, /* should never change */ dispatchAssetListEvent]
    )

    const doCreateDirectory = React.useCallback(() => {
        dispatchAssetListEvent({
            type: assetListEventModule.AssetListEventType.newFolder,
            parentKey: null,
            parentId: null,
        })
    }, [/* should never change */ dispatchAssetListEvent])

    React.useEffect(() => {
        const onDragEnter = (event: DragEvent) => {
            if (
                page === pageSwitcher.Page.drive &&
                event.dataTransfer?.types.includes('Files') === true
            ) {
                setIsFileBeingDragged(true)
            }
        }
        document.body.addEventListener('dragenter', onDragEnter)
        return () => {
            document.body.removeEventListener('dragenter', onDragEnter)
        }
    }, [page])

    return (
        <div
            className={`flex flex-col flex-1 overflow-hidden gap-2.5 px-3.25 ${
                hidden ? 'hidden' : ''
            }`}
        >
            <div className="flex flex-col self-start gap-3">
                <h1 className="text-xl font-bold h-9.5 pl-1.5">
                    {backend.type === backendModule.BackendType.remote
                        ? 'Cloud Drive'
                        : 'Local Drive'}
                </h1>
                <DriveBar
                    doCreateProject={doCreateProject}
                    doUploadFiles={doUploadFiles}
                    doCreateDirectory={doCreateDirectory}
                    dispatchAssetEvent={dispatchAssetEvent}
                />
            </div>
            <AssetsTable
                query={query}
                appRunner={appRunner}
                initialProjectName={initialProjectName}
                assetEvents={assetEvents}
                dispatchAssetEvent={dispatchAssetEvent}
                assetListEvents={assetListEvents}
                dispatchAssetListEvent={dispatchAssetListEvent}
                doOpenIde={doOpenEditor}
                doCloseIde={doCloseEditor}
                loadingProjectManagerDidFail={loadingProjectManagerDidFail}
                isListingRemoteDirectoryWhileOffline={isListingRemoteDirectoryWhileOffline}
                isListingLocalDirectoryAndWillFail={isListingLocalDirectoryAndWillFail}
                isListingRemoteDirectoryAndWillFail={isListingRemoteDirectoryAndWillFail}
            />
            {isFileBeingDragged &&
            organization != null &&
            backend.type === backendModule.BackendType.remote ? (
                <div
                    className="text-white text-lg fixed w-screen h-screen inset-0 opacity-0 hover:opacity-100 bg-primary bg-opacity-75 backdrop-blur-none hover:backdrop-blur-xs transition-all grid place-items-center"
                    onDragLeave={() => {
                        setIsFileBeingDragged(false)
                    }}
                    onDragOver={event => {
                        event.preventDefault()
                    }}
                    onDrop={event => {
                        event.preventDefault()
                        setIsFileBeingDragged(false)
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.uploadFiles,
                            parentKey: null,
                            parentId: null,
                            files: Array.from(event.dataTransfer.files),
                        })
                    }}
                >
                    Drop to upload files.
                </div>
            ) : null}
        </div>
    )
}
