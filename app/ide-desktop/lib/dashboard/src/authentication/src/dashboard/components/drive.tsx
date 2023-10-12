/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as common from 'enso-common'

import type * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'
import * as localStorageModule from '../localStorage'
import * as localStorageProvider from '../../providers/localStorage'

import * as app from '../../components/app'
import * as pageSwitcher from './pageSwitcher'
import type * as spinner from './spinner'
import CategorySwitcher, * as categorySwitcher from './categorySwitcher'
import AssetsTable from './assetsTable'
import DriveBar from './driveBar'

// =============
// === Drive ===
// =============

/** Props for a {@link Drive}. */
export interface DriveProps {
    page: pageSwitcher.Page
    hidden: boolean
    initialProjectName: string | null
    /** These events will be dispatched the next time the assets list is refreshed, rather than
     * immediately. */
    queuedAssetEvents: assetEventModule.AssetEvent[]
    assetListEvents: assetListEventModule.AssetListEvent[]
    dispatchAssetListEvent: (directoryEvent: assetListEventModule.AssetListEvent) => void
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (directoryEvent: assetEventModule.AssetEvent) => void
    query: string
    projectStartupInfo: backendModule.ProjectStartupInfo | null
    doCreateProject: (templateId: string | null) => void
    doOpenEditor: (
        project: backendModule.ProjectAsset,
        setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
        switchPage: boolean
    ) => void
    doCloseEditor: (project: backendModule.ProjectAsset) => void
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export default function Drive(props: DriveProps) {
    const {
        page,
        hidden,
        initialProjectName,
        queuedAssetEvents,
        query,
        projectStartupInfo,
        assetListEvents,
        dispatchAssetListEvent,
        assetEvents,
        dispatchAssetEvent,
        doOpenEditor,
        doCloseEditor,
        loadingProjectManagerDidFail,
        isListingRemoteDirectoryWhileOffline,
        isListingLocalDirectoryAndWillFail,
        isListingRemoteDirectoryAndWillFail,
    } = props
    const navigate = hooks.useNavigate()
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const toastAndLog = hooks.useToastAndLog()
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)
    const [category, setCategory] = React.useState(
        () =>
            localStorage.get(localStorageModule.LocalStorageKey.driveCategory) ??
            categorySwitcher.Category.home
    )

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

    const doCreateProject = React.useCallback(
        (
            templateId: string | null,
            onSpinnerStateChange?: (state: spinner.SpinnerState) => void
        ) => {
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.newProject,
                parentKey: null,
                parentId: null,
                templateId: templateId ?? null,
                onSpinnerStateChange: onSpinnerStateChange ?? null,
            })
        },
        [/* should never change */ dispatchAssetListEvent]
    )

    const doCreateDirectory = React.useCallback(() => {
        dispatchAssetListEvent({
            type: assetListEventModule.AssetListEventType.newFolder,
            parentKey: null,
            parentId: null,
        })
    }, [/* should never change */ dispatchAssetListEvent])

    const doCreateDataConnector = React.useCallback(
        (name: string, value: string) => {
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.newDataConnector,
                parentKey: null,
                parentId: null,
                name,
                value,
            })
        },
        [/* should never change */ dispatchAssetListEvent]
    )

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

    return isListingRemoteDirectoryWhileOffline ? (
        <div className={`mx-2 grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
            <div className="flex flex-col gap-4">
                <div className="text-center text-base">You are not signed in.</div>
                <button
                    className="h-8 w-16 self-center rounded-full bg-help py-px text-base leading-170 text-white"
                    onClick={() => {
                        navigate(app.LOGIN_PATH)
                    }}
                >
                    Login
                </button>
            </div>
        </div>
    ) : isListingLocalDirectoryAndWillFail ? (
        <div className={`mx-2 grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
            <div className="text-center text-base">
                Could not connect to the Project Manager. Please try restarting{' '}
                {common.PRODUCT_NAME}, or manually launching the Project Manager.
            </div>
        </div>
    ) : isListingRemoteDirectoryAndWillFail ? (
        <div className={`mx-2 grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
            <div className="text-center text-base">
                We will review your user details and enable the cloud experience for you shortly.
            </div>
        </div>
    ) : (
        <div
            data-testid="drive-view"
            className={`mt-8 flex flex-1 flex-col gap-2.5 overflow-hidden px-3.25 ${
                hidden ? 'hidden' : ''
            }`}
        >
            <div className="flex flex-col gap-3 self-start">
                <h1 className="h-9.5 pl-1.5 text-xl font-bold">
                    {backend.type === backendModule.BackendType.remote
                        ? 'Cloud Drive'
                        : 'Local Drive'}
                </h1>
                <DriveBar
                    category={category}
                    doCreateProject={doCreateProject}
                    doUploadFiles={doUploadFiles}
                    doCreateDirectory={doCreateDirectory}
                    doCreateDataConnector={doCreateDataConnector}
                    dispatchAssetEvent={dispatchAssetEvent}
                />
            </div>
            <div className="flex flex-1 gap-3 overflow-hidden">
                {backend.type === backendModule.BackendType.remote && (
                    <div className="flex flex-col gap-4 py-1">
                        <CategorySwitcher category={category} setCategory={setCategory} />
                    </div>
                )}
                <AssetsTable
                    query={query}
                    category={category}
                    initialProjectName={initialProjectName}
                    projectStartupInfo={projectStartupInfo}
                    queuedAssetEvents={queuedAssetEvents}
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
            </div>
            {isFileBeingDragged &&
            organization != null &&
            backend.type === backendModule.BackendType.remote ? (
                <div
                    className="fixed inset-0 grid h-screen w-screen place-items-center bg-primary bg-opacity-75 text-lg text-white opacity-0 backdrop-blur-none transition-all hover:opacity-100 hover:backdrop-blur-xs"
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
