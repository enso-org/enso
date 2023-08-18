/** @file The directory header bar and directory item listing. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as string from '../../string'

import * as pageSwitcher from './pageSwitcher'
import AssetsTable from './assetsTable'
import CategorySwitcher from './categorySwitcher'
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
    const logger = loggerProvider.useLogger()
    const { organization, accessToken } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [initialized, setInitialized] = React.useState(false)
    const [assets, rawSetAssets] = React.useState<backendModule.AnyAsset[]>([])
    const [isLoadingAssets, setIsLoadingAssets] = React.useState(true)
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)
    const [assetEvents, dispatchAssetEvent] = hooks.useEvent<assetEventModule.AssetEvent>()
    const [nameOfProjectToImmediatelyOpen, setNameOfProjectToImmediatelyOpen] =
        React.useState(initialProjectName)

    const assetFilter = React.useMemo(() => {
        if (query === '') {
            return null
        } else {
            const regex = new RegExp(string.regexEscape(query), 'i')
            return (asset: backendModule.AnyAsset) => regex.test(asset.title)
        }
    }, [query])

    React.useEffect(() => {
        const onBlur = () => {
            setIsFileBeingDragged(false)
        }
        window.addEventListener('blur', onBlur)
        return () => {
            window.removeEventListener('blur', onBlur)
        }
    }, [])

    React.useEffect(() => {
        setIsLoadingAssets(true)
    }, [backend])

    React.useEffect(() => {
        if (backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail) {
            setIsLoadingAssets(false)
        }
    }, [loadingProjectManagerDidFail, backend.type])

    const setAssets = React.useCallback(
        (newAssets: backendModule.AnyAsset[]) => {
            rawSetAssets(newAssets)
            // The project name here might also be a string with project id, e.g. when opening
            // a project file from explorer on Windows.
            const isInitialProject = (asset: backendModule.AnyAsset) =>
                asset.title === initialProjectName || asset.id === initialProjectName
            if (nameOfProjectToImmediatelyOpen != null) {
                const projectToLoad = newAssets
                    .filter(backendModule.assetIsProject)
                    .find(isInitialProject)
                if (projectToLoad != null) {
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.openProject,
                        id: projectToLoad.id,
                    })
                }
                setNameOfProjectToImmediatelyOpen(null)
            }
            if (!initialized) {
                setInitialized(true)
                if (initialProjectName != null) {
                    if (!newAssets.some(isInitialProject)) {
                        const errorMessage = `No project named '${initialProjectName}' was found.`
                        toastify.toast.error(errorMessage)
                        logger.error(`Error opening project on startup: ${errorMessage}`)
                    }
                }
            }
        },
        [
            initialized,
            initialProjectName,
            logger,
            nameOfProjectToImmediatelyOpen,
            /* should never change */ setNameOfProjectToImmediatelyOpen,
            /* should never change */ dispatchAssetEvent,
        ]
    )

    React.useEffect(() => {
        if (initialized) {
            setAssets([])
        }
        // `setAssets` is a callback, not a dependency. `initialized` is not a dependency either.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [backend])

    hooks.useAsyncEffect(
        null,
        async signal => {
            switch (backend.type) {
                case backendModule.BackendType.local: {
                    if (!isListingLocalDirectoryAndWillFail) {
                        const newAssets = await backend.listDirectory({ parentId: null }, null)
                        if (!signal.aborted) {
                            setIsLoadingAssets(false)
                            setAssets(newAssets)
                        }
                    }
                    break
                }
                case backendModule.BackendType.remote: {
                    if (
                        !isListingRemoteDirectoryAndWillFail &&
                        !isListingRemoteDirectoryWhileOffline
                    ) {
                        const newAssets = await backend.listDirectory({ parentId: null }, null)
                        if (!signal.aborted) {
                            setIsLoadingAssets(false)
                            setAssets(newAssets)
                        }
                    } else {
                        setIsLoadingAssets(false)
                    }
                    break
                }
            }
        },
        [accessToken, organization, backend]
    )

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
            <div className="flex flex-1 gap-3 overflow-hidden">
                {backend.type === backendModule.BackendType.remote && (
                    <div className="flex flex-col gap-4 py-1">
                        <CategorySwitcher />
                    </div>
                )}
                <AssetsTable
                    items={assets}
                    filter={assetFilter}
                    isLoading={isLoadingAssets}
                    appRunner={appRunner}
                    assetEvents={assetEvents}
                    dispatchAssetEvent={dispatchAssetEvent}
                    assetListEvents={assetListEvents}
                    dispatchAssetListEvent={dispatchAssetListEvent}
                    doOpenIde={doOpenEditor}
                    doCloseIde={doCloseEditor}
                />
            </div>
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
