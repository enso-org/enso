/** @file The directory header bar and directory item listing. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import * as common from 'enso-common'

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
import DriveBar from './driveBar'

// =================
// === Constants ===
// =================

/** The `localStorage` key under which the ID of the current directory is stored. */
const DIRECTORY_STACK_KEY = `${common.PRODUCT_NAME.toLowerCase()}-dashboard-directory-stack`

// =================
// === DriveView ===
// =================

/** Props for a {@link DriveView}. */
export interface DriveViewProps {
    page: pageSwitcher.Page
    hidden: boolean
    initialProjectName: string | null
    directoryId: backendModule.DirectoryId | null
    setDirectoryId: (directoryId: backendModule.DirectoryId) => void
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
        directoryId,
        setDirectoryId,
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
    const [directoryStack, setDirectoryStack] = React.useState<backendModule.DirectoryAsset[]>([])
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
    }, [backend, directoryId])

    React.useEffect(() => {
        if (backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail) {
            setIsLoadingAssets(false)
        }
    }, [loadingProjectManagerDidFail, backend.type])

    React.useEffect(() => {
        const cachedDirectoryStackJson = localStorage.getItem(DIRECTORY_STACK_KEY)
        if (cachedDirectoryStackJson != null) {
            // The JSON was inserted by the code below, so it will always have the right type.
            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            const cachedDirectoryStack: backendModule.DirectoryAsset[] =
                JSON.parse(cachedDirectoryStackJson)
            setDirectoryStack(cachedDirectoryStack)
            const cachedDirectoryId = cachedDirectoryStack[cachedDirectoryStack.length - 1]?.id
            if (cachedDirectoryId) {
                setDirectoryId(cachedDirectoryId)
            }
        }
    }, [setDirectoryId])

    React.useEffect(() => {
        if (
            organization != null &&
            directoryId === backendModule.rootDirectoryId(organization.id)
        ) {
            localStorage.removeItem(DIRECTORY_STACK_KEY)
        } else {
            localStorage.setItem(DIRECTORY_STACK_KEY, JSON.stringify(directoryStack))
        }
    }, [directoryStack, directoryId, organization])

    const setAssets = React.useCallback(
        (newAssets: backendModule.AnyAsset[]) => {
            rawSetAssets(newAssets)
            if (nameOfProjectToImmediatelyOpen != null) {
                const projectToLoad = newAssets.find(
                    projectAsset => projectAsset.title === nameOfProjectToImmediatelyOpen
                )
                if (projectToLoad != null) {
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.openProject,
                        id: projectToLoad.id,
                    })
                }
                setNameOfProjectToImmediatelyOpen(null)
            }
            if (!initialized && initialProjectName != null) {
                setInitialized(true)
                if (!newAssets.some(asset => asset.title === initialProjectName)) {
                    const errorMessage = `No project named '${initialProjectName}' was found.`
                    toastify.toast.error(errorMessage)
                    logger.error(`Error opening project on startup: ${errorMessage}`)
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
        setAssets([])
        // `setAssets` is a callback, not a dependency.
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
                        !isListingRemoteDirectoryWhileOffline &&
                        directoryId != null
                    ) {
                        const newAssets = await backend.listDirectory(
                            { parentId: directoryId },
                            directoryStack[0]?.title ?? null
                        )
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
        [accessToken, directoryId, backend]
    )

    const doUploadFiles = React.useCallback(
        (files: FileList) => {
            if (backend.type === backendModule.BackendType.local) {
                // TODO[sb]: Allow uploading `.enso-project`s
                // https://github.com/enso-org/cloud-v2/issues/510
                toastAndLog('Files cannot be uploaded to the local backend')
            } else if (directoryId == null) {
                // This should never happen, however display a nice error message in case it does.
                toastAndLog('Files cannot be uploaded while offline')
            } else {
                dispatchAssetListEvent({
                    type: assetListEventModule.AssetListEventType.uploadFiles,
                    parentId: directoryId,
                    files,
                })
            }
        },
        [backend.type, directoryId, toastAndLog, /* should never change */ dispatchAssetListEvent]
    )

    const doCreateDirectory = React.useCallback(() => {
        dispatchAssetListEvent({
            type: assetListEventModule.AssetListEventType.createDirectory,
            parentId: directoryId,
        })
    }, [directoryId, /* should never change */ dispatchAssetListEvent])

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
                />
            </div>
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
            {isFileBeingDragged &&
            directoryId != null &&
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
                            parentId: directoryId,
                            files: event.dataTransfer.files,
                        })
                    }}
                >
                    Drop to upload files.
                </div>
            ) : null}
        </div>
    )
}
