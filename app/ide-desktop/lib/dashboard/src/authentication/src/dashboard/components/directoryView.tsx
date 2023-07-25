/** @file The directory header bar and directory item listing. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import * as common from 'enso-common'

import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as tabModule from '../tab'

import * as fileListEventModule from '../events/fileListEvent'
import * as projectEventModule from '../events/projectEvent'
import * as projectListEventModule from '../events/projectListEvent'

import DirectoriesTable from './directoriesTable'
import DriveBar from './driveBar'
import FilesTable from './filesTable'
import ProjectsTable from './projectsTable'
import SecretsTable from './secretsTable'

// =================
// === Constants ===
// =================

/** The `localStorage` key under which the ID of the current directory is stored. */
const DIRECTORY_STACK_KEY = `${common.PRODUCT_NAME.toLowerCase()}-dashboard-directory-stack`

// ========================
// === Helper functions ===
// ========================

/** Sanitizes a string for use as a regex. */
function regexEscape(string: string) {
    return string.replace(/[\\^$.|?*+()[{]/g, '\\$&')
}

// =====================
// === DirectoryView ===
// =====================

/** Props for a {@link DirectoryView}. */
export interface DirectoryViewProps {
    tab: tabModule.Tab
    initialProjectName: string | null
    nameOfProjectToImmediatelyOpen: string | null
    setNameOfProjectToImmediatelyOpen: (nameOfProjectToImmediatelyOpen: string | null) => void
    directoryId: backendModule.DirectoryId | null
    setDirectoryId: (directoryId: backendModule.DirectoryId | null) => void
    projectListEvent: projectListEventModule.ProjectListEvent | null
    dispatchProjectListEvent: (directoryEvent: projectListEventModule.ProjectListEvent) => void
    query: string
    onOpenIde: (project: backendModule.ProjectAsset) => void
    onCloseIde: () => void
    appRunner: AppRunner | null
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
function DirectoryView(props: DirectoryViewProps) {
    const {
        tab,
        initialProjectName,
        nameOfProjectToImmediatelyOpen,
        setNameOfProjectToImmediatelyOpen,
        directoryId,
        setDirectoryId,
        query,
        projectListEvent,
        dispatchProjectListEvent,
        onOpenIde,
        onCloseIde,
        appRunner,
        loadingProjectManagerDidFail,
        isListingRemoteDirectoryWhileOffline,
        isListingLocalDirectoryAndWillFail,
        isListingRemoteDirectoryAndWillFail,
    } = props
    const logger = loggerProvider.useLogger()
    const { organization, accessToken } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()

    const [initialized, setInitialized] = React.useState(false)
    const [isLoadingAssets, setIsLoadingAssets] = React.useState(true)
    const [directoryStack, setDirectoryStack] = React.useState<backendModule.DirectoryAsset[]>([])
    // Defined by the spec as `compact` by default, however some columns lack an implementation
    // in the remote (cloud) backend and will therefore be empty.
    const [columnDisplayMode, setColumnDisplayMode] = React.useState(
        columnModule.ColumnDisplayMode.release
    )
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)

    const [projectAssets, setProjectAssets] = React.useState<backendModule.ProjectAsset[]>([])
    const [directoryAssets, setDirectoryAssets] = React.useState<backendModule.DirectoryAsset[]>([])
    const [secretAssets, setSecretAssets] = React.useState<backendModule.SecretAsset[]>([])
    const [fileAssets, setFileAssets] = React.useState<backendModule.FileAsset[]>([])
    const [projectEvent, dispatchProjectEvent] =
        React.useState<projectEventModule.ProjectEvent | null>(null)
    const [fileListEvent, dispatchFileListEvent] =
        React.useState<fileListEventModule.FileListEvent | null>(null)

    const assetFilter = React.useMemo(() => {
        if (query === '') {
            return null
        } else {
            const regex = new RegExp(regexEscape(query), 'i')
            return (asset: backendModule.Asset) => regex.test(asset.title)
        }
    }, [query])

    const directory = directoryStack[directoryStack.length - 1] ?? null
    const parentDirectory = directoryStack[directoryStack.length - 2] ?? null

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
        setProjectAssets([])
        setDirectoryAssets([])
        setSecretAssets([])
        setFileAssets([])
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

    const assets = hooks.useAsyncEffect(
        [],
        async signal => {
            switch (backend.type) {
                case backendModule.BackendType.local: {
                    if (!isListingLocalDirectoryAndWillFail) {
                        const newAssets = await backend.listDirectory()
                        if (!signal.aborted) {
                            setIsLoadingAssets(false)
                        }
                        return newAssets
                    } else {
                        return []
                    }
                }
                case backendModule.BackendType.remote: {
                    if (
                        !isListingRemoteDirectoryAndWillFail &&
                        !isListingRemoteDirectoryWhileOffline &&
                        directoryId != null
                    ) {
                        const newAssets = await backend.listDirectory(
                            { parentId: directoryId },
                            directory?.title ?? null
                        )
                        if (!signal.aborted) {
                            setIsLoadingAssets(false)
                        }
                        return newAssets
                    } else {
                        setIsLoadingAssets(false)
                        return []
                    }
                }
            }
        },
        [accessToken, directoryId, backend]
    )

    React.useEffect(() => {
        const newProjectAssets = assets.filter(backendModule.assetIsProject)
        setProjectAssets(newProjectAssets)
        setDirectoryAssets(assets.filter(backendModule.assetIsDirectory))
        setSecretAssets(assets.filter(backendModule.assetIsSecret))
        setFileAssets(assets.filter(backendModule.assetIsFile))
        if (nameOfProjectToImmediatelyOpen != null) {
            const projectToLoad = newProjectAssets.find(
                projectAsset => projectAsset.title === nameOfProjectToImmediatelyOpen
            )
            if (projectToLoad != null) {
                dispatchProjectEvent({
                    type: projectEventModule.ProjectEventType.open,
                    projectId: projectToLoad.id,
                })
            }
            setNameOfProjectToImmediatelyOpen(null)
        }
        if (!initialized && initialProjectName != null) {
            setInitialized(true)
            if (!newProjectAssets.some(asset => asset.title === initialProjectName)) {
                const errorMessage = `No project named '${initialProjectName}' was found.`
                toastify.toast.error(errorMessage)
                logger.error(`Error opening project on startup: ${errorMessage}`)
            }
        }
        // `nameOfProjectToImmediatelyOpen` must NOT trigger this effect.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [
        assets,
        initialized,
        initialProjectName,
        logger,
        /* should never change */ setNameOfProjectToImmediatelyOpen,
        /* should never change */ dispatchProjectEvent,
    ])

    const doCreateProject = React.useCallback(() => {
        dispatchProjectListEvent({
            type: projectListEventModule.ProjectListEventType.create,
            templateId: null,
            onSpinnerStateChange: null,
        })
    }, [/* should never change */ dispatchProjectListEvent])

    const enterDirectory = React.useCallback(
        (directoryAsset: backendModule.DirectoryAsset) => {
            setDirectoryId(directoryAsset.id)
            setDirectoryStack([...directoryStack, directoryAsset])
        },
        [directoryStack, setDirectoryId]
    )

    const exitDirectory = React.useCallback(() => {
        setDirectoryId(
            parentDirectory?.id ??
                (organization != null ? backendModule.rootDirectoryId(organization.id) : null)
        )
        setDirectoryStack(
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            directoryStack.slice(0, -1)
        )
    }, [directoryStack, organization, parentDirectory?.id, setDirectoryId])

    React.useEffect(() => {
        const onDragEnter = (event: DragEvent) => {
            if (
                tab === tabModule.Tab.dashboard &&
                event.dataTransfer?.types.includes('Files') === true
            ) {
                setIsFileBeingDragged(true)
            }
        }
        document.body.addEventListener('dragenter', onDragEnter)
        return () => {
            document.body.removeEventListener('dragenter', onDragEnter)
        }
    }, [tab])

    return (
        <>
            <DriveBar
                directoryId={directoryId}
                directory={directory}
                parentDirectory={parentDirectory}
                columnDisplayMode={columnDisplayMode}
                setColumnDisplayMode={setColumnDisplayMode}
                exitDirectory={exitDirectory}
                dispatchFileListEvent={dispatchFileListEvent}
            />
            {/* Padding. */}
            <div className="h-6 mx-2" />
            <div className="flex-1 overflow-auto mx-2">
                <ProjectsTable
                    appRunner={appRunner}
                    directoryId={directoryId}
                    items={projectAssets}
                    filter={assetFilter}
                    isLoading={isLoadingAssets}
                    columnDisplayMode={columnDisplayMode}
                    projectEvent={projectEvent}
                    dispatchProjectEvent={dispatchProjectEvent}
                    projectListEvent={projectListEvent}
                    dispatchProjectListEvent={dispatchProjectListEvent}
                    doCreateProject={doCreateProject}
                    doOpenIde={onOpenIde}
                    doCloseIde={onCloseIde}
                />
                {/* Padding. */}
                <div className="h-10" />
                <DirectoriesTable
                    directoryId={directoryId}
                    items={directoryAssets}
                    filter={assetFilter}
                    isLoading={isLoadingAssets}
                    columnDisplayMode={columnDisplayMode}
                    enterDirectory={enterDirectory}
                />
                {/* Padding. */}
                <div className="h-10" />
                <SecretsTable
                    directoryId={directoryId}
                    items={secretAssets}
                    filter={assetFilter}
                    isLoading={isLoadingAssets}
                    columnDisplayMode={columnDisplayMode}
                />
                {/* Padding. */}
                <div className="h-10" />
                <FilesTable
                    directoryId={directoryId}
                    items={fileAssets}
                    filter={assetFilter}
                    isLoading={isLoadingAssets}
                    columnDisplayMode={columnDisplayMode}
                    fileListEvent={fileListEvent}
                    dispatchFileListEvent={dispatchFileListEvent}
                />
            </div>
            {isFileBeingDragged &&
            directoryId != null &&
            backend.type === backendModule.BackendType.remote ? (
                <div
                    className="text-white text-lg fixed w-screen h-screen inset-0 bg-primary grid place-items-center"
                    onDragLeave={() => {
                        setIsFileBeingDragged(false)
                    }}
                    onDragOver={event => {
                        event.preventDefault()
                    }}
                    onDrop={event => {
                        event.preventDefault()
                        setIsFileBeingDragged(false)
                        dispatchFileListEvent({
                            type: fileListEventModule.FileListEventType.uploadMultiple,
                            files: event.dataTransfer.files,
                        })
                    }}
                >
                    Drop to upload files.
                </div>
            ) : null}
        </>
    )
}

export default DirectoryView
