/** @file The directory header bar and directory item listing. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as common from 'enso-common'

import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as directoryEventModule from '../events/directoryEvent'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as newtype from '../../newtype'
import * as projectEventModule from '../events/projectEvent'

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
/** The {@link RegExp} matching a directory name following the default naming convention. */
const DIRECTORY_NAME_REGEX = /^New_Directory_(?<directoryIndex>\d+)$/
/** The default prefix of an automatically generated directory. */
const DIRECTORY_NAME_DEFAULT_PREFIX = 'New_Directory_'

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
    initialProjectName: string | null
    nameOfProjectToImmediatelyOpen: string | null
    setNameOfProjectToImmediatelyOpen: (nameOfProjectToImmediatelyOpen: string | null) => void
    directoryId: backendModule.DirectoryId | null
    setDirectoryId: (directoryId: backendModule.DirectoryId | null) => void
    directoryEvent: directoryEventModule.DirectoryEvent | null
    dispatchDirectoryEvent: (directoryEvent: directoryEventModule.DirectoryEvent | null) => void
    query: string
    refresh: hooks.RefreshState
    doRefresh: () => void
    onOpenIde: (project: backendModule.ProjectAsset) => void
    onCloseIde: () => void
    appRunner: AppRunner | null
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
    experimentalShowColumnDisplayModeSwitcher: boolean
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
function DirectoryView(props: DirectoryViewProps) {
    const {
        initialProjectName,
        nameOfProjectToImmediatelyOpen,
        setNameOfProjectToImmediatelyOpen,
        directoryId,
        setDirectoryId,
        query,
        refresh,
        directoryEvent,
        dispatchDirectoryEvent,
        doRefresh,
        onOpenIde,
        onCloseIde,
        appRunner,
        loadingProjectManagerDidFail,
        isListingRemoteDirectoryWhileOffline,
        isListingLocalDirectoryAndWillFail,
        isListingRemoteDirectoryAndWillFail,
        experimentalShowColumnDisplayModeSwitcher,
    } = props
    const logger = loggerProvider.useLogger()
    const { organization, accessToken } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()

    const [initialized, setInitialized] = React.useState(false)
    const [isLoadingAssets, setIsLoadingAssets] = React.useState(true)
    const [directoryStack, setDirectoryStack] = React.useState<backendModule.DirectoryAsset[]>([])
    // Defined by the spec as `compact` by default, however it is not ready yet.
    const [columnDisplayMode, setColumnDisplayMode] = React.useState(
        columnModule.ColumnDisplayMode.release
    )

    const [projectAssets, setProjectAssets] = React.useState<backendModule.ProjectAsset[]>([])
    const [directoryAssets, setDirectoryAssets] = React.useState<backendModule.DirectoryAsset[]>([])
    const [secretAssets, setSecretAssets] = React.useState<backendModule.SecretAsset[]>([])
    const [fileAssets, setFileAssets] = React.useState<backendModule.FileAsset[]>([])
    const [projectEvent, dispatchProjectEvent] =
        hooks.useEvent<projectEventModule.ProjectEvent | null>()

    const queryRegex = React.useMemo(() => new RegExp(regexEscape(query), 'i'), [query])
    const visibleProjectAssets = React.useMemo(
        () => projectAssets.filter(asset => queryRegex.test(asset.title)),
        [queryRegex, projectAssets]
    )
    const visibleDirectoryAssets = React.useMemo(
        () => directoryAssets.filter(asset => queryRegex.test(asset.title)),
        [queryRegex, directoryAssets]
    )
    const visibleSecretAssets = React.useMemo(
        () => secretAssets.filter(asset => queryRegex.test(asset.title)),
        [queryRegex, secretAssets]
    )
    const visibleFileAssets = React.useMemo(
        () => fileAssets.filter(asset => queryRegex.test(asset.title)),
        [queryRegex, fileAssets]
    )

    const directory = directoryStack[directoryStack.length - 1] ?? null
    const parentDirectory = directoryStack[directoryStack.length - 2] ?? null

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
        [accessToken, directoryId, refresh, backend]
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
                toast.error(errorMessage)
                logger.error(`Error opening project on startup: ${errorMessage}`)
            }
        }
    }, [
        assets,
        nameOfProjectToImmediatelyOpen,
        initialized,
        initialProjectName,
        logger,
        /* should never change */ setNameOfProjectToImmediatelyOpen,
        /* should never change */ dispatchProjectEvent,
    ])

    const getNewProjectName = React.useCallback(
        (templateId?: string | null) => {
            const prefix = `${templateId ?? 'New_Project'}_`
            const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
            const projectIndices = projectAssets
                .map(project => projectNameTemplate.exec(project.title)?.groups?.projectIndex)
                .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
            return `${prefix}${Math.max(...projectIndices) + 1}`
        },
        [projectAssets]
    )

    React.useEffect(() => {
        void (async () => {
            if (directoryEvent != null) {
                switch (directoryEvent.type) {
                    case directoryEventModule.DirectoryEventType.createProject: {
                        const projectName = getNewProjectName(directoryEvent.templateId)
                        const placeholderNewProjectAsset: backendModule.ProjectAsset = {
                            type: backendModule.AssetType.project,
                            title: projectName,
                            // Although this is a dummy value, it MUST be unique as it is used
                            // as the React key for lists.
                            id: newtype.asNewtype<backendModule.ProjectId>(
                                Number(new Date()).toString()
                            ),
                            modifiedAt: dateTime.toRfc3339(new Date()),
                            parentId:
                                directoryId ?? newtype.asNewtype<backendModule.DirectoryId>(''),
                            permissions: [],
                            projectState: { type: backendModule.ProjectState.new },
                        }
                        setProjectAssets(oldProjectAssets => [
                            placeholderNewProjectAsset,
                            ...oldProjectAssets,
                        ])
                        const createProjectPromise = backend.createProject({
                            projectName,
                            projectTemplateName: directoryEvent.templateId ?? null,
                            parentDirectoryId: directoryId,
                        })
                        await toast.promise(createProjectPromise, {
                            loading: 'Creating new empty project...',
                            success: 'Created new empty project.',
                            // This is UNSAFE, as the original function's parameter is of type
                            // `any`.
                            error: (promiseError: Error) =>
                                `Error creating new empty project: ${promiseError.message}`,
                        })
                        // `newProject.projectId` cannot be used directly in a `ProjectEvent`
                        // as the project does not yet exist in the project list.
                        // Opening the project would work, but the project would display as closed
                        // as it would be created after the event is sent.
                        setNameOfProjectToImmediatelyOpen(projectName)
                        doRefresh()
                        break
                    }
                    case directoryEventModule.DirectoryEventType.createDirectory: {
                        if (backend.type !== backendModule.BackendType.remote) {
                            toast.error('Folders cannot be created on the local backend.')
                        } else {
                            const directoryIndices = directoryAssets
                                .map(directoryAsset =>
                                    DIRECTORY_NAME_REGEX.exec(directoryAsset.title)
                                )
                                .map(match => match?.groups?.directoryIndex)
                                .map(maybeIndex =>
                                    maybeIndex != null ? parseInt(maybeIndex, 10) : 0
                                )
                            const title = `${DIRECTORY_NAME_DEFAULT_PREFIX}${
                                Math.max(...directoryIndices) + 1
                            }`
                            const placeholderNewDirectoryAsset: backendModule.DirectoryAsset = {
                                title,
                                type: backendModule.AssetType.directory,
                                id: newtype.asNewtype<backendModule.DirectoryId>(
                                    Number(new Date()).toString()
                                ),
                                modifiedAt: dateTime.toRfc3339(new Date()),
                                parentId:
                                    directoryId ?? newtype.asNewtype<backendModule.DirectoryId>(''),
                                permissions: [],
                                projectState: null,
                            }
                            setDirectoryAssets(oldDirectoryAssets => [
                                placeholderNewDirectoryAsset,
                                ...oldDirectoryAssets,
                            ])
                            const createDirectoryPromise = backend.createDirectory({
                                parentId: directoryId,
                                title,
                            })
                            await toast.promise(createDirectoryPromise, {
                                loading: 'Creating folder...',
                                success: 'Sucessfully created folder.',
                                // This is UNSAFE, as the original function's parameter is
                                // of type `any`.
                                error: (promiseError: Error) =>
                                    `Error creating new folder: ${promiseError.message}`,
                            })
                            doRefresh()
                        }
                        break
                    }
                }
            }
        })()
    }, [
        backend,
        directoryEvent,
        directoryId,
        directoryAssets,
        getNewProjectName,
        /* should never change */ doRefresh,
        /* should never change */ setNameOfProjectToImmediatelyOpen,
    ])

    const doCreateProject = React.useCallback(() => {
        dispatchDirectoryEvent({
            type: directoryEventModule.DirectoryEventType.createProject,
            templateId: null,
        })
    }, [/* should never change */ dispatchDirectoryEvent])

    const doCreateDirectory = React.useCallback(() => {
        dispatchDirectoryEvent({
            type: directoryEventModule.DirectoryEventType.createDirectory,
        })
    }, [/* should never change */ dispatchDirectoryEvent])

    const enterDirectory = React.useCallback(
        (directoryAsset: backendModule.DirectoryAsset) => {
            setDirectoryId(directoryAsset.id)
            setDirectoryStack([...directoryStack, directoryAsset])
        },
        [directoryStack, setDirectoryId]
    )

    const exitDirectory = () => {
        setDirectoryId(
            parentDirectory?.id ??
                (organization != null ? backendModule.rootDirectoryId(organization.id) : null)
        )
        setDirectoryStack(
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            directoryStack.slice(0, -1)
        )
    }

    return (
        <>
            <DriveBar
                directoryId={directoryId}
                directory={directory}
                parentDirectory={parentDirectory}
                columnDisplayMode={columnDisplayMode}
                setColumnDisplayMode={setColumnDisplayMode}
                experimentalShowColumnDisplayModeSwitcher={
                    experimentalShowColumnDisplayModeSwitcher
                }
                onUpload={doRefresh}
                exitDirectory={exitDirectory}
            />
            <div className="h-10" />
            <ProjectsTable
                appRunner={appRunner}
                items={visibleProjectAssets}
                isLoading={isLoadingAssets}
                columnDisplayMode={columnDisplayMode}
                projectEvent={projectEvent}
                setProjectEvent={dispatchProjectEvent}
                doCreateProject={doCreateProject}
                onRename={doRefresh}
                onDelete={doRefresh}
                doOpenIde={onOpenIde}
                doCloseIde={onCloseIde}
            />
            <div className="h-10" />
            <DirectoriesTable
                items={visibleDirectoryAssets}
                isLoading={isLoadingAssets}
                columnDisplayMode={columnDisplayMode}
                query={query}
                doCreateDirectory={doCreateDirectory}
                onRename={doRefresh}
                onDelete={doRefresh}
                enterDirectory={enterDirectory}
            />
            <div className="h-10" />
            <SecretsTable
                directoryId={directoryId}
                items={visibleSecretAssets}
                isLoading={isLoadingAssets}
                columnDisplayMode={columnDisplayMode}
                query={query}
                onCreate={doRefresh}
                onDelete={doRefresh}
            />
            <div className="h-10" />
            <FilesTable
                directoryId={directoryId}
                items={visibleFileAssets}
                isLoading={isLoadingAssets}
                columnDisplayMode={columnDisplayMode}
                query={query}
                onCreate={doRefresh}
                onDelete={doRefresh}
            />
        </>
    )
}

export default DirectoryView
