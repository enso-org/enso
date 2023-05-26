/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as react from 'react'

import * as common from 'enso-common'

import * as backendModule from '../backend'
import * as dateTime from '../dateTime'
import * as fileInfo from '../../fileInfo'
import * as hooks from '../../hooks'
import * as http from '../../http'
import * as localBackend from '../localBackend'
import * as newtype from '../../newtype'
import * as projectManager from '../projectManager'
import * as remoteBackendModule from '../remoteBackend'
import * as svg from '../../components/svg'
import * as uploadMultipleFiles from '../../uploadMultipleFiles'

import * as auth from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import PermissionDisplay, * as permissionDisplay from './permissionDisplay'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import Ide from './ide'
import ProjectActionButton from './projectActionButton'
import Rows from './rows'
import Templates from './templates'
import TopBar from './topBar'

import ConfirmDeleteModal from './confirmDeleteModal'
import RenameModal from './renameModal'
import UploadFileModal from './uploadFileModal'

import DirectoryCreateForm from './directoryCreateForm'
import FileCreateForm from './fileCreateForm'
import SecretCreateForm from './secretCreateForm'

// =============
// === Types ===
// =============

/** Main content of the screen. Only one should be visible at a time. */
export enum Tab {
    dashboard = 'dashboard',
    ide = 'ide',
}

/** Determines which columns are visible. */
enum ColumnDisplayMode {
    /** Show only columns which are ready for release. */
    release = 'release',
    /** Show all columns. */
    all = 'all',
    /** Show only name and metadata. */
    compact = 'compact',
    /** Show only columns relevant to documentation editors. */
    docs = 'docs',
    /** Show only name, metadata, and configuration options. */
    settings = 'settings',
}

/** Column type. */
enum Column {
    name = 'name',
    lastModified = 'last-modified',
    sharedWith = 'shared-with',
    docs = 'docs',
    labels = 'labels',
    dataAccess = 'data-access',
    usagePlan = 'usage-plan',
    engine = 'engine',
    ide = 'ide',
}

/** Values provided to form creation dialogs. */
export interface CreateFormProps {
    left: number
    top: number
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

// =================
// === Constants ===
// =================

/** Feature flags to enable or disable experimental features. */
const EXPERIMENTAL = {
    /** A selector that lets the user choose between pre-defined sets of visible columns. */
    columnModeSwitcher: false,
}

/** The `id` attribute of the element into which the IDE will be rendered. */
const IDE_ELEMENT_ID = 'root'
/** The `localStorage` key under which the ID of the current directory is stored. */
const DIRECTORY_STACK_KEY = 'enso-dashboard-directory-stack'

/** English names for the name column. */
const ASSET_TYPE_NAME: Record<backendModule.AssetType, string> = {
    [backendModule.AssetType.project]: 'Projects',
    [backendModule.AssetType.file]: 'Files',
    [backendModule.AssetType.secret]: 'Secrets',
    [backendModule.AssetType.directory]: 'Folders',
} as const

/** Forms to create each asset type. */
const ASSET_TYPE_CREATE_FORM: Record<
    Exclude<backendModule.AssetType, backendModule.AssetType.project>,
    (props: CreateFormProps) => JSX.Element
> = {
    [backendModule.AssetType.file]: FileCreateForm,
    [backendModule.AssetType.secret]: SecretCreateForm,
    [backendModule.AssetType.directory]: DirectoryCreateForm,
}

/** English names for every column except for the name column. */
const COLUMN_NAME: Record<Exclude<Column, Column.name>, string> = {
    [Column.lastModified]: 'Last modified',
    [Column.sharedWith]: 'Shared with',
    [Column.docs]: 'Docs',
    [Column.labels]: 'Labels',
    [Column.dataAccess]: 'Data access',
    [Column.usagePlan]: 'Usage plan',
    [Column.engine]: 'Engine',
    [Column.ide]: 'IDE',
} as const

/** CSS classes for every column. Currently only used to set the widths. */
const COLUMN_CSS_CLASS: Record<Column, string> = {
    [Column.name]: 'w-60',
    [Column.lastModified]: 'w-40',
    [Column.sharedWith]: 'w-36',
    [Column.docs]: 'w-96',
    [Column.labels]: 'w-80',
    [Column.dataAccess]: 'w-96',
    [Column.usagePlan]: '',
    [Column.engine]: 'w-20',
    [Column.ide]: 'w-20',
} as const

/** The corresponding `Permissions` for each backend `PermissionAction`. */
const PERMISSION: Record<backendModule.PermissionAction, permissionDisplay.Permissions> = {
    [backendModule.PermissionAction.own]: { type: permissionDisplay.Permission.owner },
    [backendModule.PermissionAction.execute]: {
        type: permissionDisplay.Permission.regular,
        read: false,
        write: false,
        docsWrite: false,
        exec: true,
    },
    [backendModule.PermissionAction.edit]: {
        type: permissionDisplay.Permission.regular,
        read: false,
        write: true,
        docsWrite: false,
        exec: false,
    },
    [backendModule.PermissionAction.read]: {
        type: permissionDisplay.Permission.regular,
        read: true,
        write: false,
        docsWrite: false,
        exec: false,
    },
}

/** The list of columns displayed on each `ColumnDisplayMode`. */
const COLUMNS_FOR: Record<ColumnDisplayMode, Column[]> = {
    [ColumnDisplayMode.release]: [Column.name, Column.lastModified, Column.sharedWith],
    [ColumnDisplayMode.all]: [
        Column.name,
        Column.lastModified,
        Column.sharedWith,
        Column.labels,
        Column.dataAccess,
        Column.usagePlan,
        Column.engine,
        Column.ide,
    ],
    [ColumnDisplayMode.compact]: [
        Column.name,
        Column.lastModified,
        Column.sharedWith,
        Column.labels,
        Column.dataAccess,
    ],
    [ColumnDisplayMode.docs]: [Column.name, Column.lastModified, Column.docs],
    [ColumnDisplayMode.settings]: [
        Column.name,
        Column.lastModified,
        Column.usagePlan,
        Column.engine,
        Column.ide,
    ],
}

// ========================
// === Helper functions ===
// ========================

/** Return the id of the root directory for a user or organization. */
function rootDirectoryId(userOrOrganizationId: backendModule.UserOrOrganizationId) {
    return newtype.asNewtype<backendModule.DirectoryId>(
        userOrOrganizationId.replace(/^organization-/, `${backendModule.AssetType.directory}-`)
    )
}

/** Returns the list of columns to be displayed. */
function columnsFor(displayMode: ColumnDisplayMode, backendType: backendModule.BackendType) {
    const columns = COLUMNS_FOR[displayMode]
    return backendType === backendModule.BackendType.local
        ? columns.filter(column => column !== Column.sharedWith)
        : columns
}

// =================
// === Dashboard ===
// =================

/** Props for {@link Dashboard}s that are common to all platforms. */
export interface DashboardProps {
    supportsLocalBackend: boolean
    appRunner: AppRunner
}

// TODO[sb]: Implement rename when clicking name of a selected row.
// There is currently no way to tell whether a row is selected from a column.

/** The component that contains the entire UI. */
function Dashboard(props: DashboardProps) {
    const { supportsLocalBackend, appRunner } = props

    const logger = loggerProvider.useLogger()
    const { accessToken, organization } = auth.useFullUserSession()
    const { backend } = backendProvider.useBackend()
    const { setBackend } = backendProvider.useSetBackend()
    const { modal } = modalProvider.useModal()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const [refresh, doRefresh] = hooks.useRefresh()

    const [query, setQuery] = react.useState('')
    const [loadingProjectManagerDidFail, setLoadingProjectManagerDidFail] = react.useState(false)
    const [directoryId, setDirectoryId] = react.useState(rootDirectoryId(organization.id))
    const [directoryStack, setDirectoryStack] = react.useState<
        backendModule.Asset<backendModule.AssetType.directory>[]
    >([])
    // Defined by the spec as `compact` by default, however it is not ready yet.
    const [columnDisplayMode, setColumnDisplayMode] = react.useState(ColumnDisplayMode.release)
    const [tab, setTab] = react.useState(Tab.dashboard)
    const [project, setProject] = react.useState<backendModule.Project | null>(null)
    const [selectedAssets, setSelectedAssets] = react.useState<backendModule.Asset[]>([])
    const [isFileBeingDragged, setIsFileBeingDragged] = react.useState(false)

    const [isLoadingAssets, setIsLoadingAssets] = react.useState(true)
    const [projectAssets, setProjectAssetsRaw] = react.useState<
        backendModule.Asset<backendModule.AssetType.project>[]
    >([])
    const [directoryAssets, setDirectoryAssetsRaw] = react.useState<
        backendModule.Asset<backendModule.AssetType.directory>[]
    >([])
    const [secretAssets, setSecretAssetsRaw] = react.useState<
        backendModule.Asset<backendModule.AssetType.secret>[]
    >([])
    const [fileAssets, setFileAssetsRaw] = react.useState<
        backendModule.Asset<backendModule.AssetType.file>[]
    >([])
    const [visibleProjectAssets, setVisibleProjectAssets] = react.useState<
        backendModule.Asset<backendModule.AssetType.project>[]
    >([])
    const [visibleDirectoryAssets, setVisibleDirectoryAssets] = react.useState<
        backendModule.Asset<backendModule.AssetType.directory>[]
    >([])
    const [visibleSecretAssets, setVisibleSecretAssets] = react.useState<
        backendModule.Asset<backendModule.AssetType.secret>[]
    >([])
    const [visibleFileAssets, setVisibleFileAssets] = react.useState<
        backendModule.Asset<backendModule.AssetType.file>[]
    >([])

    const listingLocalDirectoryAndWillFail =
        backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail
    const listingRemoteDirectoryAndWillFail =
        backend.type === backendModule.BackendType.remote && !organization.isEnabled
    const directory = directoryStack[directoryStack.length - 1]
    const parentDirectory = directoryStack[directoryStack.length - 2]

    react.useEffect(() => {
        const onProjectManagerLoadingFailed = () => {
            setLoadingProjectManagerDidFail(true)
        }
        document.addEventListener(
            projectManager.ProjectManagerEvents.loadingFailed,
            onProjectManagerLoadingFailed
        )
        return () => {
            document.removeEventListener(
                projectManager.ProjectManagerEvents.loadingFailed,
                onProjectManagerLoadingFailed
            )
        }
    }, [])

    react.useEffect(() => {
        if (backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail) {
            setIsLoadingAssets(false)
        }
    }, [isLoadingAssets, loadingProjectManagerDidFail, backend.type])

    react.useEffect(() => {
        if (supportsLocalBackend) {
            new localBackend.LocalBackend()
        }
    }, [])

    react.useEffect(() => {
        const onKeyDown = (event: KeyboardEvent) => {
            if (
                // On macOS, we need to check for combination of `alt` + `d` which is `∂` (`del`).
                (event.key === 'd' || event.key === '∂') &&
                event.ctrlKey &&
                event.altKey &&
                !event.shiftKey &&
                !event.metaKey
            ) {
                setTab(Tab.dashboard)
                const ideElement = document.getElementById(IDE_ELEMENT_ID)
                if (ideElement) {
                    ideElement.style.top = '-100vh'
                    ideElement.style.display = 'fixed'
                }
            }
        }
        document.addEventListener('keydown', onKeyDown)
        return () => {
            document.removeEventListener('keydown', onKeyDown)
        }
    }, [])

    const setProjectAssets = (
        newProjectAssets: backendModule.Asset<backendModule.AssetType.project>[]
    ) => {
        setProjectAssetsRaw(newProjectAssets)
        setVisibleProjectAssets(newProjectAssets.filter(asset => asset.title.includes(query)))
    }
    const setDirectoryAssets = (
        newDirectoryAssets: backendModule.Asset<backendModule.AssetType.directory>[]
    ) => {
        setDirectoryAssetsRaw(newDirectoryAssets)
        setVisibleDirectoryAssets(newDirectoryAssets.filter(asset => asset.title.includes(query)))
    }
    const setSecretAssets = (
        newSecretAssets: backendModule.Asset<backendModule.AssetType.secret>[]
    ) => {
        setSecretAssetsRaw(newSecretAssets)
        setVisibleSecretAssets(newSecretAssets.filter(asset => asset.title.includes(query)))
    }
    const setFileAssets = (newFileAssets: backendModule.Asset<backendModule.AssetType.file>[]) => {
        setFileAssetsRaw(newFileAssets)
        setVisibleFileAssets(newFileAssets.filter(asset => asset.title.includes(query)))
    }

    const exitDirectory = () => {
        setDirectoryId(parentDirectory?.id ?? rootDirectoryId(organization.id))
        setDirectoryStack(
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            directoryStack.slice(0, -1)
        )
    }

    const enterDirectory = (
        directoryAsset: backendModule.Asset<backendModule.AssetType.directory>
    ) => {
        setDirectoryId(directoryAsset.id)
        setDirectoryStack([...directoryStack, directoryAsset])
    }

    react.useEffect(() => {
        const cachedDirectoryStackJson = localStorage.getItem(DIRECTORY_STACK_KEY)
        if (cachedDirectoryStackJson) {
            // The JSON was inserted by the code below, so it will always have the right type.
            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            const cachedDirectoryStack: backendModule.Asset<backendModule.AssetType.directory>[] =
                JSON.parse(cachedDirectoryStackJson)
            setDirectoryStack(cachedDirectoryStack)
            const cachedDirectoryId = cachedDirectoryStack[cachedDirectoryStack.length - 1]?.id
            if (cachedDirectoryId) {
                setDirectoryId(cachedDirectoryId)
            }
        }
    }, [])

    react.useEffect(() => {
        if (directoryId === rootDirectoryId(organization.id)) {
            localStorage.removeItem(DIRECTORY_STACK_KEY)
        } else {
            localStorage.setItem(DIRECTORY_STACK_KEY, JSON.stringify(directoryStack))
        }
    }, [directoryStack])

    /** React components for the name column. */
    const nameRenderers: {
        [Type in backendModule.AssetType]: (asset: backendModule.Asset<Type>) => JSX.Element
    } = {
        [backendModule.AssetType.project]: projectAsset => (
            <div
                className="flex text-left items-center align-middle whitespace-nowrap"
                onClick={event => {
                    if (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey) {
                        setModal(() => (
                            <RenameModal
                                assetType={projectAsset.type}
                                name={projectAsset.title}
                                {...(backend.type === backendModule.BackendType.local
                                    ? {
                                          namePattern: '[A-Z][a-z]*(?:_\\d+|_[A-Z][a-z]*)*',
                                          title:
                                              'Names must be in Upper_Snake_Case. ' +
                                              '(Numbers (_0, _1) are also allowed.)',
                                      }
                                    : {})}
                                // TODO: Wait for backend implementation.
                                doRename={() => Promise.resolve()}
                                onSuccess={doRefresh}
                            />
                        ))
                    }
                }}
            >
                <ProjectActionButton
                    project={projectAsset}
                    appRunner={appRunner}
                    doRefresh={doRefresh}
                    onClose={() => {
                        setProject(null)
                    }}
                    openIde={async () => {
                        setTab(Tab.ide)
                        if (project?.projectId !== projectAsset.id) {
                            setProject(await backend.getProjectDetails(projectAsset.id))
                        }
                        const ideElement = document.getElementById(IDE_ELEMENT_ID)
                        if (ideElement) {
                            ideElement.style.top = ''
                            ideElement.style.display = 'absolute'
                        }
                    }}
                />
                <span className="px-2">{projectAsset.title}</span>
            </div>
        ),
        [backendModule.AssetType.directory]: directoryAsset => (
            <div
                className="flex text-left items-center align-middle whitespace-nowrap"
                onClick={event => {
                    if (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey) {
                        setModal(() => (
                            <RenameModal
                                assetType={directoryAsset.type}
                                name={directoryAsset.title}
                                // TODO: Wait for backend implementation.
                                doRename={() => Promise.resolve()}
                                onSuccess={doRefresh}
                            />
                        ))
                    }
                }}
                onDoubleClick={() => {
                    enterDirectory(directoryAsset)
                }}
            >
                {svg.DIRECTORY_ICON} <span className="px-2">{directoryAsset.title}</span>
            </div>
        ),
        [backendModule.AssetType.secret]: secret => (
            <div
                className="flex text-left items-center align-middle whitespace-nowrap"
                onClick={event => {
                    if (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey) {
                        setModal(() => (
                            <RenameModal
                                assetType={secret.type}
                                name={secret.title}
                                // FIXME[sb]: Wait for backend implementation.
                                doRename={() => Promise.resolve()}
                                onSuccess={doRefresh}
                            />
                        ))
                    }
                }}
            >
                {svg.SECRET_ICON} <span className="px-2">{secret.title}</span>
            </div>
        ),
        [backendModule.AssetType.file]: file => (
            <div
                className="flex text-left items-center align-middle whitespace-nowrap"
                onClick={event => {
                    if (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey) {
                        setModal(() => (
                            <RenameModal
                                assetType={file.type}
                                name={file.title}
                                // TODO: Wait for backend implementation.
                                doRename={() => Promise.resolve()}
                                onSuccess={doRefresh}
                            />
                        ))
                    }
                }}
            >
                {fileInfo.fileIcon()} <span className="px-2">{file.title}</span>
            </div>
        ),
    }

    /** React components for every column except for the name column. */
    // This is not a React component even though it contains JSX.
    // eslint-disable-next-line no-restricted-syntax
    const columnRenderer: Record<
        Exclude<Column, Column.name>,
        (asset: backendModule.Asset) => react.ReactNode
    > = {
        [Column.lastModified]: asset =>
            asset.modifiedAt && <>{dateTime.formatDateTime(new Date(asset.modifiedAt))}</>,
        [Column.sharedWith]: asset => (
            <>
                {(asset.permissions ?? []).map(user => (
                    <PermissionDisplay
                        key={user.user.organization_id}
                        permissions={PERMISSION[user.permission]}
                    >
                        {svg.DEFAULT_USER_ICON}
                    </PermissionDisplay>
                ))}
            </>
        ),
        [Column.docs]: () => <></>,
        [Column.labels]: () => {
            // This is not a React component even though it contains JSX.
            // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
            const onContextMenu = (event: react.MouseEvent) => {
                event.preventDefault()
                event.stopPropagation()
                setModal(() => (
                    <ContextMenu event={event}>
                        <ContextMenuEntry
                            disabled
                            onClick={() => {
                                // TODO: Wait for backend implementation.
                            }}
                        >
                            Rename label
                        </ContextMenuEntry>
                    </ContextMenu>
                ))
            }
            return <></>
        },
        [Column.dataAccess]: () => <></>,
        [Column.usagePlan]: () => <></>,
        [Column.engine]: () => <></>,
        [Column.ide]: () => <></>,
    }

    const renderer = <Type extends backendModule.AssetType>(column: Column, assetType: Type) => {
        return column === Column.name
            ? // This is type-safe only if we pass enum literals as `assetType`.
              // eslint-disable-next-line no-restricted-syntax
              (nameRenderers[assetType] as (asset: backendModule.Asset<Type>) => JSX.Element)
            : columnRenderer[column]
    }

    /** Heading element for every column. */
    const ColumnHeading = (column: Column, assetType: backendModule.AssetType) =>
        column === Column.name ? (
            assetType === backendModule.AssetType.project ? (
                <>{ASSET_TYPE_NAME[assetType]}</>
            ) : (
                <div className="inline-flex">
                    {ASSET_TYPE_NAME[assetType]}
                    <button
                        className="mx-1"
                        onClick={event => {
                            event.stopPropagation()
                            const buttonPosition =
                                // This type assertion is safe as this event handler is on a button.
                                // eslint-disable-next-line no-restricted-syntax
                                (event.target as HTMLButtonElement).getBoundingClientRect()
                            // This is a React component even though it doesn't contain JSX.
                            // eslint-disable-next-line no-restricted-syntax
                            const CreateForm = ASSET_TYPE_CREATE_FORM[assetType]
                            setModal(() => (
                                <CreateForm
                                    left={buttonPosition.left + window.scrollX}
                                    top={buttonPosition.top + window.scrollY}
                                    directoryId={directoryId}
                                    onSuccess={doRefresh}
                                />
                            ))
                        }}
                    >
                        {svg.ADD_ICON}
                    </button>
                </div>
            )
        ) : (
            <>{COLUMN_NAME[column]}</>
        )

    // The purpose of this effect is to enable search action.
    react.useEffect(() => {
        setVisibleProjectAssets(projectAssets.filter(asset => asset.title.includes(query)))
        setVisibleDirectoryAssets(directoryAssets.filter(asset => asset.title.includes(query)))
        setVisibleSecretAssets(secretAssets.filter(asset => asset.title.includes(query)))
        setVisibleFileAssets(fileAssets.filter(asset => asset.title.includes(query)))
    }, [query])

    const setAssets = (assets: backendModule.Asset[]) => {
        const newProjectAssets = assets.filter(
            backendModule.assetIsType(backendModule.AssetType.project)
        )
        const newDirectoryAssets = assets.filter(
            backendModule.assetIsType(backendModule.AssetType.directory)
        )
        const newSecretAssets = assets.filter(
            backendModule.assetIsType(backendModule.AssetType.secret)
        )
        const newFileAssets = assets.filter(backendModule.assetIsType(backendModule.AssetType.file))
        setProjectAssets(newProjectAssets)
        setDirectoryAssets(newDirectoryAssets)
        setSecretAssets(newSecretAssets)
        setFileAssets(newFileAssets)
    }

    hooks.useAsyncEffect(
        null,
        async signal => {
            if (listingLocalDirectoryAndWillFail) {
                // Do not `setIsLoadingAssets(false)`
            } else if (!listingRemoteDirectoryAndWillFail) {
                const assets = await backend.listDirectory({ parentId: directoryId })
                if (!signal.aborted) {
                    setIsLoadingAssets(false)
                    setAssets(assets)
                }
            } else {
                setIsLoadingAssets(false)
            }
        },
        [accessToken, directoryId, refresh, backend]
    )

    react.useEffect(() => {
        const onBlur = () => {
            setIsFileBeingDragged(false)
        }

        window.addEventListener('blur', onBlur)

        return () => {
            window.removeEventListener('blur', onBlur)
        }
    }, [])

    const handleEscapeKey = (event: react.KeyboardEvent<HTMLDivElement>) => {
        if (
            event.key === 'Escape' &&
            !event.ctrlKey &&
            !event.shiftKey &&
            !event.altKey &&
            !event.metaKey
        ) {
            if (modal) {
                event.preventDefault()
                unsetModal()
            }
        }
    }

    const openDropZone = (event: react.DragEvent<HTMLDivElement>) => {
        if (event.dataTransfer.types.includes('Files')) {
            setIsFileBeingDragged(true)
        }
    }

    const getNewProjectName = (templateName?: string | null): string => {
        const prefix = `${templateName ?? 'New_Project'}_`
        const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
        let highestProjectIndex = 0
        for (const projectAsset of projectAssets) {
            let projectIndex = projectNameTemplate.exec(projectAsset.title)?.groups?.projectIndex
            if (projectIndex) {
                highestProjectIndex = Math.max(highestProjectIndex, parseInt(projectIndex, 10))
            }
        }
        return `${prefix}${highestProjectIndex + 1}`
    }

    const handleCreateProject = async (templateId?: string | null) => {
        const projectName = getNewProjectName(templateId)
        const body: backendModule.CreateProjectRequestBody = {
            projectName,
            projectTemplateName: templateId ?? null,
            parentDirectoryId: directoryId,
        }
        await backend.createProject(body)
        doRefresh()
    }

    return (
        <div
            className={`flex flex-col relative select-none text-primary text-xs min-h-screen p-2 ${
                tab === Tab.dashboard ? '' : 'hidden'
            }`}
            onClick={event => {
                unsetModal()
                if (!event.shiftKey) {
                    setSelectedAssets([])
                }
            }}
            onKeyDown={handleEscapeKey}
            onDragEnter={openDropZone}
        >
            <TopBar
                supportsLocalBackend={supportsLocalBackend}
                projectName={project?.name ?? null}
                tab={tab}
                toggleTab={() => {
                    if (project && tab === Tab.dashboard) {
                        setTab(Tab.ide)
                        const ideElement = document.getElementById(IDE_ELEMENT_ID)
                        if (ideElement) {
                            ideElement.style.top = ''
                            ideElement.style.display = 'absolute'
                        }
                    } else {
                        setTab(Tab.dashboard)
                        const ideElement = document.getElementById(IDE_ELEMENT_ID)
                        if (ideElement) {
                            ideElement.style.top = '-100vh'
                            ideElement.style.display = 'fixed'
                        }
                    }
                }}
                setBackendType={newBackendType => {
                    if (newBackendType !== backend.type) {
                        setIsLoadingAssets(true)
                        setProjectAssets([])
                        setDirectoryAssets([])
                        setSecretAssets([])
                        setFileAssets([])
                        switch (newBackendType) {
                            case backendModule.BackendType.local:
                                setBackend(new localBackend.LocalBackend())
                                break
                            case backendModule.BackendType.remote: {
                                const headers = new Headers()
                                headers.append('Authorization', `Bearer ${accessToken}`)
                                const client = new http.Client(headers)
                                setBackend(new remoteBackendModule.RemoteBackend(client, logger))
                                break
                            }
                        }
                    }
                }}
                query={query}
                setQuery={setQuery}
            />
            {listingLocalDirectoryAndWillFail ? (
                <div className="grow grid place-items-center">
                    <div className="text-base text-center">
                        Could not connect to the Project Manager. Please try restarting{' '}
                        {common.PRODUCT_NAME}, or manually launching the Project Manager.
                    </div>
                </div>
            ) : listingRemoteDirectoryAndWillFail ? (
                <div className="grow grid place-items-center">
                    <div className="text-base text-center">
                        We will review your user details and enable the cloud experience for you
                        shortly.
                    </div>
                </div>
            ) : (
                <>
                    <Templates onTemplateClick={handleCreateProject} />
                    <div className="flex flex-row flex-nowrap my-2">
                        <h1 className="text-xl font-bold mx-4 self-center">Drive</h1>
                        <div className="flex flex-row flex-nowrap mx-4">
                            {backend.type === backendModule.BackendType.remote && (
                                <>
                                    <div className="bg-gray-100 rounded-l-full flex flex-row flex-nowrap items-center p-1 mx-0.5">
                                        {directory && (
                                            <>
                                                <button className="mx-2" onClick={exitDirectory}>
                                                    {parentDirectory?.title ?? '/'}
                                                </button>
                                                {svg.SMALL_RIGHT_ARROW_ICON}
                                            </>
                                        )}
                                        <span className="mx-2">{directory?.title ?? '/'}</span>
                                    </div>
                                    <div className="bg-gray-100 rounded-r-full flex flex-row flex-nowrap items-center mx-0.5">
                                        <div className="m-2">Shared with</div>
                                        <div></div>
                                    </div>
                                </>
                            )}
                            <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap px-1.5 py-1 mx-4">
                                <button
                                    className={`mx-1 ${
                                        backend.type === backendModule.BackendType.local
                                            ? 'opacity-50'
                                            : ''
                                    }`}
                                    disabled={backend.type === backendModule.BackendType.local}
                                    onClick={event => {
                                        event.stopPropagation()
                                        setModal(() => (
                                            <UploadFileModal
                                                directoryId={directoryId}
                                                onSuccess={doRefresh}
                                            />
                                        ))
                                    }}
                                >
                                    {svg.UPLOAD_ICON}
                                </button>
                                <button
                                    className={`mx-1 opacity-50`}
                                    disabled={true}
                                    onClick={event => {
                                        event.stopPropagation()
                                        unsetModal()
                                        /* TODO */
                                    }}
                                >
                                    {svg.DOWNLOAD_ICON}
                                </button>
                            </div>
                            {EXPERIMENTAL.columnModeSwitcher && (
                                <>
                                    <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap p-1.5 mx-4">
                                        <button
                                            className={`${
                                                columnDisplayMode === ColumnDisplayMode.all
                                                    ? 'bg-white shadow-soft'
                                                    : 'opacity-50'
                                            } rounded-full px-1.5`}
                                            onClick={() => {
                                                setColumnDisplayMode(ColumnDisplayMode.all)
                                            }}
                                        >
                                            All
                                        </button>
                                        <button
                                            className={`${
                                                columnDisplayMode === ColumnDisplayMode.compact
                                                    ? 'bg-white shadow-soft'
                                                    : 'opacity-50'
                                            } rounded-full px-1.5`}
                                            onClick={() => {
                                                setColumnDisplayMode(ColumnDisplayMode.compact)
                                            }}
                                        >
                                            Compact
                                        </button>
                                        <button
                                            className={`${
                                                columnDisplayMode === ColumnDisplayMode.docs
                                                    ? 'bg-white shadow-soft'
                                                    : 'opacity-50'
                                            } rounded-full px-1.5`}
                                            onClick={() => {
                                                setColumnDisplayMode(ColumnDisplayMode.docs)
                                            }}
                                        >
                                            Docs
                                        </button>
                                        <button
                                            className={`${
                                                columnDisplayMode === ColumnDisplayMode.settings
                                                    ? 'bg-white shadow-soft'
                                                    : 'opacity-50'
                                            } rounded-full px-1.5`}
                                            onClick={() => {
                                                setColumnDisplayMode(ColumnDisplayMode.settings)
                                            }}
                                        >
                                            Settings
                                        </button>
                                    </div>
                                </>
                            )}
                        </div>
                    </div>
                    <table className="table-fixed items-center border-collapse mt-2">
                        <tbody>
                            <tr className="h-10">
                                {columnsFor(columnDisplayMode, backend.type).map(column => (
                                    <td key={column} className={COLUMN_CSS_CLASS[column]} />
                                ))}
                            </tr>
                            <Rows<backendModule.Asset<backendModule.AssetType.project>>
                                items={visibleProjectAssets}
                                getKey={proj => proj.id}
                                isLoading={isLoadingAssets}
                                placeholder={
                                    <span className="opacity-75">
                                        You have no project yet. Go ahead and create one using the
                                        form above.
                                    </span>
                                }
                                columns={columnsFor(columnDisplayMode, backend.type).map(
                                    column => ({
                                        id: column,
                                        heading: ColumnHeading(
                                            column,
                                            backendModule.AssetType.project
                                        ),
                                        render: renderer(column, backendModule.AssetType.project),
                                    })
                                )}
                                onClick={(projectAsset, event) => {
                                    event.stopPropagation()
                                    unsetModal()
                                    setSelectedAssets(
                                        event.shiftKey
                                            ? [...selectedAssets, projectAsset]
                                            : [projectAsset]
                                    )
                                }}
                                onContextMenu={(projectAsset, event) => {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    const doOpenForEditing = () => {
                                        // FIXME[sb]: Switch to IDE tab
                                        // once merged with `show-and-open-workspace` branch.
                                    }
                                    const doOpenAsFolder = () => {
                                        // FIXME[sb]: Uncomment once backend support
                                        // is in place.
                                        // The following code does not typecheck
                                        // since `ProjectId`s are not `DirectoryId`s.
                                        // enterDirectory(projectAsset)
                                    }
                                    // This is not a React component even though it contains JSX.
                                    // eslint-disable-next-line no-restricted-syntax
                                    const doRename = () => {
                                        setModal(() => (
                                            <RenameModal
                                                name={projectAsset.title}
                                                assetType={projectAsset.type}
                                                {...(backend.type ===
                                                backendModule.BackendType.local
                                                    ? {
                                                          namePattern:
                                                              '[A-Z][a-z]*(?:_\\d+|_[A-Z][a-z]*)*',
                                                          title:
                                                              'Names must be in Upper_Snake_Case. ' +
                                                              '(Numbers (_0, _1) are also allowed.)',
                                                      }
                                                    : {})}
                                                doRename={async name => {
                                                    await backend.projectUpdate(projectAsset.id, {
                                                        ami: null,
                                                        ideVersion: null,
                                                        projectName: name,
                                                    })
                                                }}
                                                onSuccess={doRefresh}
                                            />
                                        ))
                                    }
                                    // This is not a React component even though it contains JSX.
                                    // eslint-disable-next-line no-restricted-syntax
                                    const doDelete = () => {
                                        setModal(() => (
                                            <ConfirmDeleteModal
                                                name={projectAsset.title}
                                                assetType={projectAsset.type}
                                                doDelete={() =>
                                                    backend.deleteProject(projectAsset.id)
                                                }
                                                onSuccess={doRefresh}
                                            />
                                        ))
                                    }
                                    setModal(() => (
                                        <ContextMenu event={event}>
                                            <ContextMenuEntry disabled onClick={doOpenForEditing}>
                                                Open for editing
                                            </ContextMenuEntry>
                                            {backend.type !== backendModule.BackendType.local && (
                                                <ContextMenuEntry disabled onClick={doOpenAsFolder}>
                                                    Open as folder
                                                </ContextMenuEntry>
                                            )}
                                            <ContextMenuEntry onClick={doRename}>
                                                Rename
                                            </ContextMenuEntry>
                                            <ContextMenuEntry onClick={doDelete}>
                                                <span className="text-red-700">Delete</span>
                                            </ContextMenuEntry>
                                        </ContextMenu>
                                    ))
                                }}
                            />
                            {backend.type === backendModule.BackendType.remote &&
                                (remoteBackend => (
                                    <>
                                        <tr className="h-10" />
                                        <Rows<
                                            backendModule.Asset<backendModule.AssetType.directory>
                                        >
                                            items={visibleDirectoryAssets}
                                            getKey={dir => dir.id}
                                            isLoading={isLoadingAssets}
                                            placeholder={
                                                <span className="opacity-75">
                                                    This directory does not contain any
                                                    subdirectories
                                                    {query ? ' matching your query' : ''}.
                                                </span>
                                            }
                                            columns={columnsFor(
                                                columnDisplayMode,
                                                backend.type
                                            ).map(column => ({
                                                id: column,
                                                heading: ColumnHeading(
                                                    column,
                                                    backendModule.AssetType.directory
                                                ),
                                                render: renderer(
                                                    column,
                                                    backendModule.AssetType.directory
                                                ),
                                            }))}
                                            onClick={(directoryAsset, event) => {
                                                event.stopPropagation()
                                                unsetModal()
                                                setSelectedAssets(
                                                    event.shiftKey
                                                        ? [...selectedAssets, directoryAsset]
                                                        : [directoryAsset]
                                                )
                                            }}
                                            onContextMenu={(_directory, event) => {
                                                event.preventDefault()
                                                event.stopPropagation()
                                                setModal(() => (
                                                    <ContextMenu event={event}></ContextMenu>
                                                ))
                                            }}
                                        />
                                        <tr className="h-10" />
                                        <Rows<backendModule.Asset<backendModule.AssetType.secret>>
                                            items={visibleSecretAssets}
                                            getKey={secret => secret.id}
                                            isLoading={isLoadingAssets}
                                            placeholder={
                                                <span className="opacity-75">
                                                    This directory does not contain any secrets
                                                    {query ? ' matching your query' : ''}.
                                                </span>
                                            }
                                            columns={columnsFor(
                                                columnDisplayMode,
                                                backend.type
                                            ).map(column => ({
                                                id: column,
                                                heading: ColumnHeading(
                                                    column,
                                                    backendModule.AssetType.secret
                                                ),
                                                render: renderer(
                                                    column,
                                                    backendModule.AssetType.secret
                                                ),
                                            }))}
                                            onClick={(secret, event) => {
                                                event.stopPropagation()
                                                unsetModal()
                                                setSelectedAssets(
                                                    event.shiftKey
                                                        ? [...selectedAssets, secret]
                                                        : [secret]
                                                )
                                            }}
                                            onContextMenu={(secret, event) => {
                                                event.preventDefault()
                                                event.stopPropagation()
                                                // This is not a React component even though it contains JSX.
                                                // eslint-disable-next-line no-restricted-syntax
                                                const doDelete = () => {
                                                    setModal(() => (
                                                        <ConfirmDeleteModal
                                                            name={secret.title}
                                                            assetType={secret.type}
                                                            doDelete={() =>
                                                                remoteBackend.deleteSecret(
                                                                    secret.id
                                                                )
                                                            }
                                                            onSuccess={doRefresh}
                                                        />
                                                    ))
                                                }
                                                setModal(() => (
                                                    <ContextMenu event={event}>
                                                        <ContextMenuEntry onClick={doDelete}>
                                                            <span className="text-red-700">
                                                                Delete
                                                            </span>
                                                        </ContextMenuEntry>
                                                    </ContextMenu>
                                                ))
                                            }}
                                        />
                                        <tr className="h-10" />
                                        <Rows<backendModule.Asset<backendModule.AssetType.file>>
                                            items={visibleFileAssets}
                                            getKey={file => file.id}
                                            isLoading={isLoadingAssets}
                                            placeholder={
                                                <span className="opacity-75">
                                                    This directory does not contain any files
                                                    {query ? ' matching your query' : ''}.
                                                </span>
                                            }
                                            columns={columnsFor(
                                                columnDisplayMode,
                                                backend.type
                                            ).map(column => ({
                                                id: column,
                                                heading: ColumnHeading(
                                                    column,
                                                    backendModule.AssetType.file
                                                ),
                                                render: renderer(
                                                    column,
                                                    backendModule.AssetType.file
                                                ),
                                            }))}
                                            onClick={(file, event) => {
                                                event.stopPropagation()
                                                unsetModal()
                                                setSelectedAssets(
                                                    event.shiftKey
                                                        ? [...selectedAssets, file]
                                                        : [file]
                                                )
                                            }}
                                            onContextMenu={(file, event) => {
                                                event.preventDefault()
                                                event.stopPropagation()
                                                const doCopy = () => {
                                                    // TODO: Wait for backend endpoint.
                                                }
                                                const doCut = () => {
                                                    // TODO: Wait for backend endpoint.
                                                }
                                                // This is not a React component even though it contains JSX.
                                                // eslint-disable-next-line no-restricted-syntax
                                                const doDelete = () => {
                                                    setModal(() => (
                                                        <ConfirmDeleteModal
                                                            name={file.title}
                                                            assetType={file.type}
                                                            doDelete={() =>
                                                                remoteBackend.deleteFile(file.id)
                                                            }
                                                            onSuccess={doRefresh}
                                                        />
                                                    ))
                                                }
                                                const doDownload = () => {
                                                    /** TODO: Wait for backend endpoint. */
                                                }
                                                setModal(() => (
                                                    <ContextMenu event={event}>
                                                        <ContextMenuEntry disabled onClick={doCopy}>
                                                            Copy
                                                        </ContextMenuEntry>
                                                        <ContextMenuEntry disabled onClick={doCut}>
                                                            Cut
                                                        </ContextMenuEntry>
                                                        <ContextMenuEntry onClick={doDelete}>
                                                            <span className="text-red-700">
                                                                Delete
                                                            </span>
                                                        </ContextMenuEntry>
                                                        <ContextMenuEntry
                                                            disabled
                                                            onClick={doDownload}
                                                        >
                                                            Download
                                                        </ContextMenuEntry>
                                                    </ContextMenu>
                                                ))
                                            }}
                                        />
                                    </>
                                ))(backend)}
                        </tbody>
                    </table>
                    {isFileBeingDragged && backend.type === backendModule.BackendType.remote ? (
                        <div
                            className="text-white text-lg fixed w-screen h-screen inset-0 bg-primary grid place-items-center"
                            onDragLeave={() => {
                                setIsFileBeingDragged(false)
                            }}
                            onDragOver={event => {
                                event.preventDefault()
                            }}
                            onDrop={async event => {
                                event.preventDefault()
                                setIsFileBeingDragged(false)
                                await uploadMultipleFiles.uploadMultipleFiles(
                                    backend,
                                    directoryId,
                                    Array.from(event.dataTransfer.files)
                                )
                                doRefresh()
                            }}
                        >
                            Drop to upload files.
                        </div>
                    ) : null}
                </>
            )}
            {/* This should be just `{modal}`, however TypeScript incorrectly throws an error. */}
            {project && <Ide project={project} appRunner={appRunner} />}
            {modal && <>{modal}</>}
        </div>
    )
}

export default Dashboard
