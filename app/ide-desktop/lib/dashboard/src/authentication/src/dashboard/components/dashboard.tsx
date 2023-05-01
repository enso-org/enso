/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as react from 'react'

import * as projectManagerModule from 'enso-content/src/project_manager'

import * as auth from '../../authentication/providers/auth'
import * as backend from '../service'
import * as fileInfo from '../../fileInfo'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as newtype from '../../newtype'
import * as platformModule from '../../platform'
import * as svg from '../../components/svg'
import * as uploadMultipleFiles from '../../uploadMultipleFiles'

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
import ProjectCreateForm from './projectCreateForm'
import SecretCreateForm from './secretCreateForm'

// =============
// === Types ===
// =============

/** Main content of the screen. Only one should be visible at a time. */
export enum Tab {
    dashboard = 'dashboard',
    ide = 'ide',
}

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
    backend: backend.Backend
    directoryId: backend.DirectoryId
    onSuccess: () => void
}

// =================
// === Constants ===
// =================

/** Enables features which are not ready for release,
 * and so are intentionally disabled for release builds. */
// This type annotation is explicit to undo TypeScript narrowing to `false`,
// which result in errors about unused code.
// eslint-disable-next-line @typescript-eslint/no-inferrable-types
const EXPERIMENTAL: boolean = true

/** The `localStorage` key under which the ID of the current directory is stored. */
const DIRECTORY_STACK_KEY = 'enso-dashboard-directory-stack'

/** English names for the name column. */
const ASSET_TYPE_NAME: Record<backend.AssetType, string> = {
    [backend.AssetType.project]: 'Projects',
    [backend.AssetType.file]: 'Files',
    [backend.AssetType.secret]: 'Secrets',
    [backend.AssetType.directory]: 'Folders',
} as const

/** Forms to create each asset type. */
const ASSET_TYPE_CREATE_FORM: Record<backend.AssetType, (props: CreateFormProps) => JSX.Element> = {
    [backend.AssetType.project]: ProjectCreateForm,
    [backend.AssetType.file]: FileCreateForm,
    [backend.AssetType.secret]: SecretCreateForm,
    [backend.AssetType.directory]: DirectoryCreateForm,
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

/** The corresponding `Permissions` for each backend `PermissionAction`. */
const PERMISSION: Record<backend.PermissionAction, permissionDisplay.Permissions> = {
    [backend.PermissionAction.own]: { type: permissionDisplay.Permission.owner },
    [backend.PermissionAction.execute]: {
        type: permissionDisplay.Permission.regular,
        read: false,
        write: false,
        docsWrite: false,
        exec: true,
    },
    [backend.PermissionAction.edit]: {
        type: permissionDisplay.Permission.regular,
        read: false,
        write: true,
        docsWrite: false,
        exec: false,
    },
    [backend.PermissionAction.read]: {
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

/** Returns the id of the root directory for a user or organization. */
function rootDirectoryId(userOrOrganizationId: backend.UserOrOrganizationId) {
    return newtype.asNewtype<backend.DirectoryId>(
        userOrOrganizationId.replace(/^organization-/, `${backend.AssetType.directory}-`)
    )
}

// =================
// === Dashboard ===
// =================

interface BaseDashboardProps {
    logger: loggerProvider.Logger
    platform: platformModule.Platform
}

interface DesktopDashboardProps extends BaseDashboardProps {
    platform: platformModule.Platform.desktop
    projectManager: projectManagerModule.ProjectManager
}

interface OtherDashboardProps extends BaseDashboardProps {
    platform: Exclude<platformModule.Platform, platformModule.Platform.desktop>
}

export type DashboardProps = DesktopDashboardProps | OtherDashboardProps

// TODO[sb]: Implement rename when clicking name of a selected row.
// There is currently no way to tell whether a row is selected from a column.

function Dashboard(props: DashboardProps) {
    const { logger, platform } = props

    const { accessToken, organization } = auth.useFullUserSession()
    const backendService = backend.createBackend(accessToken, logger)
    const { modal } = modalProvider.useModal()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const [refresh, doRefresh] = hooks.useRefresh()

    const [query, setQuery] = react.useState('')
    const [directoryId, setDirectoryId] = react.useState(rootDirectoryId(organization.id))
    const [directoryStack, setDirectoryStack] = react.useState<
        backend.Asset<backend.AssetType.directory>[]
    >([])
    // Defined by the spec as `compact` by default, however it is not ready yet.
    const [columnDisplayMode, setColumnDisplayMode] = react.useState(ColumnDisplayMode.release)

    const [projectAssets, setProjectAssetsRaw] = react.useState<
        backend.Asset<backend.AssetType.project>[]
    >([])
    const [directoryAssets, setDirectoryAssetsRaw] = react.useState<
        backend.Asset<backend.AssetType.directory>[]
    >([])
    const [secretAssets, setSecretAssetsRaw] = react.useState<
        backend.Asset<backend.AssetType.secret>[]
    >([])
    const [fileAssets, setFileAssetsRaw] = react.useState<backend.Asset<backend.AssetType.file>[]>(
        []
    )
    const [visibleProjectAssets, setVisibleProjectAssets] = react.useState<
        backend.Asset<backend.AssetType.project>[]
    >([])
    const [visibleDirectoryAssets, setVisibleDirectoryAssets] = react.useState<
        backend.Asset<backend.AssetType.directory>[]
    >([])
    const [visibleSecretAssets, setVisibleSecretAssets] = react.useState<
        backend.Asset<backend.AssetType.secret>[]
    >([])
    const [visibleFileAssets, setVisibleFileAssets] = react.useState<
        backend.Asset<backend.AssetType.file>[]
    >([])

    const [tab, setTab] = react.useState(Tab.dashboard)
    const [project, setProject] = react.useState<backend.Project | null>(null)

    const [selectedAssets, setSelectedAssets] = react.useState<backend.Asset[]>([])
    const [isFileBeingDragged, setIsFileBeingDragged] = react.useState(false)

    const directory = directoryStack[directoryStack.length - 1]
    const parentDirectory = directoryStack[directoryStack.length - 2]

    function setProjectAssets(newProjectAssets: backend.Asset<backend.AssetType.project>[]) {
        setProjectAssetsRaw(newProjectAssets)
        setVisibleProjectAssets(newProjectAssets.filter(asset => asset.title.includes(query)))
    }
    function setDirectoryAssets(newDirectoryAssets: backend.Asset<backend.AssetType.directory>[]) {
        setDirectoryAssetsRaw(newDirectoryAssets)
        setVisibleDirectoryAssets(newDirectoryAssets.filter(asset => asset.title.includes(query)))
    }
    function setSecretAssets(newSecretAssets: backend.Asset<backend.AssetType.secret>[]) {
        setSecretAssetsRaw(newSecretAssets)
        setVisibleSecretAssets(newSecretAssets.filter(asset => asset.title.includes(query)))
    }
    function setFileAssets(newFileAssets: backend.Asset<backend.AssetType.file>[]) {
        setFileAssetsRaw(newFileAssets)
        setVisibleFileAssets(newFileAssets.filter(asset => asset.title.includes(query)))
    }

    function exitDirectory() {
        setDirectoryId(parentDirectory?.id ?? rootDirectoryId(organization.id))
        setDirectoryStack(
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            directoryStack.slice(0, -1)
        )
    }

    function enterDirectory(directoryAsset: backend.Asset<backend.AssetType.directory>) {
        setDirectoryId(directoryAsset.id)
        setDirectoryStack([...directoryStack, directoryAsset])
    }

    react.useEffect(() => {
        const cachedDirectoryStackJson = localStorage.getItem(DIRECTORY_STACK_KEY)
        if (cachedDirectoryStackJson) {
            // The JSON was inserted by the code below, so it will always have the right type.
            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            const cachedDirectoryStack: backend.Asset<backend.AssetType.directory>[] =
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
        [Type in backend.AssetType]: (asset: backend.Asset<Type>) => JSX.Element
    } = {
        [backend.AssetType.project]: projectAsset => (
            <div
                className="flex text-left items-center align-middle whitespace-nowrap"
                onClick={event => {
                    if (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey) {
                        setModal(() => (
                            <RenameModal
                                assetType={projectAsset.type}
                                name={projectAsset.title}
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
                    openIde={async () => {
                        setTab(Tab.ide)
                        setProject(await backendService.getProjectDetails(projectAsset.id))
                    }}
                />
                <span className="px-2">{projectAsset.title}</span>
            </div>
        ),
        [backend.AssetType.directory]: directoryAsset => (
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
        [backend.AssetType.secret]: secret => (
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
        [backend.AssetType.file]: file => (
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
                {fileInfo.fileIcon(fileInfo.fileExtension(file.title))}{' '}
                <span className="px-2">{file.title}</span>
            </div>
        ),
    }

    /** React components for every column except for the name column. */
    const columnRenderer: Record<
        Exclude<Column, Column.name>,
        (asset: backend.Asset) => JSX.Element
    > = {
        [Column.lastModified]: () => <></>,
        [Column.sharedWith]: asset => (
            <>
                {(asset.permissions ?? []).map(user => (
                    <PermissionDisplay
                        key={user.user.organization_id}
                        permissions={PERMISSION[user.permission]}
                    >
                        <img
                            className="rounded-full h-6"
                            src="https://faces-img.xcdn.link/image-lorem-face-4742.jpg"
                        />
                    </PermissionDisplay>
                ))}
            </>
        ),
        [Column.docs]: () => <></>,
        [Column.labels]: () => {
            // This is not a React component even though it contains JSX.
            // eslint-disable-next-line no-restricted-syntax
            function onContextMenu(event: react.MouseEvent) {
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

    function renderer<Type extends backend.AssetType>(column: Column, assetType: Type) {
        return column === Column.name
            ? // This is type-safe only if we pass enum literals as `assetType`.

              // eslint-disable-next-line no-restricted-syntax
              (nameRenderers[assetType] as (asset: backend.Asset<Type>) => JSX.Element)
            : columnRenderer[column]
    }

    /** Heading element for every column. */
    function ColumnHeading(column: Column, assetType: backend.AssetType) {
        return column === Column.name ? (
            <div className="inline-flex">
                {ASSET_TYPE_NAME[assetType]}
                <button
                    className="mx-1"
                    onClick={event => {
                        event.stopPropagation()
                        const buttonPosition =
                            // This type assertion is safe as this event handler is on a `button`.
                            // eslint-disable-next-line no-restricted-syntax
                            (event.target as HTMLButtonElement).getBoundingClientRect()
                        // This is a React component even though it doesn't contain JSX.
                        // eslint-disable-next-line no-restricted-syntax
                        const CreateForm = ASSET_TYPE_CREATE_FORM[assetType]
                        setModal(() => (
                            <CreateForm
                                left={buttonPosition.left}
                                top={buttonPosition.top}
                                // FIXME[sb]: Don't pass outdated `doRefresh` - maybe `backendService` too.
                                backend={backendService}
                                directoryId={directoryId}
                                onSuccess={doRefresh}
                            />
                        ))
                    }}
                >
                    {svg.ADD_ICON}
                </button>
            </div>
        ) : (
            <>{COLUMN_NAME[column]}</>
        )
    }

    // The purpose of this effect is to enable search action.
    react.useEffect(() => {
        setVisibleProjectAssets(projectAssets.filter(asset => asset.title.includes(query)))
        setVisibleDirectoryAssets(directoryAssets.filter(asset => asset.title.includes(query)))
        setVisibleSecretAssets(secretAssets.filter(asset => asset.title.includes(query)))
        setVisibleFileAssets(fileAssets.filter(asset => asset.title.includes(query)))
    }, [query])

    function setAssets(assets: backend.Asset[]) {
        const newProjectAssets = assets.filter(backend.assetIsType(backend.AssetType.project))
        const newDirectoryAssets = assets.filter(backend.assetIsType(backend.AssetType.directory))
        const newSecretAssets = assets.filter(backend.assetIsType(backend.AssetType.secret))
        const newFileAssets = assets.filter(backend.assetIsType(backend.AssetType.file))
        setProjectAssets(newProjectAssets)
        setDirectoryAssets(newDirectoryAssets)
        setSecretAssets(newSecretAssets)
        setFileAssets(newFileAssets)
    }

    hooks.useAsyncEffect(
        null,
        async signal => {
            let assets: backend.Asset[]

            switch (platform) {
                case platformModule.Platform.cloud: {
                    assets = await backendService.listDirectory({
                        parentId: directoryId,
                    })
                    break
                }
                case platformModule.Platform.desktop: {
                    const result = await props.projectManager.listProjects({})
                    const localProjects = result.result.projects
                    assets = []
                    for (const localProject of localProjects) {
                        assets.push({
                            type: backend.AssetType.project,
                            title: localProject.name,
                            id: localProject.id,
                            parentId: '',
                            permissions: null,
                        })
                    }
                    break
                }
            }
            if (!signal.aborted) {
                setAssets(assets)
            }
        },
        [accessToken, directoryId, refresh]
    )

    react.useEffect(() => {
        function onBlur() {
            setIsFileBeingDragged(false)
        }

        window.addEventListener('blur', onBlur)

        return () => {
            window.removeEventListener('blur', onBlur)
        }
    }, [])

    function handleEscapeKey(event: react.KeyboardEvent<HTMLDivElement>) {
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

    function openDropZone(event: react.DragEvent<HTMLDivElement>) {
        if (event.dataTransfer.types.includes('Files')) {
            setIsFileBeingDragged(true)
        }
    }

    function getNewProjectName(templateName?: string | null): string {
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

    async function handleCreateProject(templateName: string | null) {
        const projectName = getNewProjectName(templateName)
        switch (platform) {
            case platformModule.Platform.cloud: {
                const body: backend.CreateProjectRequestBody = {
                    projectName,
                    projectTemplateName:
                        templateName?.replace(/_/g, '').toLocaleLowerCase() ?? null,
                    parentDirectoryId: directoryId,
                }
                if (templateName) {
                    body.projectTemplateName = templateName.replace(/_/g, '').toLocaleLowerCase()
                }
                const projectAsset = await backendService.createProject(body)
                setProjectAssets([
                    ...projectAssets,
                    {
                        type: backend.AssetType.project,
                        title: projectAsset.name,
                        id: projectAsset.projectId,
                        parentId: '',
                        permissions: [],
                    },
                ])
                break
            }
            case platformModule.Platform.desktop: {
                const result = await props.projectManager.createProject({
                    name: newtype.asNewtype<projectManagerModule.ProjectName>(projectName),
                    ...(templateName ? { projectTemplate: templateName } : {}),
                })
                const newProject = result.result
                setProjectAssets([
                    ...projectAssets,
                    {
                        type: backend.AssetType.project,
                        title: projectName,
                        id: newProject.projectId,
                        parentId: '',
                        permissions: [],
                    },
                ])
                break
            }
        }
    }

    return (
        <div
            className={`select-none text-primary text-xs min-h-screen ${
                tab === Tab.dashboard ? '' : 'hidden'
            }`}
            onClick={unsetModal}
            onKeyDown={handleEscapeKey}
            onDragEnter={openDropZone}
        >
            <div>
                <TopBar
                    projectName={project?.name ?? null}
                    tab={tab}
                    toggleTab={() => {
                        if (project && tab === Tab.dashboard) {
                            setTab(Tab.ide)
                        } else {
                            setTab(Tab.dashboard)
                        }
                    }}
                    query={query}
                    setQuery={setQuery}
                />
                <Templates onTemplateClick={handleCreateProject} />
                <div className="flex flex-row flex-nowrap">
                    <h1 className="text-xl font-bold mx-4 self-center">Drive</h1>
                    <div className="flex flex-row flex-nowrap mx-4">
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
                            <div className="-m-1">
                                <PermissionDisplay
                                    permissions={{ type: permissionDisplay.Permission.admin }}
                                >
                                    marketing
                                </PermissionDisplay>
                            </div>
                        </div>
                        <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap px-1.5 py-1 mx-4">
                            <button
                                className="mx-1"
                                onClick={event => {
                                    event.stopPropagation()
                                    setModal(() => (
                                        <UploadFileModal
                                            backend={backendService}
                                            directoryId={directoryId}
                                            onSuccess={doRefresh}
                                        />
                                    ))
                                }}
                            >
                                {svg.UPLOAD_ICON}
                            </button>
                            <button
                                className={`mx-1 ${
                                    selectedAssets.length === 0 ? 'opacity-50' : ''
                                }`}
                                disabled={selectedAssets.length === 0}
                                onClick={event => {
                                    event.stopPropagation()
                                    /* TODO */
                                }}
                            >
                                {svg.DOWNLOAD_ICON}
                            </button>
                        </div>
                        {EXPERIMENTAL && (
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
            </div>
            <table className="items-center w-full bg-transparent border-collapse">
                <tbody>
                    <tr className="h-10" />
                    <Rows<backend.Asset<backend.AssetType.project>>
                        items={visibleProjectAssets}
                        getKey={proj => proj.id}
                        placeholder={
                            <span className="opacity-75">
                                You have no project yet. Go ahead and create one using the form
                                above.
                            </span>
                        }
                        columns={COLUMNS_FOR[columnDisplayMode].map(column => ({
                            id: column,
                            heading: ColumnHeading(column, backend.AssetType.project),
                            render: renderer(column, backend.AssetType.project),
                        }))}
                        onClick={projectAsset => {
                            setSelectedAssets([projectAsset])
                        }}
                        onContextMenu={(projectAsset, event) => {
                            event.preventDefault()
                            event.stopPropagation()
                            function doOpenForEditing() {
                                // FIXME[sb]: Switch to IDE tab
                                // once merged with `show-and-open-workspace` branch.
                            }
                            function doOpenAsFolder() {
                                // FIXME[sb]: Uncomment once backend support
                                // is in place.
                                // The following code does not typecheck
                                // since `ProjectId`s are not `DirectoryId`s.
                                // enterDirectory(projectAsset)
                            }
                            // This is not a React component even though it contains JSX.
                            // eslint-disable-next-line no-restricted-syntax
                            function doRename() {
                                setModal(() => (
                                    <RenameModal
                                        name={projectAsset.title}
                                        assetType={projectAsset.type}
                                        // FIXME[sb]: Replace with API call
                                        // when implemented in backend.
                                        doRename={() => Promise.resolve()}
                                        onSuccess={doRefresh}
                                    />
                                ))
                            }
                            // This is not a React component even though it contains JSX.
                            // eslint-disable-next-line no-restricted-syntax
                            function doDelete() {
                                setModal(() => (
                                    <ConfirmDeleteModal
                                        name={projectAsset.title}
                                        assetType={projectAsset.type}
                                        doDelete={() =>
                                            backendService.deleteProject(projectAsset.id)
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
                                    <ContextMenuEntry disabled onClick={doOpenAsFolder}>
                                        Open as folder
                                    </ContextMenuEntry>
                                    <ContextMenuEntry disabled onClick={doRename}>
                                        Rename
                                    </ContextMenuEntry>
                                    <ContextMenuEntry onClick={doDelete}>
                                        <span className="text-red-700">Delete</span>
                                    </ContextMenuEntry>
                                </ContextMenu>
                            ))
                        }}
                    />
                    {platform === platformModule.Platform.cloud && (
                        <>
                            <tr className="h-10" />
                            <Rows<backend.Asset<backend.AssetType.directory>>
                                items={visibleDirectoryAssets}
                                getKey={dir => dir.id}
                                placeholder={
                                    <span className="opacity-75">
                                        This directory does not contain any subdirectories
                                        {query ? ' matching your query' : ''}.
                                    </span>
                                }
                                columns={COLUMNS_FOR[columnDisplayMode].map(column => ({
                                    id: column,
                                    heading: ColumnHeading(column, backend.AssetType.directory),
                                    render: renderer(column, backend.AssetType.directory),
                                }))}
                                onClick={directoryAsset => {
                                    setSelectedAssets([directoryAsset])
                                }}
                                onContextMenu={(_directory, event) => {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    setModal(() => <ContextMenu event={event}></ContextMenu>)
                                }}
                            />
                            <tr className="h-10" />
                            <Rows<backend.Asset<backend.AssetType.secret>>
                                items={visibleSecretAssets}
                                getKey={secret => secret.id}
                                placeholder={
                                    <span className="opacity-75">
                                        This directory does not contain any secrets
                                        {query ? ' matching your query' : ''}.
                                    </span>
                                }
                                columns={COLUMNS_FOR[columnDisplayMode].map(column => ({
                                    id: column,
                                    heading: ColumnHeading(column, backend.AssetType.secret),
                                    render: renderer(column, backend.AssetType.secret),
                                }))}
                                onClick={secret => {
                                    setSelectedAssets([secret])
                                }}
                                onContextMenu={(secret, event) => {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    // This is not a React component even though it contains JSX.
                                    // eslint-disable-next-line no-restricted-syntax
                                    function doDelete() {
                                        setModal(() => (
                                            <ConfirmDeleteModal
                                                name={secret.title}
                                                assetType={secret.type}
                                                doDelete={() =>
                                                    backendService.deleteSecret(secret.id)
                                                }
                                                onSuccess={doRefresh}
                                            />
                                        ))
                                    }
                                    setModal(() => (
                                        <ContextMenu event={event}>
                                            <ContextMenuEntry onClick={doDelete}>
                                                <span className="text-red-700">Delete</span>
                                            </ContextMenuEntry>
                                        </ContextMenu>
                                    ))
                                }}
                            />
                            <tr className="h-10" />
                            <Rows<backend.Asset<backend.AssetType.file>>
                                items={visibleFileAssets}
                                getKey={file => file.id}
                                placeholder={
                                    <span className="opacity-75">
                                        This directory does not contain any files
                                        {query ? ' matching your query' : ''}.
                                    </span>
                                }
                                columns={COLUMNS_FOR[columnDisplayMode].map(column => ({
                                    id: column,
                                    heading: ColumnHeading(column, backend.AssetType.file),
                                    render: renderer(column, backend.AssetType.file),
                                }))}
                                onClick={file => {
                                    setSelectedAssets([file])
                                }}
                                onContextMenu={(file, event) => {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    function doCopy() {
                                        /** TODO: Call endpoint for copying file. */
                                    }
                                    function doCut() {
                                        /** TODO: Call endpoint for downloading file. */
                                    }
                                    // This is not a React component even though it contains JSX.
                                    // eslint-disable-next-line no-restricted-syntax
                                    function doDelete() {
                                        setModal(() => (
                                            <ConfirmDeleteModal
                                                name={file.title}
                                                assetType={file.type}
                                                doDelete={() => backendService.deleteFile(file.id)}
                                                onSuccess={doRefresh}
                                            />
                                        ))
                                    }
                                    function doDownload() {
                                        /** TODO: Call endpoint for downloading file. */
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
                                                <span className="text-red-700">Delete</span>
                                            </ContextMenuEntry>
                                            <ContextMenuEntry disabled onClick={doDownload}>
                                                Download
                                            </ContextMenuEntry>
                                        </ContextMenu>
                                    ))
                                }}
                            />
                        </>
                    )}
                </tbody>
            </table>
            {isFileBeingDragged ? (
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
                            backendService,
                            directoryId,
                            Array.from(event.dataTransfer.files)
                        )
                        doRefresh()
                    }}
                >
                    Drop to upload files.
                </div>
            ) : null}
            {/* This should be just `{modal}`, however TypeScript incorrectly throws an error. */}
            {project && <Ide backendService={backendService} project={project} />}
            {modal && <>{modal}</>}
        </div>
    )
}

export default Dashboard
