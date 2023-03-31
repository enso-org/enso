/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as react from 'react'
import * as reactDom from 'react-dom'

import * as projectManagerModule from 'enso-content/src/project_manager'

import * as auth from '../../authentication/providers/auth'
import * as backend from '../service'
import * as fileInfo from '../../fileInfo'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as newtype from '../../newtype'
import * as platformModule from '../../platform'
import * as svg from '../../components/svg'
import * as uploadMultipleFiles from '../../uploadMultipleFiles'

import Label, * as label from './label'
import PermissionDisplay, * as permissionDisplay from './permissionDisplay'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import ProjectActionButton from './projectActionButton'
import Rows from './rows'

import ConfirmDeleteModal from './confirmDeleteModal'
import UploadFileModal from './uploadFileModal'

import DirectoryCreateForm from './directoryCreateForm'
import FileCreateForm from './fileCreateForm'
import ProjectCreateForm from './projectCreateForm'
import SecretCreateForm from './secretCreateForm'

// =============
// === Types ===
// =============

enum ColumnDisplayMode {
    all = 'all',
    compact = 'compact',
    docs = 'docs',
    settings = 'settings',
}

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

/** Values provided to modals. */
export interface ModalProps {
    backend: backend.Backend
    directoryId: backend.DirectoryId
    onSuccess: () => void
    close: () => void
}

/** Values provided to modals. */
export interface CreateFormProps {
    backend: backend.Backend
    directoryId: backend.DirectoryId
    onSuccess: () => void
    close: () => void
}

// =================
// === Constants ===
// =================

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

/** React components for every column except for the name column. */
const COLUMN_RENDERER: Record<
    Exclude<Column, Column.name>,
    (asset: backend.Asset) => JSX.Element
> = {
    [Column.lastModified]: () => <>aa</>,
    [Column.sharedWith]: asset => (
        <>
            {(asset.permissions ?? []).map(user => (
                <PermissionDisplay
                    key={user.user.organization_id}
                    permissions={PERMISSION[user.permission]}
                >
                    <img
                        className="rounded-full h-8"
                        src="https://faces-img.xcdn.link/image-lorem-face-4742.jpg"
                    />
                </PermissionDisplay>
            ))}
        </>
    ),
    [Column.docs]: () => <>aa</>,
    [Column.labels]: () => (
        <>
            <Label status={label.Status.warning}>outdated version</Label>
            <Label status={label.Status.severeWarning}>low resources</Label>
            <Label>do not change</Label>
        </>
    ),
    [Column.dataAccess]: () => (
        <>
            <PermissionDisplay permissions={{ type: permissionDisplay.Permission.admin }}>
                <div className="px-4 py-1">./user_data</div>
            </PermissionDisplay>
            <PermissionDisplay
                permissions={{
                    type: permissionDisplay.Permission.regular,
                    write: true,
                    read: true,
                    exec: true,
                    docsWrite: true,
                }}
            >
                <div className="px-4 py-1">this folder</div>
            </PermissionDisplay>
            <PermissionDisplay
                permissions={{
                    type: permissionDisplay.Permission.regular,
                    write: false,
                    read: false,
                    exec: false,
                    docsWrite: false,
                }}
            >
                <div className="px-4 py-1">no access</div>
            </PermissionDisplay>
        </>
    ),
    [Column.usagePlan]: () => <>aa</>,
    [Column.engine]: () => <>aa</>,
    [Column.ide]: () => <>aa</>,
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

function Dashboard(props: DashboardProps) {
    const { logger, platform } = props
    const { accessToken, organization } = auth.useFullUserSession()
    const backendService = backend.createBackend(accessToken, logger)

    const [refresh, doRefresh] = hooks.useRefresh()

    const [directoryId, setDirectoryId] = react.useState(rootDirectoryId(organization.id))
    const [directoryStack, setDirectoryStack] = react.useState<
        backend.Asset<backend.AssetType.directory>[]
    >([])
    const [columnDisplayMode, setColumnDisplayMode] = react.useState(ColumnDisplayMode.compact)

    const [projectAssets, setProjectAssets] = react.useState<
        backend.Asset<backend.AssetType.project>[]
    >([])
    const [directoryAssets, setDirectoryAssets] = react.useState<
        backend.Asset<backend.AssetType.directory>[]
    >([])
    const [secretAssets, setSecretAssets] = react.useState<
        backend.Asset<backend.AssetType.secret>[]
    >([])
    const [fileAssets, setFileAssets] = react.useState<backend.Asset<backend.AssetType.file>[]>([])

    const [project, setProject] = react.useState<backend.Project | null>(null)
    const [projectStates, setProjectStates] = react.useState<
        Record<backend.ProjectId, backend.ProjectState>
    >({})

    const [selectedAssets, setSelectedAssets] = react.useState<backend.Asset[]>([])
    const [visibleCreateForm, setVisibleCreateForm] = react.useState<backend.AssetType | null>(null)
    const [isFileBeingDragged, setIsFileBeingDragged] = react.useState(false)
    const [Modal, setModal] = react.useState<((props: ModalProps) => JSX.Element) | null>(null)
    const [contextMenu, setContextMenu] = react.useState<JSX.Element | null>(null)

    const directory = directoryStack[directoryStack.length - 1]
    const parentDirectory = directoryStack[directoryStack.length - 2]

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
            <div className="flex text-left items-center align-middle whitespace-nowrap">
                <ProjectActionButton
                    project={projectAsset}
                    state={projectStates[projectAsset.id] ?? backend.ProjectState.created}
                    openIde={async () => {
                        setProject(await backendService.getProjectDetails(projectAsset.id))
                    }}
                    onOpen={() => {
                        setProjectStates({
                            ...projectStates,
                            [projectAsset.id]: backend.ProjectState.opened,
                        })
                    }}
                    onOpenStart={() => {
                        setProjectStates({
                            ...projectStates,
                            [projectAsset.id]: backend.ProjectState.openInProgress,
                        })
                    }}
                    onClose={() => {
                        setProjectStates({
                            ...projectStates,
                            [projectAsset.id]: backend.ProjectState.closed,
                        })
                    }}
                />
                <span className="px-4">{projectAsset.title}</span>
            </div>
        ),
        [backend.AssetType.directory]: directoryAsset => (
            <div
                className="flex text-left items-center align-middle whitespace-nowrap"
                onDoubleClick={() => {
                    setDirectoryId(directoryAsset.id)
                    setDirectoryStack([...directoryStack, directoryAsset])
                }}
            >
                {svg.DIRECTORY_ICON} <span className="px-4">{directoryAsset.title}</span>
            </div>
        ),
        [backend.AssetType.secret]: secret => (
            <div className="flex text-left items-center align-middle whitespace-nowrap">
                {svg.SECRET_ICON} <span className="px-4">{secret.title}</span>
            </div>
        ),
        [backend.AssetType.file]: file => (
            <div className="flex text-left items-center align-middle whitespace-nowrap">
                {fileInfo.fileIcon(fileInfo.fileExtension(file.title))}{' '}
                <span className="px-4">{file.title}</span>
            </div>
        ),
    }

    function renderer<Type extends backend.AssetType>(column: Column, assetType: Type) {
        return column === Column.name
            ? // This is type-safe only if we pass enum literals as `assetType`.

              // eslint-disable-next-line no-restricted-syntax
              (nameRenderers[assetType] as (asset: backend.Asset<Type>) => JSX.Element)
            : COLUMN_RENDERER[column]
    }

    /** Heading element for every column. */
    function ColumnHeading(column: Column, assetType: backend.AssetType) {
        // This is a React component even though it doesn't contain JSX.
        // eslint-disable-next-line no-restricted-syntax
        const CreateForm = ASSET_TYPE_CREATE_FORM[assetType]
        return column === Column.name ? (
            <div className="inline-flex">
                {ASSET_TYPE_NAME[assetType]}{' '}
                {assetType === visibleCreateForm ? (
                    <div
                        className="relative font-medium mx-1"
                        onClick={event => {
                            event.stopPropagation()
                        }}
                    >
                        <div className="absolute z-10">
                            <CreateForm
                                backend={backendService}
                                directoryId={directoryId}
                                onSuccess={doRefresh}
                                close={() => {
                                    setVisibleCreateForm(null)
                                }}
                            />
                        </div>
                    </div>
                ) : (
                    <button
                        className="mx-1"
                        onClick={event => {
                            event.stopPropagation()
                            closeAllDialogs()
                            setVisibleCreateForm(assetType)
                        }}
                    >
                        {svg.ADD_ICON}
                    </button>
                )}
            </div>
        ) : (
            <>{COLUMN_NAME[column]}</>
        )
    }

    // FIXME[sb]: There is a race condition between the initial `directoryId`
    // and the `directoryId` saved in `localStorage`.
    react.useEffect(() => {
        void (async (): Promise<void> => {
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
            reactDom.unstable_batchedUpdates(() => {
                setProjectAssets(assets.filter(backend.assetIsType(backend.AssetType.project)))
                setDirectoryAssets(assets.filter(backend.assetIsType(backend.AssetType.directory)))
                setSecretAssets(assets.filter(backend.assetIsType(backend.AssetType.secret)))
                setFileAssets(assets.filter(backend.assetIsType(backend.AssetType.file)))
                setProjectStates({})
            })
        })()
    }, [accessToken, directoryId, refresh])

    react.useEffect(() => {
        function onBlur() {
            setIsFileBeingDragged(false)
        }

        window.addEventListener('blur', onBlur)

        return () => {
            window.removeEventListener('blur', onBlur)
        }
    }, [])

    function closeAllDialogs() {
        setModal(null)
        setVisibleCreateForm(null)
        setContextMenu(null)
    }

    function onClick() {
        closeAllDialogs()
    }

    function onKeyDown(event: react.KeyboardEvent<HTMLDivElement>) {
        if (
            event.key === 'Escape' &&
            !event.ctrlKey &&
            !event.shiftKey &&
            !event.altKey &&
            !event.metaKey
        ) {
            if (visibleCreateForm || Modal || contextMenu) {
                event.preventDefault()
            }
            closeAllDialogs()
        }
    }

    function onDragEnter(event: react.DragEvent<HTMLDivElement>) {
        if (event.dataTransfer.types.includes('Files')) {
            setIsFileBeingDragged(true)
        }
    }

    return (
        <div
            className="select-none text-primary"
            onClick={onClick}
            onKeyDown={onKeyDown}
            onDragEnter={onDragEnter}
            onContextMenu={event => {
                event.preventDefault()
                event.stopPropagation()
                setContextMenu(
                    <ContextMenu event={event}>
                        <ContextMenuEntry
                            disabled
                            onClick={async () => {
                                try {
                                    const items = await navigator.clipboard.read()
                                    const fileIds: string[] = []
                                    for (const item of items) {
                                        for (const type of item.types) {
                                            if (type === 'application/enso-file-id') {
                                                const blob = await item.getType(type)
                                                fileIds.push(await blob.text())
                                                break
                                            }
                                        }
                                    }
                                    // TODO: Re-parent `fileIds` to `directoryId` when endpoint is ready.
                                } catch {
                                    // ignored
                                }
                            }}
                        >
                            Paste
                        </ContextMenuEntry>
                    </ContextMenu>
                )
            }}
        >
            {/* These are placeholders. When implementing a feature,
             * please replace the appropriate placeholder with the actual element.*/}
            <div id="header" />
            <div id="templates" />
            <div className="flex flex-row flex-nowrap">
                <h1 className="text-xl font-bold mx-6 self-center">Drive</h1>
                <div className="flex flex-row flex-nowrap mx-2">
                    <div className="bg-gray-100 rounded-l-full flex flex-row flex-nowrap items-center p-2 mx-0.5">
                        {directory ? (
                            <>
                                <button
                                    className="mx-2"
                                    onClick={() => {
                                        setDirectoryId(
                                            parentDirectory?.id ?? rootDirectoryId(organization.id)
                                        )
                                        setDirectoryStack(
                                            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                                            directoryStack.slice(0, -1)
                                        )
                                    }}
                                >
                                    {parentDirectory?.title ?? '~'}
                                </button>
                                {svg.SMALL_RIGHT_ARROW_ICON}
                            </>
                        ) : (
                            <></>
                        )}
                        <span className="mx-2">{directory?.title ?? '~'}</span>
                    </div>
                    <div className="bg-gray-100 rounded-r-full flex flex-row flex-nowrap items-center p-2 mx-0.5">
                        <span className="mx-2">Shared with</span>
                    </div>
                </div>
                <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap p-2 mx-4">
                    <button
                        className="mx-1"
                        onClick={event => {
                            event.stopPropagation()
                            closeAllDialogs()
                            setModal(() => UploadFileModal)
                        }}
                    >
                        {svg.UPLOAD_ICON}
                    </button>
                    <button
                        className={`${selectedAssets.length === 0 ? 'opacity-50' : ''} mx-1`}
                        disabled={selectedAssets.length === 0}
                        onClick={event => {
                            event.stopPropagation()
                            /* TODO */
                        }}
                    >
                        {svg.DOWNLOAD_ICON}
                    </button>
                </div>
                <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap p-1.5">
                    <button
                        className={`${
                            columnDisplayMode === ColumnDisplayMode.all
                                ? 'bg-white shadow-soft'
                                : 'opacity-50'
                        } rounded-full px-2`}
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
                        } rounded-full px-2`}
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
                        } rounded-full px-2`}
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
                        } rounded-full px-2`}
                        onClick={() => {
                            setColumnDisplayMode(ColumnDisplayMode.settings)
                        }}
                    >
                        Settings
                    </button>
                </div>
            </div>
            <table className="items-center w-full bg-transparent border-collapse">
                <tbody>
                    <tr className="h-8" />
                    <Rows<backend.Asset<backend.AssetType.project>>
                        items={projectAssets}
                        getKey={proj => proj.id}
                        placeholder={
                            <>
                                You have no project yet. Go ahead and create one using the form
                                above.
                            </>
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
                            setContextMenu(
                                <ContextMenu event={event}>
                                    <ContextMenuEntry
                                        onClick={() => {
                                            setModal(() => () => (
                                                // FIXME[sb]: These need some way to close.
                                                <ConfirmDeleteModal
                                                    name={projectAsset.title}
                                                    assetType={projectAsset.type}
                                                    doDelete={() =>
                                                        backendService.deleteProject(
                                                            projectAsset.id
                                                        )
                                                    }
                                                />
                                            ))
                                        }}
                                    >
                                        <span className="text-red-700">Delete</span>
                                    </ContextMenuEntry>
                                </ContextMenu>
                            )
                        }}
                    />
                    <tr className="h-8" />
                    <Rows<backend.Asset<backend.AssetType.directory>>
                        items={directoryAssets}
                        getKey={dir => dir.id}
                        placeholder={<>This directory does not contain any subdirectories.</>}
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
                            setContextMenu(<ContextMenu event={event}></ContextMenu>)
                        }}
                    />
                    <tr className="h-8" />
                    <Rows<backend.Asset<backend.AssetType.secret>>
                        items={secretAssets}
                        getKey={secret => secret.id}
                        placeholder={<>This directory does not contain any secrets.</>}
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
                            setContextMenu(
                                <ContextMenu event={event}>
                                    <ContextMenuEntry
                                        onClick={() => {
                                            setModal(() => () => (
                                                <ConfirmDeleteModal
                                                    name={secret.title}
                                                    assetType={secret.type}
                                                    doDelete={() =>
                                                        backendService.deleteSecret(secret.id)
                                                    }
                                                />
                                            ))
                                        }}
                                    >
                                        <span className="text-red-700">Delete</span>
                                    </ContextMenuEntry>
                                </ContextMenu>
                            )
                        }}
                    />
                    <tr className="h-8" />
                    <Rows<backend.Asset<backend.AssetType.file>>
                        items={fileAssets}
                        getKey={file => file.id}
                        placeholder={<>This directory does not contain any files.</>}
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
                            setContextMenu(
                                <ContextMenu event={event}>
                                    <ContextMenuEntry
                                        disabled
                                        onClick={() => {
                                            /** TODO: Call endpoint for downloading file. */
                                        }}
                                    >
                                        Copy
                                    </ContextMenuEntry>
                                    <ContextMenuEntry
                                        disabled
                                        onClick={() => {
                                            /** TODO: Call endpoint for downloading file. */
                                        }}
                                    >
                                        Cut
                                    </ContextMenuEntry>
                                    <ContextMenuEntry
                                        onClick={() => {
                                            setModal(() => () => (
                                                <ConfirmDeleteModal
                                                    name={file.title}
                                                    assetType={file.type}
                                                    doDelete={() =>
                                                        backendService.deleteFile(file.id)
                                                    }
                                                />
                                            ))
                                        }}
                                    >
                                        <span className="text-red-700">Delete</span>
                                    </ContextMenuEntry>
                                    <ContextMenuEntry
                                        disabled
                                        onClick={() => {
                                            /** TODO: Call endpoint for downloading file. */
                                        }}
                                    >
                                        Download
                                    </ContextMenuEntry>
                                </ContextMenu>
                            )
                        }}
                    />
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
                    onDrop={event => {
                        event.preventDefault()
                        setIsFileBeingDragged(false)
                        uploadMultipleFiles.uploadMultipleFiles(
                            backendService,
                            directoryId,
                            Array.from(event.dataTransfer.files)
                        )
                    }}
                >
                    Drop to upload files.
                </div>
            ) : (
                <></>
            )}
            {contextMenu}
            {Modal ? (
                <div
                    className="fixed w-screen h-screen inset-0 bg-primary grid place-items-center"
                    onClick={() => {
                        setModal(null)
                    }}
                >
                    <Modal
                        backend={backendService}
                        directoryId={directoryId}
                        onSuccess={doRefresh}
                        close={() => {
                            setModal(null)
                        }}
                    />
                </div>
            ) : (
                <></>
            )}
        </div>
    )
}

export default Dashboard
