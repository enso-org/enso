/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as react from 'react'
import toast from 'react-hot-toast'

import ArrowRightSmallIcon from 'enso-assets/arrow_right_small.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'
import DirectoryIcon from 'enso-assets/directory.svg'
import DownloadIcon from 'enso-assets/download.svg'
import PlusIcon from 'enso-assets/plus.svg'
import SecretIcon from 'enso-assets/secret.svg'
import UploadIcon from 'enso-assets/upload.svg'

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
import * as uploadMultipleFiles from '../../uploadMultipleFiles'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import PermissionDisplay, * as permissionDisplay from './permissionDisplay'
import ProjectActionButton, * as projectActionButton from './projectActionButton'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import Ide from './ide'
import Rows from './rows'
import Templates from './templates'
import TopBar from './topBar'

import ConfirmDeleteModal from './confirmDeleteModal'
import RenameModal from './renameModal'

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
    getNewProjectName: (templateId: string | null) => string
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
const DIRECTORY_STACK_KEY = `${common.PRODUCT_NAME.toLowerCase()}-dashboard-directory-stack`
/** The {@link RegExp} matching a directory name following the default naming convention. */
const DIRECTORY_NAME_REGEX = /^New_Directory_(?<directoryIndex>\d+)$/
/** The default prefix of an automatically generated directory. */
const DIRECTORY_NAME_DEFAULT_PREFIX = 'New_Directory_'

/** English names for the name column. */
const ASSET_TYPE_NAME: Record<backendModule.AssetType, string> = {
    [backendModule.AssetType.project]: 'Projects',
    [backendModule.AssetType.file]: 'Files',
    [backendModule.AssetType.secret]: 'Secrets',
    [backendModule.AssetType.directory]: 'Folders',
} as const

/** Forms to create each asset type. */
const ASSET_TYPE_CREATE_FORM: Record<
    Exclude<
        backendModule.AssetType,
        backendModule.AssetType.directory | backendModule.AssetType.project
    >,
    (props: CreateFormProps) => JSX.Element
> = {
    [backendModule.AssetType.file]: FileCreateForm,
    [backendModule.AssetType.secret]: SecretCreateForm,
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
    [ColumnDisplayMode.release]: [Column.name, Column.lastModified /*, Column.sharedWith*/],
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

/** Returns a new array, in which the given asset is replaced with a new asset with the new name. */
function arrayWithAssetTitleChanged<T extends backendModule.AssetType>(
    assets: backendModule.Asset<T>[],
    asset: backendModule.Asset<T>,
    newTitle: string
) {
    return assets.map(item => (item === asset ? { ...asset, title: newTitle } : item))
}

/** Returns a new array, in which the given asset is omitted. */
function arrayWithAssetOmitted<T extends backendModule.AssetType>(
    assets: backendModule.Asset<T>[],
    asset: backendModule.Asset<T>
) {
    return assets.filter(item => item !== asset)
}

/** Sanitizes a string for use as a regex. */
function regexEscape(string: string) {
    return string.replace(/[\\^$.|?*+()[{]/g, '\\$&')
}

// =================
// === Dashboard ===
// =================

/** Props for {@link Dashboard}s that are common to all platforms. */
export interface DashboardProps {
    /** Whether the application may have the local backend running. */
    supportsLocalBackend: boolean
    appRunner: AppRunner
    initialProjectName: string | null
}

// TODO[sb]: Implement rename when clicking name of a selected row.
// There is currently no way to tell whether a row is selected from a column.

/** The component that contains the entire UI. */
function Dashboard(props: DashboardProps) {
    const { supportsLocalBackend, appRunner, initialProjectName } = props

    const logger = loggerProvider.useLogger()
    const session = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setBackend } = backendProvider.useSetBackend()
    const { modal } = modalProvider.useModal()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const [refresh, doRefresh] = hooks.useRefresh()

    const [onDirectoryNextLoaded, setOnDirectoryNextLoaded] = react.useState<
        ((assets: backendModule.Asset[]) => void) | null
    >(() =>
        initialProjectName != null
            ? (assets: backendModule.Asset[]) => {
                  if (
                      !assets.some(
                          asset =>
                              asset.type === backendModule.AssetType.project &&
                              asset.title === initialProjectName
                      )
                  ) {
                      const errorMessage = `No project named '${initialProjectName}' was found.`
                      toast.error(errorMessage)
                      logger.error(`Error opening project on startup: ${errorMessage}`)
                  }
              }
            : null
    )
    const [nameOfProjectToImmediatelyOpen, setNameOfProjectToImmediatelyOpen] =
        react.useState(initialProjectName)
    const [projectEvent, setProjectEvent] = react.useState<projectActionButton.ProjectEvent | null>(
        null
    )
    const [query, setQuery] = react.useState('')
    const [loadingProjectManagerDidFail, setLoadingProjectManagerDidFail] = react.useState(false)
    const [directoryId, setDirectoryId] = react.useState(
        session.organization != null ? rootDirectoryId(session.organization.id) : null
    )
    const [directoryStack, setDirectoryStack] = react.useState<
        backendModule.Asset<backendModule.AssetType.directory>[]
    >([])
    // Defined by the spec as `compact` by default, however it is not ready yet.
    const [columnDisplayMode, setColumnDisplayMode] = react.useState(ColumnDisplayMode.release)
    const [tab, setTab] = react.useState(Tab.dashboard)
    const [project, setProject] = react.useState<backendModule.Project | null>(null)
    const [selectedAssets, setSelectedAssets] = react.useState<backendModule.Asset[]>([])
    const [isFileBeingDragged, setIsFileBeingDragged] = react.useState(false)
    const [isScrollBarVisible, setIsScrollBarVisible] = react.useState(false)

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
    const [projectDatas, setProjectDatas] = react.useState<
        Record<backendModule.ProjectId, projectActionButton.ProjectData>
    >({})

    const isListingLocalDirectoryAndWillFail =
        backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail
    const isListingRemoteDirectoryAndWillFail =
        backend.type === backendModule.BackendType.remote && !session.organization?.isEnabled
    const isListingRemoteDirectoryWhileOffline =
        session.type === authProvider.UserSessionType.offline &&
        backend.type === backendModule.BackendType.remote
    const directory = directoryStack[directoryStack.length - 1]
    const parentDirectory = directoryStack[directoryStack.length - 2]

    const switchToIdeTab = react.useCallback(() => {
        setTab(Tab.ide)
        const ideElement = document.getElementById(IDE_ELEMENT_ID)
        if (ideElement) {
            ideElement.style.top = ''
            ideElement.style.display = 'absolute'
        }
    }, [])

    const switchToDashboardTab = react.useCallback(() => {
        setTab(Tab.dashboard)
        doRefresh()
        const ideElement = document.getElementById(IDE_ELEMENT_ID)
        if (ideElement) {
            ideElement.style.top = '-100vh'
            ideElement.style.display = 'fixed'
        }
    }, [])

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
        if (
            supportsLocalBackend &&
            localStorage.getItem(backendProvider.BACKEND_TYPE_KEY) !==
                backendModule.BackendType.remote
        ) {
            setBackend(new localBackend.LocalBackend())
        }
    }, [])

    react.useEffect(() => {
        document.addEventListener('show-dashboard', switchToDashboardTab)
        return () => {
            document.removeEventListener('show-dashboard', switchToDashboardTab)
        }
    }, [])

    react.useEffect(() => {
        if (projectEvent != null) {
            setProjectEvent(null)
        }
    }, [projectEvent])

    const openIde = async (projectId: backendModule.ProjectId) => {
        switchToIdeTab()
        if (project?.projectId !== projectId) {
            setProject(await backend.getProjectDetails(projectId))
        }
    }

    const closeIde = () => {
        setProject(null)
    }

    const setBackendType = (newBackendType: backendModule.BackendType) => {
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
                    headers.append('Authorization', `Bearer ${session.accessToken ?? ''}`)
                    const client = new http.Client(headers)
                    setBackend(new remoteBackendModule.RemoteBackend(client, logger))
                    break
                }
            }
        }
    }

    const setProjectAssets = (
        newProjectAssets: backendModule.Asset<backendModule.AssetType.project>[]
    ) => {
        setProjectAssetsRaw(newProjectAssets)
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleProjectAssets(newProjectAssets.filter(asset => queryRegex.test(asset.title)))
    }
    const setDirectoryAssets = (
        newDirectoryAssets: backendModule.Asset<backendModule.AssetType.directory>[]
    ) => {
        setDirectoryAssetsRaw(newDirectoryAssets)
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleDirectoryAssets(newDirectoryAssets.filter(asset => queryRegex.test(asset.title)))
    }
    const setSecretAssets = (
        newSecretAssets: backendModule.Asset<backendModule.AssetType.secret>[]
    ) => {
        setSecretAssetsRaw(newSecretAssets)
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleSecretAssets(newSecretAssets.filter(asset => queryRegex.test(asset.title)))
    }
    const setFileAssets = (newFileAssets: backendModule.Asset<backendModule.AssetType.file>[]) => {
        setFileAssetsRaw(newFileAssets)
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleFileAssets(newFileAssets.filter(asset => queryRegex.test(asset.title)))
    }

    const exitDirectory = () => {
        setDirectoryId(
            parentDirectory?.id ??
                (session.organization != null ? rootDirectoryId(session.organization.id) : null)
        )
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
        if (
            session.organization == null ||
            directoryId === rootDirectoryId(session.organization.id)
        ) {
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
                    if (event.detail === 2) {
                        // It is a double click; open the project.
                        setProjectEvent({
                            type: projectActionButton.ProjectEventType.open,
                            projectId: projectAsset.id,
                        })
                    } else if (
                        event.ctrlKey &&
                        !event.altKey &&
                        !event.shiftKey &&
                        !event.metaKey
                    ) {
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
                                doRename={async newName => {
                                    setProjectAssets(
                                        arrayWithAssetTitleChanged(
                                            projectAssets,
                                            projectAsset,
                                            newName
                                        )
                                    )
                                    await backend.projectUpdate(projectAsset.id, {
                                        ami: null,
                                        ideVersion: null,
                                        projectName: newName,
                                    })
                                }}
                                onComplete={doRefresh}
                            />
                        ))
                    }
                }}
            >
                <ProjectActionButton
                    project={projectAsset}
                    projectData={
                        projectDatas[projectAsset.id] ?? projectActionButton.DEFAULT_PROJECT_DATA
                    }
                    setProjectData={newProjectData => {
                        if (typeof newProjectData === 'function') {
                            setProjectDatas(oldProjectDatas => ({
                                ...oldProjectDatas,
                                [projectAsset.id]: newProjectData(
                                    oldProjectDatas[projectAsset.id] ??
                                        projectActionButton.DEFAULT_PROJECT_DATA
                                ),
                            }))
                        } else {
                            setProjectDatas({
                                ...projectDatas,
                                [projectAsset.id]: newProjectData,
                            })
                        }
                    }}
                    appRunner={appRunner}
                    event={projectEvent}
                    doRefresh={doRefresh}
                    doOpenManually={() => {
                        setProjectEvent({
                            type: projectActionButton.ProjectEventType.open,
                            projectId: projectAsset.id,
                        })
                    }}
                    onClose={() => {
                        setProjectEvent({
                            type: projectActionButton.ProjectEventType.cancelOpeningAll,
                        })
                        closeIde()
                    }}
                    openIde={() => openIde(projectAsset.id)}
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
                                doRename={newName => {
                                    setDirectoryAssets(
                                        arrayWithAssetTitleChanged(
                                            directoryAssets,
                                            directoryAsset,
                                            newName
                                        )
                                    )
                                    return Promise.reject(
                                        'The backend endpoint does not exist yet.'
                                    )
                                }}
                                onComplete={doRefresh}
                            />
                        ))
                    }
                }}
                onDoubleClick={() => {
                    enterDirectory(directoryAsset)
                }}
            >
                <img src={DirectoryIcon} /> <span className="px-2">{directoryAsset.title}</span>
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
                                doRename={newName => {
                                    setSecretAssets(
                                        arrayWithAssetTitleChanged(secretAssets, secret, newName)
                                    )
                                    return Promise.reject(
                                        'The backend endpoint does not exist yet.'
                                    )
                                }}
                                onComplete={doRefresh}
                            />
                        ))
                    }
                }}
            >
                <img src={SecretIcon} /> <span className="px-2">{secret.title}</span>
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
                                doRename={newName => {
                                    setFileAssets(
                                        arrayWithAssetTitleChanged(fileAssets, file, newName)
                                    )
                                    return Promise.reject(
                                        'The backend endpoint does not exist yet.'
                                    )
                                }}
                                onComplete={doRefresh}
                            />
                        ))
                    }
                }}
            >
                <img src={fileInfo.fileIcon()} /> <span className="px-2">{file.title}</span>
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
                        <img src={DefaultUserIcon} height={24} width={24} />
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
                    // This is a placeholder key. It should be replaced with label ID when labels
                    // are implemented.
                    <ContextMenu key={'label'} event={event}>
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

    const uploadFilesFromInput = async (event: React.FormEvent<HTMLInputElement>) => {
        if (backend.type === backendModule.BackendType.local) {
            // TODO[sb]: Allow uploading `.enso-project`s
            // https://github.com/enso-org/cloud-v2/issues/510
            toast.error('Cannot upload files to the local backend.')
        } else if (event.currentTarget.files == null || event.currentTarget.files.length === 0) {
            toast.success('No files selected to upload.')
        } else if (directoryId == null) {
            // This should never happen, however display a nice
            // error message in case it somehow does.
            toast.error('You cannot upload files while offline.')
        } else {
            await uploadMultipleFiles.uploadMultipleFiles(
                backend,
                directoryId,
                Array.from(event.currentTarget.files)
            )
            doRefresh()
        }
    }

    /** Heading element for every column. */
    const ColumnHeading = (column: Column, assetType: backendModule.AssetType) =>
        column === Column.name ? (
            assetType === backendModule.AssetType.file ? (
                <div className="inline-flex">
                    {ASSET_TYPE_NAME[assetType]}
                    <input
                        type="file"
                        id="files_table_upload_files_input"
                        name="files_table_upload_files_input"
                        multiple
                        className="w-0 h-0"
                        onInput={uploadFilesFromInput}
                    />
                    <label htmlFor="files_table_upload_files_input" className="cursor-pointer mx-1">
                        <img src={PlusIcon} />
                    </label>
                </div>
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
                            if (assetType === backendModule.AssetType.project) {
                                void toast.promise(handleCreateProject(null), {
                                    loading: 'Creating new empty project...',
                                    success: 'Created new empty project.',
                                    // This is UNSAFE, as the original function's parameter is of type
                                    // `any`.
                                    error: (promiseError: Error) =>
                                        `Error creating new empty project: ${promiseError.message}`,
                                })
                            } else if (assetType === backendModule.AssetType.directory) {
                                void toast.promise(handleCreateDirectory(), {
                                    loading: 'Creating new directory...',
                                    success: 'Created new directory.',
                                    // This is UNSAFE, as the original function's parameter is of type
                                    // `any`.
                                    error: (promiseError: Error) =>
                                        `Error creating new directory: ${promiseError.message}`,
                                })
                            } else {
                                // This is a React component even though it doesn't contain JSX.
                                // eslint-disable-next-line no-restricted-syntax
                                const CreateForm = ASSET_TYPE_CREATE_FORM[assetType]
                                setModal(() => (
                                    <CreateForm
                                        left={buttonPosition.left + window.scrollX}
                                        top={buttonPosition.top + window.scrollY}
                                        getNewProjectName={getNewProjectName}
                                        // This is safe; headings are not rendered when there is no
                                        // internet connection.
                                        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                                        directoryId={directoryId!}
                                        onSuccess={doRefresh}
                                    />
                                ))
                            }
                        }}
                    >
                        <img src={PlusIcon} />
                    </button>
                </div>
            )
        ) : (
            <>{COLUMN_NAME[column]}</>
        )

    react.useEffect(() => {
        const queryRegex = new RegExp(regexEscape(query), 'i')
        const doesItMatchQuery = (asset: backendModule.Asset) => queryRegex.test(asset.title)
        setVisibleProjectAssets(projectAssets.filter(doesItMatchQuery))
        setVisibleDirectoryAssets(directoryAssets.filter(doesItMatchQuery))
        setVisibleSecretAssets(secretAssets.filter(doesItMatchQuery))
        setVisibleFileAssets(fileAssets.filter(doesItMatchQuery))
    }, [query])

    react.useLayoutEffect(() => {
        if (isLoadingAssets) {
            document.body.style.overflowY = isScrollBarVisible ? 'scroll' : ''
        } else {
            document.body.style.overflowY = ''
            setIsScrollBarVisible(document.body.scrollHeight > document.body.clientHeight)
        }
    }, [isLoadingAssets, projectAssets, directoryAssets, secretAssets, fileAssets])

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
        if (nameOfProjectToImmediatelyOpen != null) {
            const projectToLoad = newProjectAssets.find(
                projectAsset => projectAsset.title === nameOfProjectToImmediatelyOpen
            )
            if (projectToLoad != null) {
                setProjectEvent({
                    type: projectActionButton.ProjectEventType.open,
                    projectId: projectToLoad.id,
                })
            }
            setNameOfProjectToImmediatelyOpen(null)
        }
        onDirectoryNextLoaded?.(assets)
        setOnDirectoryNextLoaded(null)
    }

    hooks.useAsyncEffect(
        null,
        async signal => {
            switch (backend.type) {
                case backendModule.BackendType.local: {
                    if (!isListingLocalDirectoryAndWillFail) {
                        const assets = await backend.listDirectory()
                        if (!signal.aborted) {
                            setIsLoadingAssets(false)
                            setAssets(assets)
                        }
                    }
                    return
                }
                case backendModule.BackendType.remote: {
                    if (
                        !isListingRemoteDirectoryAndWillFail &&
                        !isListingRemoteDirectoryWhileOffline &&
                        directoryId != null
                    ) {
                        const assets = await backend.listDirectory({ parentId: directoryId })
                        if (!signal.aborted) {
                            setIsLoadingAssets(false)
                            setAssets(assets)
                        }
                    } else {
                        setIsLoadingAssets(false)
                    }
                    return
                }
            }
        },
        [session.accessToken, directoryId, refresh, backend]
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
            const projectIndex = projectNameTemplate.exec(projectAsset.title)?.groups?.projectIndex
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
        const templateText = templateId != null ? ` from template '${templateId}'` : ''
        setProjectAssets([
            {
                type: backendModule.AssetType.project,
                title: projectName,
                // The ID must be unique in order to be updated correctly in the UI.
                id: newtype.asNewtype<backendModule.ProjectId>(String(Number(new Date()))),
                modifiedAt: dateTime.toRfc3339(new Date()),
                // Falling back to the empty string is okay as this is what the local backend does.
                parentId: directoryId ?? newtype.asNewtype<backendModule.AssetId>(''),
                permissions: [],
                projectState: { type: backendModule.ProjectState.new },
            },
            ...projectAssets,
        ])
        await toast.promise(backend.createProject(body), {
            loading: `Creating project '${projectName}'${templateText}...`,
            success: `Created project '${projectName}'${templateText}.`,
            // This is UNSAFE, as the original function's parameter is of type `any`.
            error: (promiseError: Error) =>
                `Error creating '${projectName}'${templateText}: ${promiseError.message}`,
        })
        // `newProject.projectId` cannot be used directly in a `ProjectEvet` as the project
        // does not yet exist in the project list. Opening the project would work, but the project
        // would display as closed as it would be created after the event is sent.
        setNameOfProjectToImmediatelyOpen(projectName)
        doRefresh()
    }

    const handleCreateDirectory = async () => {
        if (backend.type !== backendModule.BackendType.remote) {
            // This should never happen, but even if it does, it is the caller's responsibility
            // to log, or display this error.
            throw new Error('Folders cannot be created on the local backend.')
        } else {
            const directoryIndices = directoryAssets
                .map(directoryAsset => DIRECTORY_NAME_REGEX.exec(directoryAsset.title))
                .map(match => match?.groups?.directoryIndex)
                .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
            const title = `${DIRECTORY_NAME_DEFAULT_PREFIX}${Math.max(0, ...directoryIndices) + 1}`
            setDirectoryAssets([
                {
                    title,
                    type: backendModule.AssetType.directory,
                    id: newtype.asNewtype<backendModule.DirectoryId>(Number(new Date()).toString()),
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: directoryId ?? newtype.asNewtype<backendModule.DirectoryId>(''),
                    permissions: [],
                    projectState: null,
                },
                ...directoryAssets,
            ])
            await backend.createDirectory({
                parentId: directoryId,
                title,
            })
            doRefresh()
            return
        }
    }

    return (
        <div
            className={`flex flex-col relative select-none text-primary text-xs min-h-screen p-2 ${
                tab === Tab.dashboard ? '' : 'hidden'
            }`}
            onClick={event => {
                if (getSelection()?.type !== 'Range') {
                    unsetModal()
                    if (!event.shiftKey) {
                        setSelectedAssets([])
                    }
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
                        switchToIdeTab()
                    } else {
                        switchToDashboardTab()
                    }
                }}
                setBackendType={setBackendType}
                query={query}
                setQuery={setQuery}
            />
            {isListingRemoteDirectoryWhileOffline ? (
                <div className="grow grid place-items-center">
                    <div className="text-base text-center">
                        You are offline. Please connect to the internet and refresh to access the
                        cloud backend.
                    </div>
                </div>
            ) : isListingLocalDirectoryAndWillFail ? (
                <div className="grow grid place-items-center">
                    <div className="text-base text-center">
                        Could not connect to the Project Manager. Please try restarting{' '}
                        {common.PRODUCT_NAME}, or manually launching the Project Manager.
                    </div>
                </div>
            ) : isListingRemoteDirectoryAndWillFail ? (
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
                                                <img src={ArrowRightSmallIcon} />
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
                                <input
                                    type="file"
                                    multiple
                                    disabled={backend.type === backendModule.BackendType.local}
                                    id="upload_files_input"
                                    name="upload_files_input"
                                    className="w-0 h-0"
                                    onInput={uploadFilesFromInput}
                                />
                                <label
                                    htmlFor="upload_files_input"
                                    className={`mx-1 ${
                                        backend.type === backendModule.BackendType.local
                                            ? 'opacity-50'
                                            : 'cursor-pointer'
                                    }`}
                                >
                                    <img src={UploadIcon} />
                                </label>
                                <button
                                    className={`mx-1 opacity-50`}
                                    disabled={true}
                                    onClick={event => {
                                        event.stopPropagation()
                                        unsetModal()
                                        /* TODO */
                                    }}
                                >
                                    <img src={DownloadIcon} />
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
                    <table className="table-fixed items-center border-collapse mt-2 w-0">
                        <tbody>
                            <tr className="h-10">
                                {columnsFor(columnDisplayMode, backend.type).map(column => (
                                    <td key={column} className={COLUMN_CSS_CLASS[column]} />
                                ))}
                            </tr>
                            <Rows<backendModule.Asset<backendModule.AssetType.project>>
                                items={visibleProjectAssets}
                                getKey={projectAsset => projectAsset.id}
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
                                        unsetModal()
                                        setProjectEvent({
                                            type: projectActionButton.ProjectEventType.open,
                                            projectId: projectAsset.id,
                                        })
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
                                                doRename={async newName => {
                                                    setProjectAssets(
                                                        arrayWithAssetTitleChanged(
                                                            projectAssets,
                                                            projectAsset,
                                                            newName
                                                        )
                                                    )
                                                    await backend.projectUpdate(projectAsset.id, {
                                                        ami: null,
                                                        ideVersion: null,
                                                        projectName: newName,
                                                    })
                                                }}
                                                onComplete={doRefresh}
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
                                                doDelete={() => {
                                                    setProjectAssets(
                                                        arrayWithAssetOmitted(
                                                            projectAssets,
                                                            projectAsset
                                                        )
                                                    )
                                                    return backend.deleteProject(projectAsset.id)
                                                }}
                                                onComplete={doRefresh}
                                            />
                                        ))
                                    }
                                    const isDisabled =
                                        backend.type === backendModule.BackendType.local &&
                                        (projectDatas[projectAsset.id]?.isRunning ?? false)
                                    setModal(() => (
                                        <ContextMenu key={projectAsset.id} event={event}>
                                            <ContextMenuEntry onClick={doOpenForEditing}>
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
                                            <ContextMenuEntry
                                                disabled={isDisabled}
                                                {...(isDisabled
                                                    ? {
                                                          title: 'A running local project cannot be removed.',
                                                      }
                                                    : {})}
                                                onClick={doDelete}
                                            >
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
                                            getKey={directoryAsset => directoryAsset.id}
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
                                            onContextMenu={(directoryAsset, event) => {
                                                event.preventDefault()
                                                event.stopPropagation()
                                                setModal(() => (
                                                    <ContextMenu
                                                        key={directoryAsset.id}
                                                        event={event}
                                                    ></ContextMenu>
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
                                                            doDelete={() => {
                                                                setSecretAssets(
                                                                    arrayWithAssetOmitted(
                                                                        secretAssets,
                                                                        secret
                                                                    )
                                                                )
                                                                return remoteBackend.deleteSecret(
                                                                    secret.id
                                                                )
                                                            }}
                                                            onComplete={doRefresh}
                                                        />
                                                    ))
                                                }
                                                setModal(() => (
                                                    <ContextMenu key={secret.id} event={event}>
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
                                                            doDelete={() => {
                                                                setFileAssets(
                                                                    arrayWithAssetOmitted(
                                                                        fileAssets,
                                                                        file
                                                                    )
                                                                )
                                                                return remoteBackend.deleteFile(
                                                                    file.id
                                                                )
                                                            }}
                                                            onComplete={doRefresh}
                                                        />
                                                    ))
                                                }
                                                const doDownload = () => {
                                                    /** TODO: Wait for backend endpoint. */
                                                }
                                                setModal(() => (
                                                    <ContextMenu key={file.id} event={event}>
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
