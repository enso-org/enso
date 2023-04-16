/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as react from 'react'
import * as reactDom from 'react-dom'

import * as projectManagerModule from 'enso-content/src/project_manager'

import * as auth from '../../authentication/providers/auth'
import * as backend from '../service'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as newtype from '../../newtype'
import * as platformModule from '../../platform'
import * as svg from '../../components/svg'

import Label, * as label from './label'
import PermissionDisplay, * as permissionDisplay from './permissionDisplay'
import Ide from './ide'
import ProjectActionButton from './projectActionButton'
import Rows from './rows'
import TopBar from './topBar'

// =============
// === Types ===
// =============

/** Main content of the screen. Only one should be visible at a time. */
export enum Tab {
    dashboard = 'dashboard',
    ide = 'ide',
}

enum ColumnDisplayMode {
    all = 'all',
    compact = 'compact',
    docs = 'docs',
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

// =================
// === Constants ===
// =================

/** English names for the name column. */
const ASSET_TYPE_NAME: Record<backend.AssetType, string> = {
    [backend.AssetType.project]: 'Projects',
    [backend.AssetType.file]: 'Files',
    [backend.AssetType.secret]: 'Secrets',
    [backend.AssetType.directory]: 'Folders',
} as const

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
    (project: backend.Asset) => JSX.Element
> = {
    [Column.lastModified]: () => <>aa</>,
    [Column.sharedWith]: () => <>aa</>,
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
                ./user_data
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
                this folder
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
                no access
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

/** English names for every column. */
function columnName(column: Column, assetType: backend.AssetType) {
    return column === Column.name ? ASSET_TYPE_NAME[assetType] : COLUMN_NAME[column]
}

/** Returns the id of the root directory for a user or organization. */
function rootDirectoryId(userOrOrganizationId: backend.UserOrOrganizationId) {
    return newtype.asNewtype<backend.DirectoryId>(
        userOrOrganizationId.replace(/^organization-/, `${backend.AssetType.directory}-`)
    )
}

/** Returns the file extension of a file name. */
function fileExtension(fileName: string) {
    return fileName.match(/\.(.+?)$/)?.[1] ?? ''
}

/** Returns the appropriate icon for a specific file extension. */
function fileIcon(_extension: string) {
    return svg.FILE_ICON
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

    const { modal } = modalProvider.useModal()
    const { unsetModal } = modalProvider.useSetModal()

    const [searchVal, setSearchVal] = react.useState('')
    const [directoryId, setDirectoryId] = react.useState(rootDirectoryId(organization.id))
    const [directoryStack, setDirectoryStack] = react.useState<
        backend.Asset<backend.AssetType.directory>[]
    >([])
    const [columnDisplayMode, setColumnDisplayMode] = react.useState(ColumnDisplayMode.compact)
    const [selectedAssets, setSelectedAssets] = react.useState<backend.Asset[]>([])

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

    const [tab, setTab] = react.useState(Tab.dashboard)
    const [project, setProject] = react.useState<backend.Project | null>(null)

    const directory = directoryStack[directoryStack.length - 1]
    const parentDirectory = directoryStack[directoryStack.length - 2]

    // The purpose of this effect is to enable search action.
    react.useEffect(() => {
        return () => {
            // TODO
        }
    }, [searchVal])

    /** React components for the name column. */
    const nameRenderers: {
        [Type in backend.AssetType]: (asset: backend.Asset<Type>) => JSX.Element
    } = {
        [backend.AssetType.project]: projectAsset => (
            <div className="flex text-left items-center align-middle whitespace-nowrap">
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
                onDoubleClick={() => {
                    setDirectoryId(directoryAsset.id)
                    setDirectoryStack([...directoryStack, directoryAsset])
                }}
            >
                {svg.DIRECTORY_ICON} <span className="px-2">{directoryAsset.title}</span>
            </div>
        ),
        [backend.AssetType.secret]: secret => (
            <div className="flex text-left items-center align-middle whitespace-nowrap">
                {svg.SECRET_ICON} <span className="px-2">{secret.title}</span>
            </div>
        ),
        [backend.AssetType.file]: file => (
            <div className="flex text-left items-center align-middle whitespace-nowrap">
                {fileIcon(fileExtension(file.title))} <span className="px-2">{file.title}</span>
            </div>
        ),
    }

    const renderer = <Type extends backend.AssetType>(column: Column, assetType: Type) => {
        return column === Column.name
            ? // This is type-safe only if we pass enum literals as `assetType`.
              // eslint-disable-next-line no-restricted-syntax
              (nameRenderers[assetType] as (asset: backend.Asset<Type>) => JSX.Element)
            : COLUMN_RENDERER[column]
    }

    // The purpose of this effect is to enable search action.
    react.useEffect(() => {
        return () => {
            // TODO
        }
    }, [searchVal])

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
                            permissions: [],
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
            })
        })()
    }, [accessToken, directoryId])

    return (
        <div className="text-primary text-xs" onClick={unsetModal}>
            <div className={tab === Tab.dashboard ? '' : 'hidden'}>
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
                    searchVal={searchVal}
                    setSearchVal={setSearchVal}
                />
                {/* This is a placeholder. When implementing a feature,
                 * please replace it with the actual element.*/}
                <div id="templates" />
                <div className="flex flex-row flex-nowrap">
                    <h1 className="text-xl font-bold mx-4 self-center">Drive</h1>
                    <div className="flex flex-row flex-nowrap mx-4">
                        <div className="bg-gray-100 rounded-l-full flex flex-row flex-nowrap items-center p-1 mx-0.5">
                            {/* FIXME[sb]: Remove `|| true` when UI to create directory is implemented. */}
                            {/* eslint-disable-next-line no-constant-condition, @typescript-eslint/no-unnecessary-condition */}
                            {directory || true ? (
                                <>
                                    <button
                                        className="mx-2"
                                        onClick={() => {
                                            setDirectoryId(
                                                parentDirectory?.id ??
                                                    rootDirectoryId(organization.id)
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
                            ) : null}
                            <span className="mx-2">{directory?.title ?? '~'}</span>
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
                        <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap p-1 mx-4">
                            <button
                                className="mx-1"
                                onClick={() => {
                                    /* TODO */
                                }}
                            >
                                {svg.UPLOAD_ICON}
                            </button>
                            <button
                                className={`mx-1 ${
                                    selectedAssets.length === 0 ? 'opacity-50' : ''
                                }`}
                                disabled={selectedAssets.length === 0}
                                onClick={() => {
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
                    </div>
                </div>
                <table className="items-center w-full bg-transparent border-collapse">
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
                            name: columnName(column, backend.AssetType.project),
                            render: renderer(column, backend.AssetType.project),
                        }))}
                    />
                    <tr className="h-8" />
                    <Rows<backend.Asset<backend.AssetType.directory>>
                        items={directoryAssets}
                        getKey={proj => proj.id}
                        placeholder={<>This directory does not contain any subdirectories.</>}
                        columns={COLUMNS_FOR[columnDisplayMode].map(column => ({
                            id: column,
                            name: columnName(column, backend.AssetType.directory),
                            render: renderer(column, backend.AssetType.directory),
                        }))}
                    />
                    <tr className="h-8" />
                    <Rows<backend.Asset<backend.AssetType.secret>>
                        items={secretAssets}
                        getKey={proj => proj.id}
                        placeholder={<>This directory does not contain any secrets.</>}
                        columns={COLUMNS_FOR[columnDisplayMode].map(column => ({
                            id: column,
                            name: columnName(column, backend.AssetType.secret),
                            render: renderer(column, backend.AssetType.secret),
                        }))}
                    />
                    <tr className="h-8" />
                    <Rows<backend.Asset<backend.AssetType.file>>
                        items={fileAssets}
                        getKey={proj => proj.id}
                        placeholder={<>This directory does not contain any files.</>}
                        columns={COLUMNS_FOR[columnDisplayMode].map(column => ({
                            id: column,
                            name: columnName(column, backend.AssetType.file),
                            render: renderer(column, backend.AssetType.file),
                        }))}
                    />
                </table>
            </div>
            <div className={tab === Tab.ide ? '' : 'hidden'}>
                {project ? <Ide backendService={backendService} project={project} /> : <></>}
            </div>
            {modal && <>{modal}</>}
        </div>
    )
}

export default Dashboard
