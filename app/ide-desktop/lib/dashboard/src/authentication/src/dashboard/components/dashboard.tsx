/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as common from 'enso-common'

import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as hooks from '../../hooks'
import * as http from '../../http'
import * as localBackend from '../localBackend'
import * as newtype from '../../newtype'
import * as platformModule from '../../platform'
import * as remoteBackendModule from '../remoteBackend'
import * as svg from '../../components/svg'
import * as uploadMultipleFiles from '../../uploadMultipleFiles'

import * as auth from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import Ide from './ide'
import Templates from './templates'
import TopBar from './topBar'

import UploadFileModal from './uploadFileModal'

import DirectoryRows from './directoryRows'
import FileRows from './fileRows'
import ProjectRows from './projectRows'
import SecretRows from './secretRows'
import ColumnDisplayModeSwitcher from './columnDisplayModeSwitcher'

// =============
// === Types ===
// =============

/** Main content of the screen. Only one should be visible at a time. */
export enum Tab {
    dashboard = 'dashboard',
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
    columnDisplayModeSwitcher: false,
}

/** The `id` attribute of the element into which the IDE will be rendered. */
const IDE_ELEMENT_ID = 'root'
/** The `localStorage` key under which the ID of the current directory is stored. */
const DIRECTORY_STACK_KEY = `${common.PRODUCT_NAME.toLowerCase()}-dashboard-directory-stack`

// ========================
// === Helper functions ===
// ========================

/** Returns the id of the root directory for a user or organization. */
function rootDirectoryId(userOrOrganizationId: backendModule.UserOrOrganizationId) {
    return newtype.asNewtype<backendModule.DirectoryId>(
        userOrOrganizationId.replace(/^organization-/, `${backendModule.AssetType.directory}-`)
    )
}

// =================
// === Dashboard ===
// =================

export interface DashboardProps {
    platform: platformModule.Platform
    appRunner: AppRunner | null
}

// TODO[sb]: Implement rename when clicking name of a selected row.
// There is currently no way to tell whether a row is selected from a column.

function Dashboard(props: DashboardProps) {
    const { platform, appRunner } = props

    const logger = loggerProvider.useLogger()
    const { accessToken, organization } = auth.useFullUserSession()
    const { backend } = backendProvider.useBackend()
    const { setBackend } = backendProvider.useSetBackend()
    const { modal } = modalProvider.useModal()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const [refresh, doRefresh] = hooks.useRefresh()

    const [query, setQuery] = React.useState('')
    const [directoryId, setDirectoryId] = React.useState(rootDirectoryId(organization.id))
    const [directoryStack, setDirectoryStack] = React.useState<backendModule.DirectoryAsset[]>([])
    // Defined by the spec as `compact` by default, however it is not ready yet.
    const [columnDisplayMode, setColumnDisplayMode] = React.useState(
        columnModule.ColumnDisplayMode.release
    )
    const [tab, setTab] = React.useState(Tab.dashboard)
    const [project, setProject] = React.useState<backendModule.Project | null>(null)
    const [selectedAssets, setSelectedAssets] = React.useState<backendModule.Asset[]>([])
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)

    const [projectAssets, setProjectAssets] = React.useState<backendModule.ProjectAsset[]>([])
    const [directoryAssets, setDirectoryAssets] = React.useState<backendModule.DirectoryAsset[]>([])
    const [secretAssets, setSecretAssets] = React.useState<backendModule.SecretAsset[]>([])
    const [fileAssets, setFileAssets] = React.useState<backendModule.FileAsset[]>([])
    const [visibleProjectAssets, setVisibleProjectAssets] = React.useState(projectAssets)
    const [visibleDirectoryAssets, setVisibleDirectoryAssets] = React.useState(directoryAssets)
    const [visibleSecretAssets, setVisibleSecretAssets] = React.useState(secretAssets)
    const [visibleFileAssets, setVisibleFileAssets] = React.useState(fileAssets)

    const directory = directoryStack[directoryStack.length - 1]
    const parentDirectory = directoryStack[directoryStack.length - 2]

    React.useEffect(() => {
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
        const onBlur = () => {
            setIsFileBeingDragged(false)
        }
        document.addEventListener('keydown', onKeyDown)
        window.addEventListener('blur', onBlur)
        return () => {
            document.removeEventListener('keydown', onKeyDown)
            window.removeEventListener('blur', onBlur)
        }
    }, [])

    React.useEffect(() => {
        const cachedDirectoryStackJson = localStorage.getItem(DIRECTORY_STACK_KEY)
        if (cachedDirectoryStackJson) {
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
    }, [])

    React.useEffect(() => {
        if (directoryId === rootDirectoryId(organization.id)) {
            localStorage.removeItem(DIRECTORY_STACK_KEY)
        } else {
            localStorage.setItem(DIRECTORY_STACK_KEY, JSON.stringify(directoryStack))
        }
    }, [directoryStack, directoryId, organization.id])

    React.useEffect(() => {
        setVisibleProjectAssets(projectAssets.filter(asset => asset.title.includes(query)))
    }, [query, projectAssets])

    React.useEffect(() => {
        setVisibleDirectoryAssets(directoryAssets.filter(asset => asset.title.includes(query)))
    }, [query, directoryAssets])

    React.useEffect(() => {
        setVisibleSecretAssets(secretAssets.filter(asset => asset.title.includes(query)))
    }, [query, secretAssets])

    React.useEffect(() => {
        setVisibleFileAssets(fileAssets.filter(asset => asset.title.includes(query)))
    }, [query, fileAssets])

    hooks.useAsyncEffect(
        null,
        async signal => {
            const assets = await backend.listDirectory({ parentId: directoryId })
            if (!signal.aborted) {
                const newProjectAssets = assets.filter(
                    backendModule.assetIsType(backendModule.AssetType.project)
                )
                const newDirectoryAssets = assets.filter(
                    backendModule.assetIsType(backendModule.AssetType.directory)
                )
                const newSecretAssets = assets.filter(
                    backendModule.assetIsType(backendModule.AssetType.secret)
                )
                const newFileAssets = assets.filter(
                    backendModule.assetIsType(backendModule.AssetType.file)
                )
                setProjectAssets(newProjectAssets)
                setDirectoryAssets(newDirectoryAssets)
                setSecretAssets(newSecretAssets)
                setFileAssets(newFileAssets)
            }
        },
        [accessToken, directoryId, refresh, backend]
    )

    const enterDirectory = (directoryAsset: backendModule.DirectoryAsset) => {
        setDirectoryId(directoryAsset.id)
        setDirectoryStack([...directoryStack, directoryAsset])
    }

    const exitDirectory = () => {
        setDirectoryId(parentDirectory?.id ?? rootDirectoryId(organization.id))
        setDirectoryStack(
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            directoryStack.slice(0, -1)
        )
    }

    const handleEscapeKey = (event: React.KeyboardEvent<HTMLDivElement>) => {
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

    const openDropZone = (event: React.DragEvent<HTMLDivElement>) => {
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
        const projectAsset = await backend.createProject(body)
        setProjectAssets([
            ...projectAssets,
            {
                type: backendModule.AssetType.project,
                title: projectAsset.name,
                id: projectAsset.projectId,
                parentId: '',
                permissions: [],
            },
        ])
    }

    const onAssetClick = (asset: backendModule.Asset, event: React.MouseEvent) => {
        event.stopPropagation()
        setSelectedAssets(event.shiftKey ? [...selectedAssets, asset] : [asset])
    }

    return (
        <div
            className={`relative select-none text-primary text-xs min-h-screen p-2 ${
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
                platform={platform}
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
                setBackendPlatform={newBackendPlatform => {
                    if (newBackendPlatform !== backend.platform) {
                        setProjectAssets([])
                        setDirectoryAssets([])
                        setSecretAssets([])
                        setFileAssets([])
                        switch (newBackendPlatform) {
                            case platformModule.Platform.desktop:
                                setBackend(new localBackend.LocalBackend())
                                break
                            case platformModule.Platform.cloud: {
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
            <Templates onTemplateClick={handleCreateProject} />
            <div className="flex flex-row flex-nowrap my-2">
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
                        <div></div>
                    </div>
                    <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap px-1.5 py-1 mx-4">
                        <button
                            className={`mx-1 ${
                                backend.platform === platformModule.Platform.desktop
                                    ? 'opacity-50'
                                    : ''
                            }`}
                            disabled={backend.platform === platformModule.Platform.desktop}
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
                                /* TODO */
                            }}
                        >
                            {svg.DOWNLOAD_ICON}
                        </button>
                    </div>
                    {EXPERIMENTAL.columnDisplayModeSwitcher && (
                        <ColumnDisplayModeSwitcher
                            columnDisplayMode={columnDisplayMode}
                            setColumnDisplayMode={setColumnDisplayMode}
                        />
                    )}
                </div>
            </div>
            <table className="items-center w-full bg-transparent border-collapse mt-2">
                <tbody>
                    <ProjectRows
                        appRunner={appRunner}
                        directoryId={directoryId}
                        items={visibleProjectAssets}
                        columnDisplayMode={columnDisplayMode}
                        onCreate={doRefresh}
                        onRename={doRefresh}
                        onDelete={doRefresh}
                        onAssetClick={onAssetClick}
                        onOpenIde={async projectAsset => {
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
                        onCloseIde={() => {
                            setProject(null)
                        }}
                    />
                    <DirectoryRows
                        directoryId={directoryId}
                        items={visibleDirectoryAssets}
                        columnDisplayMode={columnDisplayMode}
                        query={query}
                        onCreate={doRefresh}
                        onRename={doRefresh}
                        onAssetClick={onAssetClick}
                        enterDirectory={enterDirectory}
                    />
                    <SecretRows
                        directoryId={directoryId}
                        items={visibleSecretAssets}
                        columnDisplayMode={columnDisplayMode}
                        query={query}
                        onCreate={doRefresh}
                        onRename={doRefresh}
                        onDelete={doRefresh}
                        onAssetClick={onAssetClick}
                    />
                    <FileRows
                        directoryId={directoryId}
                        items={visibleFileAssets}
                        columnDisplayMode={columnDisplayMode}
                        query={query}
                        onCreate={doRefresh}
                        onRename={doRefresh}
                        onDelete={doRefresh}
                        onAssetClick={onAssetClick}
                    />
                </tbody>
            </table>
            {isFileBeingDragged && backend.platform === platformModule.Platform.cloud ? (
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
            {/* This should be just `{modal}`, however TypeScript incorrectly throws an error. */}
            {project && <Ide project={project} appRunner={appRunner} />}
            {modal && <>{modal}</>}
        </div>
    )
}

export default Dashboard
