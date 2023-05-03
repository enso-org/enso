/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as backendModule from '../backend'
import * as hooks from '../../hooks'
import * as http from '../../http'
import * as localBackend from '../localBackend'
import * as platformModule from '../../platform'
import * as remoteBackendModule from '../remoteBackend'
import * as uploadMultipleFiles from '../../uploadMultipleFiles'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import DirectoryView from './directoryView'
import Ide from './ide'
import Templates from './templates'
import TopBar from './topBar'

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
    const { accessToken, organization } = authProvider.useFullUserSession()
    const { backend } = backendProvider.useBackend()
    const { setBackend } = backendProvider.useSetBackend()
    const { modal } = modalProvider.useModal()
    const { unsetModal } = modalProvider.useSetModal()

    const [refresh, doRefresh] = hooks.useRefresh()

    const [directoryId, setDirectoryId] = React.useState(
        backendModule.rootDirectoryId(organization.id)
    )
    const [query, setQuery] = React.useState('')
    const [tab, setTab] = React.useState(Tab.dashboard)
    const [project, setProject] = React.useState<backendModule.Project | null>(null)
    const [selectedAssets, setSelectedAssets] = React.useState<backendModule.Asset[]>([])
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)

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

    const clearModalAndSelectedAssets = (event: React.MouseEvent) => {
        unsetModal()
        if (!event.shiftKey) {
            setSelectedAssets([])
        }
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

    const toggleTab = () => {
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
    }

    const setBackendPlatform = (newBackendPlatform: platformModule.Platform) => {
        if (newBackendPlatform !== backend.platform) {
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
    }

    const getNewProjectName = async (templateId?: string | null) => {
        const prefix = `${templateId ?? 'New_Project'}_`
        const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
        let highestProjectIndex = 0
        const projects = await backend.listProjects()
        for (const listedProject of projects) {
            const projectIndex = projectNameTemplate.exec(listedProject.name)?.groups?.projectIndex
            if (projectIndex) {
                highestProjectIndex = Math.max(highestProjectIndex, parseInt(projectIndex, 10))
            }
        }
        return `${prefix}${highestProjectIndex + 1}`
    }

    const handleCreateProject = async (templateId?: string | null) => {
        const projectName = await getNewProjectName(templateId)
        const body: backendModule.CreateProjectRequestBody = {
            projectName,
            projectTemplateName: templateId ?? null,
            parentDirectoryId: directoryId,
        }
        const projectAsset = await backend.createProject(body)
        doRefresh()
    }

    const onAssetClick = (asset: backendModule.Asset, event: React.MouseEvent) => {
        event.stopPropagation()
        setSelectedAssets(event.shiftKey ? [...selectedAssets, asset] : [asset])
    }

    const openIde = async (projectAsset: backendModule.ProjectAsset) => {
        setTab(Tab.ide)
        if (project?.projectId !== projectAsset.id) {
            setProject(await backend.getProjectDetails(projectAsset.id))
        }
        const ideElement = document.getElementById(IDE_ELEMENT_ID)
        if (ideElement) {
            ideElement.style.top = ''
            ideElement.style.display = 'absolute'
        }
    }

    const closeIde = () => {
        setProject(null)
    }

    return (
        <div
            className={`relative select-none text-primary text-xs min-h-screen p-2 ${
                tab === Tab.dashboard ? '' : 'hidden'
            }`}
            onClick={clearModalAndSelectedAssets}
            onKeyDown={handleEscapeKey}
            onDragEnter={openDropZone}
        >
            <TopBar
                platform={platform}
                projectName={project?.name ?? null}
                tab={tab}
                toggleTab={toggleTab}
                setBackendPlatform={setBackendPlatform}
                query={query}
                setQuery={setQuery}
            />
            <Templates onTemplateClick={handleCreateProject} />
            <DirectoryView
                directoryId={directoryId}
                setDirectoryId={setDirectoryId}
                query={query}
                refresh={refresh}
                doRefresh={doRefresh}
                onAssetClick={onAssetClick}
                onOpenIde={openIde}
                onCloseIde={closeIde}
                appRunner={appRunner}
                experimentalShowColumnDisplayModeSwitcher={EXPERIMENTAL.columnDisplayModeSwitcher}
            />
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
