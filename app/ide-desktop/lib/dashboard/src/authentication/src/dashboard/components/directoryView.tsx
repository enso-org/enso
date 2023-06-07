/** @file The directory header bar and directory item listing. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as common from 'enso-common'

import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as reactiveEvents from '../reactiveEvents'

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

/** A type guard that returns `true` if a given {@link backendModule.Asset} is a
 * {@link backendModule.ProjectAsset}. */
// This is a function, so its name should be camelCase.
// eslint-disable-next-line no-restricted-syntax
const isProjectAsset = backendModule.assetIsType(backendModule.AssetType.project)

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
    query: string
    refresh: hooks.RefreshState
    doRefresh: () => void
    doCreateBlankProject: () => Promise<void>
    onOpenIde: (project: backendModule.ProjectAsset) => void
    onCloseIde: () => void
    onAssetClick: (asset: backendModule.Asset, event: React.MouseEvent<HTMLTableRowElement>) => void
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
        doRefresh,
        doCreateBlankProject,
        onOpenIde,
        onCloseIde,
        onAssetClick,
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

    const [isLoadingAssets, setIsLoadingAssets] = React.useState(true)
    const [directoryStack, setDirectoryStack] = React.useState<backendModule.DirectoryAsset[]>([])
    // Defined by the spec as `compact` by default, however it is not ready yet.
    const [columnDisplayMode, setColumnDisplayMode] = React.useState(
        columnModule.ColumnDisplayMode.release
    )
    const [onDirectoryNextLoaded, setOnDirectoryNextLoaded] = React.useState<
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

    const [{ projectAssets, directoryAssets, secretAssets, fileAssets }, setAssets] =
        React.useReducer(
            (_: unknown, assets: backendModule.Asset[]) => {
                const assetIsType = backendModule.assetIsType
                return {
                    projectAssets: assets.filter(assetIsType(backendModule.AssetType.project)),
                    directoryAssets: assets.filter(assetIsType(backendModule.AssetType.directory)),
                    secretAssets: assets.filter(assetIsType(backendModule.AssetType.secret)),
                    fileAssets: assets.filter(assetIsType(backendModule.AssetType.file)),
                }
            },
            {
                projectAssets: [],
                directoryAssets: [],
                secretAssets: [],
                fileAssets: [],
            }
        )
    const [visibleProjectAssets, setVisibleProjectAssets] = React.useState(projectAssets)
    const [visibleDirectoryAssets, setVisibleDirectoryAssets] = React.useState(directoryAssets)
    const [visibleSecretAssets, setVisibleSecretAssets] = React.useState(secretAssets)
    const [visibleFileAssets, setVisibleFileAssets] = React.useState(fileAssets)
    const [projectEvent, setProjectEvent] = React.useState<reactiveEvents.ProjectEvent | null>(null)

    const directory = directoryStack[directoryStack.length - 1] ?? null
    const parentDirectory = directoryStack[directoryStack.length - 2] ?? null

    React.useEffect(() => {
        setIsLoadingAssets(true)
        setAssets([])
    }, [setAssets, backend, directoryId])

    React.useEffect(() => {
        if (backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail) {
            setIsLoadingAssets(false)
        }
    }, [isLoadingAssets, loadingProjectManagerDidFail, backend.type])

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
                            setAssets(newAssets)
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
                        const newAssets = await backend.listDirectory({ parentId: directoryId })
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
        setAssets(assets)
        if (nameOfProjectToImmediatelyOpen != null) {
            const projectToLoad = assets
                .filter(isProjectAsset)
                .find(projectAsset => projectAsset.title === nameOfProjectToImmediatelyOpen)
            if (projectToLoad != null) {
                setProjectEvent({
                    type: reactiveEvents.ProjectEventType.open,
                    projectId: projectToLoad.id,
                })
            }
            setNameOfProjectToImmediatelyOpen(null)
        }
        onDirectoryNextLoaded?.(assets)
        setOnDirectoryNextLoaded(null)
    }, [
        assets,
        nameOfProjectToImmediatelyOpen,
        setNameOfProjectToImmediatelyOpen,
        onDirectoryNextLoaded,
    ])

    React.useEffect(() => {
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleProjectAssets(projectAssets.filter(asset => queryRegex.test(asset.title)))
    }, [query, projectAssets])

    React.useEffect(() => {
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleDirectoryAssets(directoryAssets.filter(asset => queryRegex.test(asset.title)))
    }, [query, directoryAssets])

    React.useEffect(() => {
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleSecretAssets(secretAssets.filter(asset => queryRegex.test(asset.title)))
    }, [query, secretAssets])

    React.useEffect(() => {
        const queryRegex = new RegExp(regexEscape(query), 'i')
        setVisibleFileAssets(fileAssets.filter(asset => queryRegex.test(asset.title)))
    }, [query, fileAssets])

    const enterDirectory = (directoryAsset: backendModule.DirectoryAsset) => {
        setDirectoryId(directoryAsset.id)
        setDirectoryStack([...directoryStack, directoryAsset])
    }

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
                setProjectEvent={setProjectEvent}
                doCreateBlankProject={doCreateBlankProject}
                onCreate={doRefresh}
                onRename={doRefresh}
                onDelete={doRefresh}
                doOpenIde={onOpenIde}
                doCloseIde={onCloseIde}
                onAssetClick={onAssetClick}
                doRefresh={doRefresh}
            />
            <div className="h-10" />
            <DirectoriesTable
                directoryId={directoryId}
                items={visibleDirectoryAssets}
                isLoading={isLoadingAssets}
                columnDisplayMode={columnDisplayMode}
                query={query}
                onCreate={doRefresh}
                onRename={doRefresh}
                onAssetClick={onAssetClick}
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
                onRename={doRefresh}
                onDelete={doRefresh}
                onAssetClick={onAssetClick}
            />
            <div className="h-10" />
            <FilesTable
                directoryId={directoryId}
                items={visibleFileAssets}
                isLoading={isLoadingAssets}
                columnDisplayMode={columnDisplayMode}
                query={query}
                onCreate={doRefresh}
                onRename={doRefresh}
                onDelete={doRefresh}
                onAssetClick={onAssetClick}
            />
        </>
    )
}

export default DirectoryView
