/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as common from 'enso-common'

import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as hooks from '../../hooks'

import DirectoriesTable from './directoryRows'
import DriveBar from './driveBar'
import FilesTable from './filesTable'
import ProjectsTable from './projectsTable'
import SecretsTable from './secretsTable'

// =================
// === Constants ===
// =================

/** The `localStorage` key under which the ID of the current directory is stored. */
const DIRECTORY_STACK_KEY = `${common.PRODUCT_NAME.toLowerCase()}-dashboard-directory-stack`

// =====================
// === DirectoryView ===
// =====================

/** Props for a {@link DirectoryView}. */
export interface DirectoryViewProps {
    directoryId: backendModule.DirectoryId
    setDirectoryId: (directoryId: backendModule.DirectoryId) => void
    query: string
    refresh: hooks.RefreshState
    doRefresh: () => void
    onOpenIde: (project: backendModule.ProjectAsset) => void
    onCloseIde: () => void
    onAssetClick: (asset: backendModule.Asset, event: React.MouseEvent<HTMLTableRowElement>) => void
    appRunner: AppRunner | null
    experimentalShowColumnDisplayModeSwitcher: boolean
}

function DirectoryView(props: DirectoryViewProps) {
    const {
        directoryId,
        setDirectoryId,
        query,
        refresh,
        doRefresh,
        onOpenIde,
        onCloseIde,
        onAssetClick,
        appRunner,
        experimentalShowColumnDisplayModeSwitcher,
    } = props
    const { organization } = authProvider.useFullUserSession()
    const { backend } = backendProvider.useBackend()

    const [isLoadingAssets, setIsLoadingAssets] = React.useState(true)
    const [directoryStack, setDirectoryStack] = React.useState<backendModule.DirectoryAsset[]>([])
    // Defined by the spec as `compact` by default, however it is not ready yet.
    const [columnDisplayMode, setColumnDisplayMode] = React.useState(
        columnModule.ColumnDisplayMode.release
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

    const directory = directoryStack[directoryStack.length - 1] ?? null
    const parentDirectory = directoryStack[directoryStack.length - 2] ?? null

    React.useEffect(() => {
        setAssets([])
    }, [setAssets, backend])

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
    }, [setDirectoryId])

    React.useEffect(() => {
        if (directoryId === backendModule.rootDirectoryId(organization.id)) {
            localStorage.removeItem(DIRECTORY_STACK_KEY)
        } else {
            localStorage.setItem(DIRECTORY_STACK_KEY, JSON.stringify(directoryStack))
        }
    }, [directoryStack, directoryId, organization.id])

    const assets = hooks.useAsyncEffect(
        [],
        async signal => {
            setIsLoadingAssets(true)
            const result = await backend.listDirectory({ parentId: directoryId })
            if (!signal.aborted) {
                setIsLoadingAssets(false)
            }
            return result
        },
        [directoryId, refresh, backend]
    )

    React.useEffect(() => {
        setAssets(assets)
    }, [assets])

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

    const enterDirectory = (directoryAsset: backendModule.DirectoryAsset) => {
        setDirectoryId(directoryAsset.id)
        setDirectoryStack([...directoryStack, directoryAsset])
        setAssets([])
    }

    const exitDirectory = () => {
        setDirectoryId(parentDirectory?.id ?? backendModule.rootDirectoryId(organization.id))
        setDirectoryStack(
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            directoryStack.slice(0, -1)
        )
        setAssets([])
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
                directoryId={directoryId}
                items={visibleProjectAssets}
                isLoading={isLoadingAssets}
                columnDisplayMode={columnDisplayMode}
                onCreate={doRefresh}
                onRename={doRefresh}
                onDelete={doRefresh}
                onOpenIde={onOpenIde}
                onCloseIde={onCloseIde}
                onAssetClick={onAssetClick}
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
