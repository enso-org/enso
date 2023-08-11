/** @file Table displaying a list of projects. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import * as common from 'enso-common'

import * as array from '../array'
import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as hooks from '../../hooks'
import * as permissions from '../permissions'
import * as presenceModule from '../presence'
import * as shortcuts from '../shortcuts'
import * as sorting from '../sorting'
import * as string from '../../string'
import * as uniqueString from '../../uniqueString'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import AssetRow from './assetRow'
import Button from './button'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import ContextMenus from './contextMenus'
import GlobalContextMenu from './globalContextMenu'
import Table from './table'

// =================
// === Constants ===
// =================

/** The `localStorage` key under which the ID of the current directory is stored. */
const EXTRA_COLUMNS_KEY =
    common.PRODUCT_NAME.toLowerCase() + '-dashboard-directory-list-extra-columns'

/** A value that represents that the first argument is less than the second argument, in a
 * sorting function. */
const COMPARE_LESS_THAN = -1
/** The value returned when {@link Array.findIndex} fails. */
const NOT_FOUND = -1
/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'item'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'items'
// This is a function, even though it is not syntactically a function.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)
/** Placeholder row. */
const PLACEHOLDER = (
    <span className="opacity-75">
        You have no projects yet. Go ahead and create one using the button above, or open a template
        from the home screen.
    </span>
)
/** Placeholder row for directories that are empty. */
export const EMPTY_DIRECTORY_PLACEHOLDER = (
    <span className="px-2 opacity-75">This folder is empty.</span>
)

/** The {@link RegExp} matching a directory name following the default naming convention. */
const DIRECTORY_NAME_REGEX = /^New_Folder_(?<directoryIndex>\d+)$/
/** The default prefix of an automatically generated directory. */
const DIRECTORY_NAME_DEFAULT_PREFIX = 'New_Folder_'

// =====================
// === splicedAssets ===
// =====================

/** Insert assets into the assets list at the correct position, removing a "This folder is empty"
 * placeholder asset, if one exists. */
function splicedAssets(
    oldAssets: backendModule.AnyAsset[],
    assetsToInsert: backendModule.AnyAsset[],
    parentKey: backendModule.DirectoryId | null,
    predicate: (asset: backendModule.AnyAsset) => boolean
) {
    const newAssets = Array.from(oldAssets)
    const insertIndex = oldAssets.findIndex(predicate)
    const firstChild = oldAssets[insertIndex]
    const numberOfItemsToRemove = firstChild?.type === backendModule.AssetType.specialEmpty ? 1 : 0
    newAssets.splice(
        insertIndex === NOT_FOUND
            ? oldAssets.findIndex(asset => asset.id === parentKey) + 1
            : insertIndex,
        numberOfItemsToRemove,
        ...assetsToInsert
    )
    return newAssets
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
    appRunner: AppRunner | null
    sortColumn: columnModule.SortableColumn | null
    setSortColumn: (column: columnModule.SortableColumn | null) => void
    sortDirection: sorting.SortDirection | null
    setSortDirection: (sortDirection: sorting.SortDirection | null) => void
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    getDepth: (id: backendModule.AssetId) => number
    doToggleDirectoryExpansion: (
        directoryId: backendModule.DirectoryId,
        key: backendModule.DirectoryId,
        title?: string
    ) => void
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
    setPresence: (presence: presenceModule.Presence) => void
    isEditingName: boolean
}

/** The default {@link AssetRowState} associated with a {@link AssetRow}. */
export const INITIAL_ROW_STATE: AssetRowState = Object.freeze({
    setPresence: () => {
        // Ignored. This MUST be replaced by the row component. It should also update `presence`.
    },
    isEditingName: false,
})

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
    appRunner: AppRunner | null
    query: string
    initialProjectName: string | null
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    assetListEvents: assetListEventModule.AssetListEvent[]
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
    const {
        appRunner,
        query,
        initialProjectName,
        assetEvents,
        dispatchAssetEvent,
        assetListEvents,
        dispatchAssetListEvent,
        doOpenIde,
        doCloseIde: rawDoCloseIde,
        loadingProjectManagerDidFail,
        isListingRemoteDirectoryWhileOffline,
        isListingLocalDirectoryAndWillFail,
        isListingRemoteDirectoryAndWillFail,
    } = props
    const logger = loggerProvider.useLogger()
    const { organization, user, accessToken } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [initialized, setInitialized] = React.useState(false)
    const [assets, setAssets] = React.useState<backendModule.AnyAsset[]>([])
    const [isLoading, setIsLoading] = React.useState(true)
    const [extraColumns, setExtraColumns] = React.useState(
        () => new Set<columnModule.ExtraColumn>()
    )
    const [sortColumn, setSortColumn] = React.useState<columnModule.SortableColumn | null>(null)
    const [sortDirection, setSortDirection] = React.useState<sorting.SortDirection | null>(null)
    const [selectedKeys, setSelectedKeys] = React.useState(() => new Set<backendModule.AssetId>())
    const [nameOfProjectToImmediatelyOpen, setNameOfProjectToImmediatelyOpen] =
        React.useState(initialProjectName)
    // Items in the root directory have a depth of 0.
    const itemDepthsRef = React.useRef(new Map<backendModule.AssetId, number>())
    const filter = React.useMemo(() => {
        if (query === '') {
            return null
        } else {
            const regex = new RegExp(string.regexEscape(query), 'i')
            return (asset: backendModule.AnyAsset) => regex.test(asset.title)
        }
    }, [query])
    const displayItems = React.useMemo(() => {
        if (sortColumn == null || sortDirection == null) {
            return assets
        } else {
            const sortDescendingMultiplier = -1
            const multiplier = {
                [sorting.SortDirection.ascending]: 1,
                [sorting.SortDirection.descending]: sortDescendingMultiplier,
            }[sortDirection]
            let compare: (a: backendModule.AnyAsset, b: backendModule.AnyAsset) => number
            switch (sortColumn) {
                case columnModule.Column.name: {
                    compare = (a, b) =>
                        a.title > b.title ? 1 : a.title < b.title ? COMPARE_LESS_THAN : 0
                    break
                }
                case columnModule.Column.modified: {
                    compare = (a, b) =>
                        Number(new Date(a.modifiedAt)) - Number(new Date(b.modifiedAt))
                    break
                }
            }
            const itemsById = new Map(assets.map(item => [item.id, item]))
            const itemIndices = new Map(assets.map((item, index) => [item, index]))
            return Array.from(assets).sort((a, b) => {
                let normalizedA = a
                let normalizedB = b
                let aDepth = itemDepthsRef.current.get(normalizedA.id) ?? 0
                let bDepth = itemDepthsRef.current.get(normalizedB.id) ?? 0
                while (aDepth > bDepth) {
                    aDepth -= 1
                    // Using `normalizedA` as a fallback is INCORRECT, but it is better than
                    // throwing an exception. This will never happen unless the parent folder is not
                    // visible anyway, implying the assets list is already in an invalid state.
                    normalizedA = itemsById.get(normalizedA.parentId) ?? normalizedA
                }
                while (bDepth > aDepth) {
                    bDepth -= 1
                    normalizedB = itemsById.get(normalizedB.parentId) ?? normalizedB
                }
                while (normalizedA.parentId !== normalizedB.parentId) {
                    normalizedA = itemsById.get(normalizedA.parentId) ?? normalizedA
                    normalizedB = itemsById.get(normalizedA.parentId) ?? normalizedB
                }
                return normalizedA.type === normalizedB.type
                    ? multiplier * compare(normalizedA, normalizedB)
                    : (itemIndices.get(normalizedA) ?? 0) - (itemIndices.get(normalizedB) ?? 0)
            })
        }
    }, [assets, sortColumn, sortDirection])

    React.useEffect(() => {
        setIsLoading(true)
    }, [backend])

    React.useEffect(() => {
        if (backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail) {
            setIsLoading(false)
        }
    }, [loadingProjectManagerDidFail, backend.type])

    const overwriteAssets = React.useCallback(
        (newAssets: backendModule.AnyAsset[]) => {
            setAssets(newAssets)
            if (nameOfProjectToImmediatelyOpen != null) {
                const projectToLoad = newAssets
                    .filter(backendModule.assetIsProject)
                    .find(projectAsset => projectAsset.title === nameOfProjectToImmediatelyOpen)
                if (projectToLoad != null) {
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.openProject,
                        id: projectToLoad.id,
                    })
                }
                setNameOfProjectToImmediatelyOpen(null)
            }
            if (!initialized && initialProjectName != null) {
                setInitialized(true)
                if (!newAssets.some(asset => asset.title === initialProjectName)) {
                    const errorMessage = `No project named '${initialProjectName}' was found.`
                    toastify.toast.error(errorMessage)
                    logger.error(`Error opening project on startup: ${errorMessage}`)
                }
            }
        },
        [
            initialized,
            initialProjectName,
            logger,
            nameOfProjectToImmediatelyOpen,
            /* should never change */ setNameOfProjectToImmediatelyOpen,
            /* should never change */ dispatchAssetEvent,
        ]
    )

    React.useEffect(() => {
        overwriteAssets([])
        // `setAssets` is a callback, not a dependency.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [backend])

    hooks.useAsyncEffect(
        null,
        async signal => {
            switch (backend.type) {
                case backendModule.BackendType.local: {
                    if (!isListingLocalDirectoryAndWillFail) {
                        const newAssets = await backend.listDirectory({ parentId: null }, null)
                        if (!signal.aborted) {
                            setIsLoading(false)
                            overwriteAssets(newAssets)
                        }
                    }
                    break
                }
                case backendModule.BackendType.remote: {
                    if (
                        !isListingRemoteDirectoryAndWillFail &&
                        !isListingRemoteDirectoryWhileOffline
                    ) {
                        const newAssets = await backend.listDirectory({ parentId: null }, null)
                        if (!signal.aborted) {
                            setIsLoading(false)
                            overwriteAssets(newAssets)
                        }
                    } else {
                        setIsLoading(false)
                    }
                    break
                }
            }
        },
        [accessToken, organization, backend]
    )

    React.useEffect(() => {
        setInitialized(true)
        const extraColumnsJson = localStorage.getItem(EXTRA_COLUMNS_KEY)
        if (extraColumnsJson != null) {
            const savedExtraColumns: unknown = JSON.parse(extraColumnsJson)
            if (
                Array.isArray(savedExtraColumns) &&
                savedExtraColumns.every(array.includesPredicate(columnModule.EXTRA_COLUMNS))
            ) {
                setExtraColumns(new Set(savedExtraColumns))
            }
        }
    }, [])

    React.useEffect(() => {
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        itemDepthsRef.current.set(backend.rootDirectoryId(organization), -1)
    }, [backend, organization])

    React.useEffect(() => {
        if (initialized) {
            localStorage.setItem(EXTRA_COLUMNS_KEY, JSON.stringify(Array.from(extraColumns)))
        }
    }, [extraColumns, initialized])

    const getDepth = React.useCallback(
        (id: backendModule.AssetId) => itemDepthsRef.current.get(id) ?? 0,
        []
    )

    React.useEffect(() => {
        // Remove unused keys.
        const oldDepths = itemDepthsRef.current
        itemDepthsRef.current = new Map(
            assets.map(backendModule.getAssetId).flatMap(key => {
                const depth = oldDepths.get(key)
                return depth != null ? [[key, depth]] : []
            })
        )
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        itemDepthsRef.current.set(backend.rootDirectoryId(organization), -1)
    }, [assets, backend, organization])

    const expandedDirectoriesRef = React.useRef(new Set<backendModule.DirectoryId>())
    React.useEffect(() => {
        expandedDirectoriesRef.current = new Set()
    }, [backend])
    const directoryListAbortControllersRef = React.useRef(
        new Map<backendModule.DirectoryId, AbortController>()
    )
    const doToggleDirectoryExpansion = React.useCallback(
        (
            directoryId: backendModule.DirectoryId,
            key: backendModule.DirectoryId,
            title?: string
        ) => {
            const set = expandedDirectoriesRef.current
            if (set.has(directoryId)) {
                const abortController = directoryListAbortControllersRef.current.get(directoryId)
                if (abortController != null) {
                    abortController.abort()
                    directoryListAbortControllersRef.current.delete(directoryId)
                }
                set.delete(directoryId)
                const foldersToCollapse = new Set([directoryId])
                setAssets(
                    assets.filter(item => {
                        const shouldKeep = !foldersToCollapse.has(item.parentId)
                        if (item.type === backendModule.AssetType.directory && !shouldKeep) {
                            foldersToCollapse.add(item.id)
                        }
                        return shouldKeep
                    })
                )
            } else {
                const childDepth = getDepth(key) + 1
                set.add(directoryId)
                const loadingAssetId = backendModule.LoadingAssetId(uniqueString.uniqueString())
                itemDepthsRef.current.set(loadingAssetId, childDepth)
                setAssets(
                    array.splicedAfter(
                        assets,
                        [
                            {
                                type: backendModule.AssetType.specialLoading,
                                title: '',
                                id: loadingAssetId,
                                modifiedAt: dateTime.toRfc3339(new Date()),
                                parentId: directoryId,
                                permissions: [],
                                projectState: null,
                            },
                        ],
                        item => item.id === key
                    )
                )
                void (async () => {
                    const abortController = new AbortController()
                    directoryListAbortControllersRef.current.set(directoryId, abortController)
                    const returnedItems = await backend.listDirectory(
                        { parentId: directoryId },
                        title ?? null
                    )
                    if (!abortController.signal.aborted) {
                        const childItems: backendModule.AnyAsset[] =
                            returnedItems.length !== 0
                                ? returnedItems
                                : [
                                      {
                                          type: backendModule.AssetType.specialEmpty,
                                          title: '',
                                          id: backendModule.EmptyAssetId(
                                              uniqueString.uniqueString()
                                          ),
                                          modifiedAt: dateTime.toRfc3339(new Date()),
                                          parentId: directoryId,
                                          permissions: [],
                                          projectState: null,
                                      },
                                  ]
                        for (const childItem of childItems) {
                            itemDepthsRef.current.set(childItem.id, childDepth)
                        }
                        setAssets(oldItems => {
                            let firstChildIndex = oldItems.findIndex(
                                item => item.parentId === directoryId
                            )
                            if (firstChildIndex === NOT_FOUND) {
                                firstChildIndex = oldItems.findIndex(item => item.id === key) + 1
                            }
                            let numberOfChildren = 1
                            while (
                                oldItems[firstChildIndex + numberOfChildren]?.parentId ===
                                directoryId
                            ) {
                                numberOfChildren += 1
                            }
                            const oldChildren = oldItems.slice(
                                firstChildIndex,
                                // Subtract one extra, to exclude the placeholder "loading" asset.
                                firstChildIndex + numberOfChildren - 1
                            )
                            const newChildren =
                                oldChildren.length === 0
                                    ? childItems
                                    : [...oldChildren, ...returnedItems].sort(
                                          backendModule.compareAssets
                                      )
                            const newItems = Array.from(oldItems)
                            newItems.splice(firstChildIndex, numberOfChildren, ...newChildren)
                            return newItems
                        })
                    }
                })()
            }
        },
        [assets, backend, getDepth]
    )

    const getNewProjectName = React.useCallback(
        (templateId: string | null, parentId: backendModule.DirectoryId | null) => {
            const prefix = `${templateId ?? 'New_Project'}_`
            const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
            const actualParentId = parentId ?? backend.rootDirectoryId(organization)
            const projectIndices = assets
                .filter(item => item.parentId === actualParentId)
                .map(project => projectNameTemplate.exec(project.title)?.groups?.projectIndex)
                .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
            return `${prefix}${Math.max(0, ...projectIndices) + 1}`
        },
        [assets, backend, organization]
    )

    hooks.useEventHandler(assetListEvents, event => {
        switch (event.type) {
            case assetListEventModule.AssetListEventType.newFolder: {
                const parentId = event.parentId ?? backend.rootDirectoryId(organization)
                const directoryIndices = assets
                    .filter(item => item.parentId === parentId)
                    .map(item => DIRECTORY_NAME_REGEX.exec(item.title))
                    .map(match => match?.groups?.directoryIndex)
                    .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
                const title = `${DIRECTORY_NAME_DEFAULT_PREFIX}${
                    Math.max(0, ...directoryIndices) + 1
                }`
                const placeholderItem: backendModule.DirectoryAsset = {
                    id: backendModule.DirectoryId(uniqueString.uniqueString()),
                    title,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId ?? backend.rootDirectoryId(organization),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                    projectState: null,
                    type: backendModule.AssetType.directory,
                }
                const typeOrder = backendModule.ASSET_TYPE_ORDER[placeholderItem.type]
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    !expandedDirectoriesRef.current.has(event.parentId)
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssets(oldItems =>
                    splicedAssets(
                        oldItems,
                        [placeholderItem],
                        event.parentKey,
                        item =>
                            item.parentId === placeholderItem.parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= typeOrder
                    )
                )
                itemDepthsRef.current.set(
                    placeholderItem.id,
                    (itemDepthsRef.current.get(placeholderItem.parentId) ?? 0) + 1
                )
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.newFolder,
                    placeholderId: placeholderItem.id,
                })
                break
            }
            case assetListEventModule.AssetListEventType.newProject: {
                const projectName = getNewProjectName(event.templateId, event.parentId)
                const dummyId = backendModule.ProjectId(uniqueString.uniqueString())
                const placeholderItem: backendModule.ProjectAsset = {
                    id: dummyId,
                    title: projectName,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId ?? backend.rootDirectoryId(organization),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                    projectState: { type: backendModule.ProjectState.placeholder },
                    type: backendModule.AssetType.project,
                }
                const typeOrder = backendModule.ASSET_TYPE_ORDER[placeholderItem.type]
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    !expandedDirectoriesRef.current.has(event.parentId)
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssets(oldItems =>
                    splicedAssets(
                        oldItems,
                        [placeholderItem],
                        event.parentKey,
                        item =>
                            item.parentId === placeholderItem.parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= typeOrder
                    )
                )
                itemDepthsRef.current.set(
                    placeholderItem.id,
                    (itemDepthsRef.current.get(placeholderItem.parentId) ?? 0) + 1
                )
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.newProject,
                    placeholderId: dummyId,
                    templateId: event.templateId,
                    onSpinnerStateChange: event.onSpinnerStateChange,
                })
                break
            }
            case assetListEventModule.AssetListEventType.uploadFiles: {
                const reversedFiles = Array.from(event.files).reverse()
                const parentId = event.parentId ?? backend.rootDirectoryId(organization)
                const placeholderFiles: backendModule.FileAsset[] = reversedFiles
                    .filter(backendModule.fileIsNotProject)
                    .map(file => ({
                        type: backendModule.AssetType.file,
                        id: backendModule.FileId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId,
                        permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: null,
                    }))
                const placeholderProjects: backendModule.ProjectAsset[] = reversedFiles
                    .filter(backendModule.fileIsProject)
                    .map(file => ({
                        type: backendModule.AssetType.project,
                        id: backendModule.ProjectId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId,
                        permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: {
                            type: backendModule.ProjectState.new,
                        },
                    }))
                const fileTypeOrder = backendModule.ASSET_TYPE_ORDER[backendModule.AssetType.file]
                const projectTypeOrder =
                    backendModule.ASSET_TYPE_ORDER[backendModule.AssetType.project]
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    !expandedDirectoriesRef.current.has(event.parentId)
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssets(oldItems =>
                    array.spliceBefore(
                        splicedAssets(
                            oldItems,
                            placeholderFiles,
                            event.parentKey,
                            item =>
                                item.parentId === parentId &&
                                backendModule.ASSET_TYPE_ORDER[item.type] >= fileTypeOrder
                        ),
                        placeholderProjects,
                        item =>
                            item.parentId === parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= projectTypeOrder
                    )
                )
                const depth =
                    event.parentId != null
                        ? (itemDepthsRef.current.get(event.parentId) ?? 0) + 1
                        : 0
                for (const file of placeholderFiles) {
                    itemDepthsRef.current.set(file.id, depth)
                }
                for (const project of placeholderProjects) {
                    itemDepthsRef.current.set(project.id, depth)
                }
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.uploadFiles,
                    files: new Map(
                        [...placeholderFiles, ...placeholderProjects].map((placeholderItem, i) => [
                            placeholderItem.id,
                            // This is SAFE, as `placeholderItems` is created using a map on
                            // `event.files`.
                            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                            event.files[i]!,
                        ])
                    ),
                })
                break
            }
            case assetListEventModule.AssetListEventType.newSecret: {
                const placeholderItem: backendModule.SecretAsset = {
                    id: backendModule.SecretId(uniqueString.uniqueString()),
                    title: event.name,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId ?? backend.rootDirectoryId(organization),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                    projectState: null,
                    type: backendModule.AssetType.secret,
                }
                const typeOrder = backendModule.ASSET_TYPE_ORDER[placeholderItem.type]
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    !expandedDirectoriesRef.current.has(event.parentId)
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssets(oldItems =>
                    splicedAssets(
                        oldItems,
                        [placeholderItem],
                        event.parentKey,
                        item =>
                            item.parentId === placeholderItem.parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= typeOrder
                    )
                )
                itemDepthsRef.current.set(
                    placeholderItem.id,
                    (itemDepthsRef.current.get(placeholderItem.parentId) ?? 0) + 1
                )
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.newSecret,
                    placeholderId: placeholderItem.id,
                    value: event.value,
                })
                break
            }
            case assetListEventModule.AssetListEventType.delete: {
                setAssets(oldItems => oldItems.filter(item => item.id !== event.id))
                itemDepthsRef.current.delete(event.id)
                break
            }
        }
    })

    const doOpenManually = React.useCallback(
        (projectId: backendModule.ProjectId) => {
            dispatchAssetEvent({
                type: assetEventModule.AssetEventType.openProject,
                id: projectId,
            })
        },
        [/* should never change */ dispatchAssetEvent]
    )

    const doCloseIde = React.useCallback(() => {
        dispatchAssetEvent({
            type: assetEventModule.AssetEventType.cancelOpeningAllProjects,
        })
        rawDoCloseIde()
    }, [rawDoCloseIde, /* should never change */ dispatchAssetEvent])

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): AssetsTableState => ({
            appRunner,
            sortColumn,
            setSortColumn,
            sortDirection,
            setSortDirection,
            assetEvents,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            getDepth,
            doToggleDirectoryExpansion,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        }),
        [
            appRunner,
            sortColumn,
            sortDirection,
            assetEvents,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            getDepth,
            doToggleDirectoryExpansion,
            /* should never change */ setSortColumn,
            /* should never change */ setSortDirection,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    return (
        <div className="flex-1 overflow-auto">
            <div className="flex flex-col w-min min-w-full h-full">
                <div className="h-0">
                    <div className="block sticky right-0 px-2 py-1 ml-auto mt-3.5 w-29 z-10">
                        <div className="inline-flex gap-3">
                            {columnModule.EXTRA_COLUMNS.map(column => (
                                <Button
                                    key={column}
                                    active={extraColumns.has(column)}
                                    image={columnModule.EXTRA_COLUMN_IMAGES[column]}
                                    onClick={() => {
                                        const newExtraColumns = new Set(extraColumns)
                                        if (extraColumns.has(column)) {
                                            newExtraColumns.delete(column)
                                        } else {
                                            newExtraColumns.add(column)
                                        }
                                        setExtraColumns(newExtraColumns)
                                    }}
                                />
                            ))}
                        </div>
                    </div>
                </div>
                <Table<
                    backendModule.AnyAsset,
                    AssetsTableState,
                    AssetRowState,
                    backendModule.AssetId
                >
                    footer={<tfoot className="h-full"></tfoot>}
                    rowComponent={AssetRow}
                    items={displayItems}
                    filter={filter}
                    isLoading={isLoading}
                    state={state}
                    initialRowState={INITIAL_ROW_STATE}
                    getKey={backendModule.getAssetId}
                    selectedKeys={selectedKeys}
                    setSelectedKeys={setSelectedKeys}
                    placeholder={PLACEHOLDER}
                    columns={columnModule.getColumnList(backend.type, extraColumns).map(column => ({
                        id: column,
                        className: columnModule.COLUMN_CSS_CLASS[column],
                        heading: columnModule.COLUMN_HEADING[column],
                        render: columnModule.COLUMN_RENDERER[column],
                    }))}
                    onContextMenu={(innerSelectedKeys, event, innerSetSelectedKeys) => {
                        event.preventDefault()
                        event.stopPropagation()
                        const pluralized = pluralize(innerSelectedKeys.size)
                        // This is not a React component even though it contains JSX.
                        // eslint-disable-next-line no-restricted-syntax
                        const doDeleteAll = () => {
                            setModal(
                                <ConfirmDeleteModal
                                    description={`${innerSelectedKeys.size} selected ${pluralized}`}
                                    doDelete={() => {
                                        innerSetSelectedKeys(new Set())
                                        dispatchAssetEvent({
                                            type: assetEventModule.AssetEventType.deleteMultiple,
                                            ids: innerSelectedKeys,
                                        })
                                        return Promise.resolve()
                                    }}
                                />
                            )
                        }
                        setModal(
                            <ContextMenus key={uniqueString.uniqueString()} event={event}>
                                {innerSelectedKeys.size !== 0 && (
                                    <ContextMenu>
                                        <ContextMenuEntry
                                            action={shortcuts.KeyboardAction.moveAllToTrash}
                                            doAction={doDeleteAll}
                                        />
                                    </ContextMenu>
                                )}
                                <GlobalContextMenu
                                    directoryKey={null}
                                    directoryId={null}
                                    dispatchAssetListEvent={dispatchAssetListEvent}
                                />
                            </ContextMenus>
                        )
                    }}
                />
            </div>
        </div>
    )
}
