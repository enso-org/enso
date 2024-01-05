/** @file Table displaying a list of projects. */
import * as React from 'react'
import * as toast from 'react-toastify'

import * as array from '../array'
import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import type * as assetQuery from '../../assetQuery'
import * as assetTreeNode from '../assetTreeNode'
import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as drag from '../drag'
import * as hooks from '../../hooks'
import * as localStorageModule from '../localStorage'
import * as localStorageProvider from '../../providers/localStorage'
import * as pasteDataModule from '../pasteData'
import * as permissions from '../permissions'
import * as set from '../set'
import * as shortcutsModule from '../shortcuts'
import * as shortcutsProvider from '../../providers/shortcuts'
import * as sorting from '../sorting'
import * as string from '../../string'
import * as uniqueString from '../../uniqueString'
import type * as visibilityModule from '../visibility'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'

import type * as assetSettingsPanel from './assetSettingsPanel'
import * as categorySwitcher from './categorySwitcher'
import AssetNameColumn from './assetNameColumn'
import AssetRow from './assetRow'
import Button from './button'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenus from './contextMenus'
import DragModal from './dragModal'
import GlobalContextMenu from './globalContextMenu'
import MenuEntry from './menuEntry'
import Table from './table'

// =================
// === Constants ===
// =================

/** The number of pixels the header bar should shrink when the extra column selector is visible. */
const TABLE_HEADER_WIDTH_SHRINKAGE_PX = 116
/** A value that represents that the first argument is less than the second argument, in a
 * sorting function. */
const COMPARE_LESS_THAN = -1
/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'item'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'items'
// This is a function, even though it is not syntactically a function.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)
/** The default placeholder row. */
const PLACEHOLDER = (
    <span className="opacity-75">
        You have no files. Go ahead and create one using the buttons above, or open a template from
        the home screen.
    </span>
)
/** A placeholder row for when a query (text or labels) is active. */
const QUERY_PLACEHOLDER = <span className="opacity-75">No files match the current filters.</span>
/** The placeholder row for the Trash category. */
const TRASH_PLACEHOLDER = <span className="opacity-75 px-1.5">Your trash is empty.</span>
/** Placeholder row for directories that are empty. */
export const EMPTY_DIRECTORY_PLACEHOLDER = (
    <span className="px-2 opacity-75">This folder is empty.</span>
)

/** The {@link RegExp} matching a directory name following the default naming convention. */
const DIRECTORY_NAME_REGEX = /^New_Folder_(?<directoryIndex>\d+)$/
/** The default prefix of an automatically generated directory. */
const DIRECTORY_NAME_DEFAULT_PREFIX = 'New_Folder_'

// ===================================
// === insertAssetTreeNodeChildren ===
// ===================================

/** Return a directory, with new children added into its list of children.
 * All children MUST have the same asset type. */
function insertAssetTreeNodeChildren(
    item: assetTreeNode.AssetTreeNode,
    children: backendModule.AnyAsset[],
    directoryKey: backendModule.AssetId,
    directoryId: backendModule.DirectoryId
): assetTreeNode.AssetTreeNode {
    const depth = item.depth + 1
    const typeOrder = children[0] != null ? backendModule.ASSET_TYPE_ORDER[children[0].type] : 0
    const nodes = (item.children ?? []).filter(
        node => node.item.type !== backendModule.AssetType.specialEmpty
    )
    const nodesToInsert = children.map(asset =>
        assetTreeNode.AssetTreeNode.fromAsset(asset, directoryKey, directoryId, depth)
    )
    const newNodes = array.splicedBefore(
        nodes,
        nodesToInsert,
        innerItem => backendModule.ASSET_TYPE_ORDER[innerItem.item.type] >= typeOrder
    )
    return item.with({ children: newNodes })
}

/** Return a directory, with new children added into its list of children.
 * The children MAY be of different asset types. */
function insertArbitraryAssetTreeNodeChildren(
    item: assetTreeNode.AssetTreeNode,
    children: backendModule.AnyAsset[],
    directoryKey: backendModule.AssetId,
    directoryId: backendModule.DirectoryId,
    getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
): assetTreeNode.AssetTreeNode {
    const depth = item.depth + 1
    const nodes = (item.children ?? []).filter(
        node => node.item.type !== backendModule.AssetType.specialEmpty
    )
    const byType: Record<backendModule.AssetType, backendModule.AnyAsset[]> = {
        [backendModule.AssetType.directory]: [],
        [backendModule.AssetType.project]: [],
        [backendModule.AssetType.file]: [],
        [backendModule.AssetType.secret]: [],
        [backendModule.AssetType.specialLoading]: [],
        [backendModule.AssetType.specialEmpty]: [],
    }
    for (const child of children) {
        byType[child.type].push(child)
    }
    let newNodes = nodes
    for (const childrenOfSpecificType of Object.values(byType)) {
        const firstChild = childrenOfSpecificType[0]
        if (firstChild) {
            const typeOrder = backendModule.ASSET_TYPE_ORDER[firstChild.type]
            const nodesToInsert = childrenOfSpecificType.map(asset =>
                assetTreeNode.AssetTreeNode.fromAsset(
                    asset,
                    directoryKey,
                    directoryId,
                    depth,
                    getKey
                )
            )
            newNodes = array.splicedBefore(
                newNodes,
                nodesToInsert,
                innerItem => backendModule.ASSET_TYPE_ORDER[innerItem.item.type] >= typeOrder
            )
        }
    }
    return newNodes === nodes ? item : item.with({ children: newNodes })
}

// =============================
// === Category to filter by ===
// =============================

const CATEGORY_TO_FILTER_BY: Record<categorySwitcher.Category, backendModule.FilterBy | null> = {
    [categorySwitcher.Category.recent]: null,
    [categorySwitcher.Category.drafts]: null,
    [categorySwitcher.Category.home]: backendModule.FilterBy.active,
    [categorySwitcher.Category.root]: null,
    [categorySwitcher.Category.trash]: backendModule.FilterBy.trashed,
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
    numberOfSelectedItems: number
    category: categorySwitcher.Category
    labels: Map<backendModule.LabelName, backendModule.Label>
    deletedLabelNames: Set<backendModule.LabelName>
    hasPasteData: boolean
    setPasteData: (pasteData: pasteDataModule.PasteData<Set<backendModule.AssetId>>) => void
    sortColumn: columnModule.SortableColumn | null
    setSortColumn: (column: columnModule.SortableColumn | null) => void
    sortDirection: sorting.SortDirection | null
    setSortDirection: (sortDirection: sorting.SortDirection | null) => void
    query: assetQuery.AssetQuery
    setQuery: React.Dispatch<React.SetStateAction<assetQuery.AssetQuery>>
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    setAssetSettingsPanelProps: React.Dispatch<
        React.SetStateAction<assetSettingsPanel.AssetSettingsPanelRequiredProps | null>
    >
    nodeMap: Readonly<
        React.MutableRefObject<ReadonlyMap<backendModule.AssetId, assetTreeNode.AssetTreeNode>>
    >
    doToggleDirectoryExpansion: (
        directoryId: backendModule.DirectoryId,
        key: backendModule.AssetId,
        title?: string | null,
        override?: boolean
    ) => void
    /** Called when the project is opened via the `ProjectActionButton`. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (
        project: backendModule.ProjectAsset,
        setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
        switchPage: boolean
    ) => void
    doCloseIde: (project: backendModule.ProjectAsset) => void
    doCreateLabel: (value: string, color: backendModule.LChColor) => Promise<void>
    doCopy: () => void
    doCut: () => void
    doPaste: (newParentKey: backendModule.AssetId, newParentId: backendModule.DirectoryId) => void
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
    setVisibility: (visibility: visibilityModule.Visibility) => void
    isEditingName: boolean
    temporarilyAddedLabels: ReadonlySet<backendModule.LabelName>
    temporarilyRemovedLabels: ReadonlySet<backendModule.LabelName>
}

/** The default {@link AssetRowState} associated with a {@link AssetRow}. */
export const INITIAL_ROW_STATE = Object.freeze<AssetRowState>({
    setVisibility: () => {
        // Ignored. This MUST be replaced by the row component. It should also update `visibility`.
    },
    isEditingName: false,
    temporarilyAddedLabels: set.EMPTY,
    temporarilyRemovedLabels: set.EMPTY,
})

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
    query: assetQuery.AssetQuery
    setQuery: React.Dispatch<React.SetStateAction<assetQuery.AssetQuery>>
    category: categorySwitcher.Category
    allLabels: Map<backendModule.LabelName, backendModule.Label>
    initialProjectName: string | null
    projectStartupInfo: backendModule.ProjectStartupInfo | null
    deletedLabelNames: Set<backendModule.LabelName>
    /** These events will be dispatched the next time the assets list is refreshed, rather than
     * immediately. */
    queuedAssetEvents: assetEventModule.AssetEvent[]
    assetListEvents: assetListEventModule.AssetListEvent[]
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    setAssetSettingsPanelProps: React.Dispatch<
        React.SetStateAction<assetSettingsPanel.AssetSettingsPanelRequiredProps | null>
    >
    doOpenIde: (
        project: backendModule.ProjectAsset,
        setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
        switchPage: boolean
    ) => void
    doCloseIde: (project: backendModule.ProjectAsset) => void
    doCreateLabel: (value: string, color: backendModule.LChColor) => Promise<void>
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
    const {
        query,
        setQuery,
        category,
        allLabels,
        deletedLabelNames,
        initialProjectName,
        projectStartupInfo,
        queuedAssetEvents: rawQueuedAssetEvents,
        assetListEvents,
        dispatchAssetListEvent,
        setAssetSettingsPanelProps,
        assetEvents,
        dispatchAssetEvent,
        doOpenIde,
        doCloseIde: rawDoCloseIde,
        doCreateLabel,
        loadingProjectManagerDidFail,
        isListingRemoteDirectoryWhileOffline,
        isListingLocalDirectoryAndWillFail,
        isListingRemoteDirectoryAndWillFail,
    } = props
    const { organization, user, accessToken } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const toastAndLog = hooks.useToastAndLog()
    const [initialized, setInitialized] = React.useState(false)
    const [isLoading, setIsLoading] = React.useState(true)
    const [extraColumns, setExtraColumns] = React.useState(
        () => new Set<columnModule.ExtraColumn>()
    )
    const [sortColumn, setSortColumn] = React.useState<columnModule.SortableColumn | null>(null)
    const [sortDirection, setSortDirection] = React.useState<sorting.SortDirection | null>(null)
    const [selectedKeys, setSelectedKeys] = React.useState(() => new Set<backendModule.AssetId>())
    const [pasteData, setPasteData] = React.useState<pasteDataModule.PasteData<
        Set<backendModule.AssetId>
    > | null>(null)
    const [, setQueuedAssetEvents] = React.useState<assetEventModule.AssetEvent[]>([])
    const [, setNameOfProjectToImmediatelyOpen] = React.useState(initialProjectName)
    const rootDirectoryId = React.useMemo(
        () => backend.rootDirectoryId(organization),
        [backend, organization]
    )
    const [assetTree, setAssetTree] = React.useState<assetTreeNode.AssetTreeNode>(() => {
        const rootParentDirectoryId = backendModule.DirectoryId('')
        return assetTreeNode.AssetTreeNode.fromAsset(
            backendModule.createRootDirectoryAsset(rootDirectoryId),
            rootParentDirectoryId,
            rootParentDirectoryId,
            -1
        )
    })
    const isCloud = backend.type === backendModule.BackendType.remote
    const scrollContainerRef = React.useRef<HTMLDivElement>(null)
    const headerRowRef = React.useRef<HTMLTableRowElement>(null)
    const assetTreeRef = React.useRef<assetTreeNode.AssetTreeNode>(assetTree)
    const pasteDataRef = React.useRef<pasteDataModule.PasteData<Set<backendModule.AssetId>> | null>(
        null
    )
    const nodeMapRef = React.useRef<
        ReadonlyMap<backendModule.AssetId, assetTreeNode.AssetTreeNode>
    >(new Map<backendModule.AssetId, assetTreeNode.AssetTreeNode>())
    const filter = React.useMemo(() => {
        if (query.query === '') {
            return null
        } else {
            return (node: assetTreeNode.AssetTreeNode) => {
                const labels: string[] = node.item.labels ?? []
                const lowercaseName = node.item.title.toLowerCase()
                return (
                    query.labels.every(label => labels.includes(label)) &&
                    query.keywords.every(keyword => lowercaseName.includes(keyword.toLowerCase()))
                )
            }
        }
    }, [query])
    const displayItems = React.useMemo(() => {
        if (sortColumn == null || sortDirection == null) {
            return assetTree.preorderTraversal()
        } else {
            const sortDescendingMultiplier = -1
            const multiplier = {
                [sorting.SortDirection.ascending]: 1,
                [sorting.SortDirection.descending]: sortDescendingMultiplier,
            }[sortDirection]
            let compare: (a: assetTreeNode.AssetTreeNode, b: assetTreeNode.AssetTreeNode) => number
            switch (sortColumn) {
                case columnModule.Column.name: {
                    compare = (a, b) =>
                        multiplier *
                        (a.item.title > b.item.title
                            ? 1
                            : a.item.title < b.item.title
                            ? COMPARE_LESS_THAN
                            : 0)

                    break
                }
                case columnModule.Column.modified: {
                    compare = (a, b) =>
                        multiplier *
                        (Number(new Date(a.item.modifiedAt)) - Number(new Date(b.item.modifiedAt)))
                    break
                }
            }
            return assetTree.preorderTraversal(tree => Array.from(tree).sort(compare))
        }
    }, [assetTree, sortColumn, sortDirection])

    React.useEffect(() => {
        if (rawQueuedAssetEvents.length !== 0) {
            setQueuedAssetEvents(oldEvents => [...oldEvents, ...rawQueuedAssetEvents])
        }
    }, [rawQueuedAssetEvents])

    React.useEffect(() => {
        setIsLoading(true)
    }, [backend, category])

    React.useEffect(() => {
        if (backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail) {
            setIsLoading(false)
        }
    }, [loadingProjectManagerDidFail, backend.type])

    React.useEffect(() => {
        assetTreeRef.current = assetTree
        nodeMapRef.current = new Map(assetTree.preorderTraversal().map(asset => [asset.key, asset]))
    }, [assetTree])

    React.useEffect(() => {
        pasteDataRef.current = pasteData
    }, [pasteData])

    React.useEffect(() => {
        return shortcuts.registerKeyboardHandlers({
            [shortcutsModule.KeyboardAction.cancelCut]: () => {
                if (pasteDataRef.current == null) {
                    return false
                } else {
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.cancelCut,
                        ids: pasteDataRef.current.data,
                    })
                    setPasteData(null)
                    return
                }
            },
        })
    }, [/* should never change */ shortcuts, /* should never change */ dispatchAssetEvent])

    React.useEffect(() => {
        if (isLoading) {
            setNameOfProjectToImmediatelyOpen(initialProjectName)
        } else {
            // The project name here might also be a string with project id, e.g. when opening
            // a project file from explorer on Windows.
            const isInitialProject = (asset: backendModule.AnyAsset) =>
                asset.title === initialProjectName || asset.id === initialProjectName
            const projectToLoad = assetTree
                .preorderTraversal()
                .map(node => node.item)
                .filter(backendModule.assetIsProject)
                .find(isInitialProject)
            if (projectToLoad != null) {
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.openProject,
                    id: projectToLoad.id,
                    shouldAutomaticallySwitchPage: true,
                    runInBackground: false,
                })
            } else {
                toastAndLog(`Could not find project '${initialProjectName}'`)
            }
        }
        // This effect MUST only run when `initialProjectName` is changed.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [initialProjectName])

    const overwriteNodes = React.useCallback(
        (newAssets: backendModule.AnyAsset[]) => {
            // This is required, otherwise we are using an outdated
            // `nameOfProjectToImmediatelyOpen`.
            setNameOfProjectToImmediatelyOpen(oldNameOfProjectToImmediatelyOpen => {
                setInitialized(true)
                const rootParentDirectoryId = backendModule.DirectoryId('')
                const rootDirectory = backendModule.createRootDirectoryAsset(rootDirectoryId)
                const newRootNode = new assetTreeNode.AssetTreeNode(
                    rootDirectoryId,
                    rootDirectory,
                    rootParentDirectoryId,
                    rootParentDirectoryId,
                    newAssets.map(asset =>
                        assetTreeNode.AssetTreeNode.fromAsset(
                            asset,
                            rootDirectory.id,
                            rootDirectory.id,
                            0
                        )
                    ),
                    -1
                )
                setAssetTree(newRootNode)
                // The project name here might also be a string with project id, e.g.
                // when opening a project file from explorer on Windows.
                const isInitialProject = (asset: backendModule.AnyAsset) =>
                    asset.title === oldNameOfProjectToImmediatelyOpen ||
                    asset.id === oldNameOfProjectToImmediatelyOpen
                if (oldNameOfProjectToImmediatelyOpen != null) {
                    const projectToLoad = newAssets
                        .filter(backendModule.assetIsProject)
                        .find(isInitialProject)
                    if (projectToLoad != null) {
                        dispatchAssetEvent({
                            type: assetEventModule.AssetEventType.openProject,
                            id: projectToLoad.id,
                            shouldAutomaticallySwitchPage: true,
                            runInBackground: false,
                        })
                    } else {
                        toastAndLog(`Could not find project '${oldNameOfProjectToImmediatelyOpen}'`)
                    }
                }
                setQueuedAssetEvents(oldQueuedAssetEvents => {
                    if (oldQueuedAssetEvents.length !== 0) {
                        queueMicrotask(() => {
                            for (const event of oldQueuedAssetEvents) {
                                dispatchAssetEvent(event)
                            }
                        })
                    }
                    return []
                })
                return null
            })
        },
        [
            rootDirectoryId,
            /* should never change */ setNameOfProjectToImmediatelyOpen,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ toastAndLog,
        ]
    )

    React.useEffect(() => {
        if (initialized) {
            overwriteNodes([])
        }
        // `overwriteAssets` is a callback, not a dependency.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [backend, category])

    hooks.useAsyncEffect(
        null,
        async signal => {
            switch (backend.type) {
                case backendModule.BackendType.local: {
                    if (!isListingLocalDirectoryAndWillFail) {
                        const newAssets = await backend.listDirectory(
                            {
                                parentId: null,
                                filterBy: CATEGORY_TO_FILTER_BY[category],
                                recentProjects: category === categorySwitcher.Category.recent,
                                labels: null,
                            },
                            null
                        )
                        if (!signal.aborted) {
                            setIsLoading(false)
                            overwriteNodes(newAssets)
                        }
                    }
                    break
                }
                case backendModule.BackendType.remote: {
                    if (
                        !isListingRemoteDirectoryAndWillFail &&
                        !isListingRemoteDirectoryWhileOffline
                    ) {
                        const queuedDirectoryListings = new Map<
                            backendModule.AssetId,
                            backendModule.AnyAsset[]
                        >()
                        const withChildren = (
                            node: assetTreeNode.AssetTreeNode
                        ): assetTreeNode.AssetTreeNode => {
                            const queuedListing = queuedDirectoryListings.get(node.item.id)
                            if (
                                queuedListing == null ||
                                !backendModule.assetIsDirectory(node.item)
                            ) {
                                return node
                            } else {
                                const directoryAsset = node.item
                                const depth = node.depth + 1
                                return node.with({
                                    children: queuedListing.map(asset =>
                                        withChildren(
                                            assetTreeNode.AssetTreeNode.fromAsset(
                                                asset,
                                                directoryAsset.id,
                                                directoryAsset.id,
                                                depth
                                            )
                                        )
                                    ),
                                })
                            }
                        }
                        for (const entry of nodeMapRef.current.values()) {
                            if (
                                backendModule.assetIsDirectory(entry.item) &&
                                entry.children != null
                            ) {
                                const id = entry.item.id
                                void backend
                                    .listDirectory(
                                        {
                                            parentId: id,
                                            filterBy: CATEGORY_TO_FILTER_BY[category],
                                            recentProjects:
                                                category === categorySwitcher.Category.recent,
                                            labels: null,
                                        },
                                        entry.item.title
                                    )
                                    .then(assets => {
                                        setAssetTree(oldTree => {
                                            let found = signal.aborted
                                            const newTree = signal.aborted
                                                ? oldTree
                                                : oldTree.map(oldAsset => {
                                                      if (oldAsset.key === entry.key) {
                                                          found = true
                                                          return withChildren(oldAsset)
                                                      } else {
                                                          return oldAsset
                                                      }
                                                  })
                                            if (!found) {
                                                queuedDirectoryListings.set(entry.key, assets)
                                            }
                                            return newTree
                                        })
                                    })
                            }
                        }
                        const newAssets = await backend.listDirectory(
                            {
                                parentId: null,
                                filterBy: CATEGORY_TO_FILTER_BY[category],
                                recentProjects: category === categorySwitcher.Category.recent,
                                labels: null,
                            },
                            null
                        )
                        if (!signal.aborted) {
                            setIsLoading(false)
                            overwriteNodes(newAssets)
                        }
                    } else {
                        setIsLoading(false)
                    }
                    break
                }
            }
        },
        [category, accessToken, organization, backend]
    )

    React.useEffect(() => {
        const savedExtraColumns = localStorage.get(localStorageModule.LocalStorageKey.extraColumns)
        if (savedExtraColumns != null) {
            setExtraColumns(new Set(savedExtraColumns))
        }
    }, [/* should never change */ localStorage])

    // Clip the header bar so that the background behind the extra colums selector is visible.
    React.useEffect(() => {
        const headerRow = headerRowRef.current
        const scrollContainer = scrollContainerRef.current
        if (
            backend.type === backendModule.BackendType.remote &&
            headerRow != null &&
            scrollContainer != null
        ) {
            let isClipPathUpdateQueued = false
            const updateClipPath = () => {
                isClipPathUpdateQueued = false
                const rightOffset = `${
                    scrollContainer.clientWidth +
                    scrollContainer.scrollLeft -
                    TABLE_HEADER_WIDTH_SHRINKAGE_PX
                }px`
                headerRow.style.clipPath = `polygon(0 0, ${rightOffset} 0, ${rightOffset} 100%, 0 100%)`
            }
            const onScroll = () => {
                if (!isClipPathUpdateQueued) {
                    isClipPathUpdateQueued = true
                    requestAnimationFrame(updateClipPath)
                }
            }
            updateClipPath()
            const observer = new ResizeObserver(onScroll)
            observer.observe(scrollContainer)
            scrollContainer.addEventListener('scroll', onScroll)
            return () => {
                observer.unobserve(scrollContainer)
                scrollContainer.removeEventListener('scroll', onScroll)
            }
        } else {
            return
        }
    }, [backend.type])

    React.useEffect(() => {
        if (initialized) {
            localStorage.set(
                localStorageModule.LocalStorageKey.extraColumns,
                Array.from(extraColumns)
            )
        }
    }, [extraColumns, initialized, /* should never change */ localStorage])

    React.useEffect(() => {
        if (selectedKeys.size !== 1) {
            setAssetSettingsPanelProps(null)
        }
    }, [selectedKeys.size, /* should never change */ setAssetSettingsPanelProps])

    const directoryListAbortControllersRef = React.useRef(
        new Map<backendModule.DirectoryId, AbortController>()
    )
    const doToggleDirectoryExpansion = React.useCallback(
        (
            directoryId: backendModule.DirectoryId,
            key: backendModule.AssetId,
            title?: string | null,
            override?: boolean
        ) => {
            const directory = nodeMapRef.current.get(key)
            const isExpanded = directory?.children != null
            const shouldExpand = override ?? !isExpanded
            if (shouldExpand === isExpanded) {
                // This is fine, as this is near the top of a very long function.
                // eslint-disable-next-line no-restricted-syntax
                return
            }
            if (!shouldExpand) {
                const abortController = directoryListAbortControllersRef.current.get(directoryId)
                if (abortController != null) {
                    abortController.abort()
                    directoryListAbortControllersRef.current.delete(directoryId)
                }
                setAssetTree(oldAssetTree =>
                    oldAssetTree.map(item =>
                        item.key !== key ? item : item.with({ children: null })
                    )
                )
            } else {
                setAssetTree(oldAssetTree =>
                    oldAssetTree.map(item =>
                        item.key !== key
                            ? item
                            : item.with({
                                  children: [
                                      assetTreeNode.AssetTreeNode.fromAsset(
                                          backendModule.createSpecialLoadingAsset(directoryId),
                                          key,
                                          directoryId,
                                          item.depth + 1
                                      ),
                                  ],
                              })
                    )
                )
                void (async () => {
                    const abortController = new AbortController()
                    directoryListAbortControllersRef.current.set(directoryId, abortController)
                    const childAssets = await backend.listDirectory(
                        {
                            parentId: directoryId,
                            filterBy: CATEGORY_TO_FILTER_BY[category],
                            recentProjects: category === categorySwitcher.Category.recent,
                            labels: null,
                        },
                        title ?? null
                    )
                    if (!abortController.signal.aborted) {
                        setAssetTree(oldAssetTree =>
                            oldAssetTree.map(item => {
                                if (item.key !== key) {
                                    return item
                                } else {
                                    const initialChildren = item.children?.filter(
                                        child =>
                                            child.item.type !==
                                            backendModule.AssetType.specialLoading
                                    )
                                    const childAssetsMap = new Map(
                                        childAssets.map(asset => [asset.id, asset])
                                    )
                                    for (const child of initialChildren ?? []) {
                                        const newChild = childAssetsMap.get(child.item.id)
                                        if (newChild != null) {
                                            child.item = newChild
                                            childAssetsMap.delete(child.item.id)
                                        }
                                    }
                                    const childAssetNodes = Array.from(
                                        childAssetsMap.values(),
                                        child =>
                                            assetTreeNode.AssetTreeNode.fromAsset(
                                                child,
                                                key,
                                                directoryId,
                                                item.depth + 1
                                            )
                                    )
                                    const specialEmptyAsset: backendModule.SpecialEmptyAsset | null =
                                        (initialChildren != null && initialChildren.length !== 0) ||
                                        childAssetNodes.length !== 0
                                            ? null
                                            : backendModule.createSpecialEmptyAsset(directoryId)
                                    const children =
                                        specialEmptyAsset != null
                                            ? [
                                                  assetTreeNode.AssetTreeNode.fromAsset(
                                                      specialEmptyAsset,
                                                      key,
                                                      directoryId,
                                                      item.depth + 1
                                                  ),
                                              ]
                                            : initialChildren == null ||
                                              initialChildren.length === 0
                                            ? childAssetNodes
                                            : [...initialChildren, ...childAssetNodes].sort(
                                                  assetTreeNode.AssetTreeNode.compare
                                              )
                                    return item.with({ children })
                                }
                            })
                        )
                    }
                })()
            }
        },
        [category, backend]
    )

    const getNewProjectName = React.useCallback(
        (templateId: string | null, parentKey: backendModule.DirectoryId | null) => {
            const prefix = `${templateId ?? 'New_Project'}_`
            const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
            const siblings =
                parentKey == null
                    ? assetTree.children ?? []
                    : nodeMapRef.current.get(parentKey)?.children ?? []
            const projectIndices = siblings
                .map(node => node.item)
                .filter(backendModule.assetIsProject)
                .map(item => projectNameTemplate.exec(item.title)?.groups?.projectIndex)
                .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
            return `${prefix}${Math.max(0, ...projectIndices) + 1}`
        },
        [assetTree, nodeMapRef]
    )

    const deleteAsset = React.useCallback((key: backendModule.AssetId) => {
        setAssetTree(oldAssetTree => oldAssetTree.filter(item => item.key !== key))
    }, [])

    /** All items must have the same type. */
    const insertAssets = React.useCallback(
        (
            assets: backendModule.AnyAsset[],
            parentKey: backendModule.AssetId | null,
            parentId: backendModule.DirectoryId | null
        ) => {
            const actualParentKey = parentKey ?? rootDirectoryId
            const actualParentId = parentId ?? rootDirectoryId
            setAssetTree(oldAssetTree => {
                return oldAssetTree.map(item =>
                    item.key !== actualParentKey
                        ? item
                        : insertAssetTreeNodeChildren(item, assets, actualParentKey, actualParentId)
                )
            })
        },
        [rootDirectoryId]
    )

    const insertArbitraryAssets = React.useCallback(
        (
            assets: backendModule.AnyAsset[],
            parentKey: backendModule.AssetId | null,
            parentId: backendModule.DirectoryId | null,
            getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
        ) => {
            const actualParentKey = parentKey ?? rootDirectoryId
            const actualParentId = parentId ?? rootDirectoryId
            setAssetTree(oldAssetTree => {
                return oldAssetTree.map(item =>
                    item.key !== actualParentKey
                        ? item
                        : insertArbitraryAssetTreeNodeChildren(
                              item,
                              assets,
                              actualParentKey,
                              actualParentId,
                              getKey
                          )
                )
            })
        },
        [rootDirectoryId]
    )

    hooks.useEventHandler(assetListEvents, event => {
        switch (event.type) {
            case assetListEventModule.AssetListEventType.newFolder: {
                const siblings = nodeMapRef.current.get(event.parentKey)?.children ?? []
                const directoryIndices = siblings
                    .map(node => node.item)
                    .filter(backendModule.assetIsDirectory)
                    .map(item => DIRECTORY_NAME_REGEX.exec(item.title))
                    .map(match => match?.groups?.directoryIndex)
                    .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
                const title = `${DIRECTORY_NAME_DEFAULT_PREFIX}${
                    Math.max(0, ...directoryIndices) + 1
                }`
                const placeholderItem: backendModule.DirectoryAsset = {
                    type: backendModule.AssetType.directory,
                    id: backendModule.DirectoryId(uniqueString.uniqueString()),
                    title,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId,
                    permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                    projectState: null,
                    labels: [],
                    description: null,
                }
                if (nodeMapRef.current.get(event.parentKey)?.children == null) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                insertAssets([placeholderItem], event.parentKey, event.parentId)
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
                    type: backendModule.AssetType.project,
                    id: dummyId,
                    title: projectName,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId,
                    permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                    projectState: {
                        type: backendModule.ProjectState.placeholder,
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        volume_id: '',
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        ...(organization != null ? { opened_by: organization.email } : {}),
                    },
                    labels: [],
                    description: null,
                }
                if (nodeMapRef.current.get(event.parentKey)?.children == null) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                insertAssets([placeholderItem], event.parentKey, event.parentId)
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
                const placeholderFiles = reversedFiles.filter(backendModule.fileIsNotProject).map(
                    (file): backendModule.FileAsset => ({
                        type: backendModule.AssetType.file,
                        id: backendModule.FileId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId: event.parentId,
                        permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: null,
                        labels: [],
                        description: null,
                    })
                )
                const placeholderProjects = reversedFiles.filter(backendModule.fileIsProject).map(
                    (file): backendModule.ProjectAsset => ({
                        type: backendModule.AssetType.project,
                        id: backendModule.ProjectId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId: event.parentId,
                        permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: {
                            type: backendModule.ProjectState.new,
                            // eslint-disable-next-line @typescript-eslint/naming-convention
                            volume_id: '',
                            // eslint-disable-next-line @typescript-eslint/naming-convention
                            ...(organization != null ? { opened_by: organization.email } : {}),
                        },
                        labels: [],
                        description: null,
                    })
                )
                if (nodeMapRef.current.get(event.parentKey)?.children == null) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                insertAssets(placeholderFiles, event.parentKey, event.parentId)
                insertAssets(placeholderProjects, event.parentKey, event.parentId)
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
            case assetListEventModule.AssetListEventType.newDataConnector: {
                const placeholderItem: backendModule.SecretAsset = {
                    type: backendModule.AssetType.secret,
                    id: backendModule.SecretId(uniqueString.uniqueString()),
                    title: event.name,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId,
                    permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                    projectState: null,
                    labels: [],
                    description: null,
                }
                if (nodeMapRef.current.get(event.parentKey)?.children == null) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                insertAssets([placeholderItem], event.parentKey, event.parentId)
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.newDataConnector,
                    placeholderId: placeholderItem.id,
                    value: event.value,
                })
                break
            }
            case assetListEventModule.AssetListEventType.willDelete: {
                if (selectedKeys.has(event.key)) {
                    setSelectedKeys(oldSelectedKeys => {
                        const newSelectedKeys = new Set(oldSelectedKeys)
                        newSelectedKeys.delete(event.key)
                        return newSelectedKeys
                    })
                }
                break
            }
            case assetListEventModule.AssetListEventType.copy: {
                const ids = new Set<backendModule.AssetId>()
                const getKey = (asset: backendModule.AnyAsset) => {
                    const newId = backendModule.createPlaceholderAssetId(asset.type)
                    ids.add(newId)
                    return newId
                }
                insertArbitraryAssets(event.items, event.newParentKey, event.newParentId, getKey)
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.copy,
                    ids,
                    newParentKey: event.newParentKey,
                    newParentId: event.newParentId,
                })
                break
            }
            case assetListEventModule.AssetListEventType.move: {
                deleteAsset(event.key)
                insertAssets([event.item], event.newParentKey, event.newParentId)
                break
            }
            case assetListEventModule.AssetListEventType.delete: {
                deleteAsset(event.key)
                break
            }
            case assetListEventModule.AssetListEventType.removeSelf: {
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.removeSelf,
                    id: event.id,
                })
                break
            }
            case assetListEventModule.AssetListEventType.closeFolder: {
                if (nodeMapRef.current.get(event.key)?.children != null) {
                    doToggleDirectoryExpansion(event.id, event.key)
                }
                break
            }
        }
    })

    const doOpenManually = React.useCallback(
        (projectId: backendModule.ProjectId) => {
            dispatchAssetEvent({
                type: assetEventModule.AssetEventType.openProject,
                id: projectId,
                shouldAutomaticallySwitchPage: true,
                runInBackground: false,
            })
        },
        [/* should never change */ dispatchAssetEvent]
    )

    const doCloseIde = React.useCallback(
        (project: backendModule.ProjectAsset) => {
            if (project.id === projectStartupInfo?.projectAsset.id) {
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.cancelOpeningAllProjects,
                })
                rawDoCloseIde(project)
            }
        },
        [projectStartupInfo, rawDoCloseIde, /* should never change */ dispatchAssetEvent]
    )

    const doCopy = React.useCallback(() => {
        unsetModal()
        setPasteData({ type: pasteDataModule.PasteType.copy, data: selectedKeys })
    }, [selectedKeys, /* should never change */ unsetModal])

    const doCut = React.useCallback(() => {
        unsetModal()
        setSelectedKeys(oldSelectedKeys => {
            queueMicrotask(() => {
                if (pasteData != null) {
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.cancelCut,
                        ids: pasteData.data,
                    })
                }
                setPasteData({ type: pasteDataModule.PasteType.move, data: oldSelectedKeys })
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.cut,
                    ids: oldSelectedKeys,
                })
            })
            return new Set()
        })
    }, [
        pasteData,
        /* should never change */ unsetModal,
        /* should never change */ dispatchAssetEvent,
    ])

    const doPaste = React.useCallback(
        (newParentKey: backendModule.AssetId, newParentId: backendModule.DirectoryId) => {
            unsetModal()
            if (pasteData != null) {
                if (pasteData.data.has(newParentKey)) {
                    toast.toast.error('Cannot paste a folder into itself.')
                } else {
                    if (pasteData.type === pasteDataModule.PasteType.copy) {
                        const assets = Array.from(pasteData.data, id =>
                            nodeMapRef.current.get(id)
                        ).flatMap(asset => (asset ? [asset.item] : []))
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.copy,
                            items: assets,
                            newParentId,
                            newParentKey,
                        })
                    } else {
                        dispatchAssetEvent({
                            type: assetEventModule.AssetEventType.move,
                            ids: pasteData.data,
                            newParentKey,
                            newParentId,
                        })
                    }
                    setPasteData(null)
                }
            }
        },
        [
            pasteData,
            /* should never change */ unsetModal,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    const doRenderContextMenu = React.useCallback(
        (
            innerSelectedKeys: Set<backendModule.AssetId>,
            event: Pick<React.MouseEvent<Element, MouseEvent>, 'pageX' | 'pageY'>,
            innerSetSelectedKeys: (items: Set<backendModule.AssetId>) => void,
            hidden: boolean
        ) => {
            const pluralized = pluralize(innerSelectedKeys.size)
            // This works because all items are mutated, ensuring their value stays
            // up to date.
            const ownsAllSelectedAssets =
                isCloud ||
                (organization != null &&
                    Array.from(innerSelectedKeys, key => {
                        const userPermissions = nodeMapRef.current.get(key)?.item.permissions
                        const selfPermission = userPermissions?.find(
                            permission => permission.user.user_email === organization.email
                        )
                        return selfPermission?.permission === permissions.PermissionAction.own
                    }).every(isOwner => isOwner))
            // This is not a React component even though it contains JSX.
            // eslint-disable-next-line no-restricted-syntax
            const doDeleteAll = () => {
                if (isCloud) {
                    unsetModal()
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.delete,
                        ids: innerSelectedKeys,
                    })
                } else {
                    setModal(
                        <ConfirmDeleteModal
                            description={`${innerSelectedKeys.size} selected ${pluralized}`}
                            doDelete={() => {
                                innerSetSelectedKeys(new Set())
                                dispatchAssetEvent({
                                    type: assetEventModule.AssetEventType.delete,
                                    ids: innerSelectedKeys,
                                })
                            }}
                        />
                    )
                }
            }
            // This is not a React component even though it contains JSX.
            // eslint-disable-next-line no-restricted-syntax
            const doRestoreAll = () => {
                unsetModal()
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.restore,
                    ids: innerSelectedKeys,
                })
            }
            if (category === categorySwitcher.Category.trash) {
                return innerSelectedKeys.size === 0 ? (
                    <></>
                ) : (
                    <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
                        <ContextMenu hidden={hidden}>
                            <MenuEntry
                                hidden={hidden}
                                action={shortcutsModule.KeyboardAction.restoreAllFromTrash}
                                doAction={doRestoreAll}
                            />
                        </ContextMenu>
                    </ContextMenus>
                )
            } else if (category !== categorySwitcher.Category.home) {
                return null
            } else {
                const deleteAction = isCloud
                    ? shortcutsModule.KeyboardAction.moveAllToTrash
                    : shortcutsModule.KeyboardAction.deleteAll
                return (
                    <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
                        {innerSelectedKeys.size !== 0 && (
                            <ContextMenu hidden={hidden}>
                                {ownsAllSelectedAssets && (
                                    <MenuEntry
                                        hidden={hidden}
                                        action={deleteAction}
                                        doAction={doDeleteAll}
                                    />
                                )}
                                {isCloud && (
                                    <MenuEntry
                                        hidden={hidden}
                                        action={shortcutsModule.KeyboardAction.copyAll}
                                        doAction={doCopy}
                                    />
                                )}
                                {isCloud && ownsAllSelectedAssets && (
                                    <MenuEntry
                                        hidden={hidden}
                                        action={shortcutsModule.KeyboardAction.cutAll}
                                        doAction={doCut}
                                    />
                                )}
                                {pasteData != null && pasteData.data.size > 0 && (
                                    <MenuEntry
                                        hidden={hidden}
                                        action={shortcutsModule.KeyboardAction.pasteAll}
                                        doAction={() => {
                                            doPaste(rootDirectoryId, rootDirectoryId)
                                        }}
                                    />
                                )}
                            </ContextMenu>
                        )}
                        <GlobalContextMenu
                            hidden={hidden}
                            hasCopyData={pasteData != null}
                            directoryKey={null}
                            directoryId={null}
                            dispatchAssetListEvent={dispatchAssetListEvent}
                            doPaste={doPaste}
                        />
                    </ContextMenus>
                )
            }
        },
        [
            isCloud,
            category,
            pasteData,
            doCopy,
            doCut,
            doPaste,
            organization,
            rootDirectoryId,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ dispatchAssetListEvent,
            /* should never change */ setModal,
            /* should never change */ unsetModal,
        ]
    )

    const hiddenContextMenu = React.useMemo(
        () => doRenderContextMenu(selectedKeys, { pageX: 0, pageY: 0 }, setSelectedKeys, true),
        [doRenderContextMenu, selectedKeys]
    )

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): AssetsTableState => ({
            numberOfSelectedItems: selectedKeys.size,
            category,
            labels: allLabels,
            deletedLabelNames,
            hasPasteData: pasteData != null,
            setPasteData,
            sortColumn,
            setSortColumn,
            sortDirection,
            setSortDirection,
            query,
            setQuery,
            assetEvents,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            setAssetSettingsPanelProps,
            nodeMap: nodeMapRef,
            doToggleDirectoryExpansion,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            doCreateLabel,
            doCopy,
            doCut,
            doPaste,
        }),
        [
            selectedKeys.size,
            category,
            allLabels,
            deletedLabelNames,
            pasteData,
            sortColumn,
            sortDirection,
            assetEvents,
            query,
            doToggleDirectoryExpansion,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            doCreateLabel,
            doCopy,
            doCut,
            doPaste,
            /* should never change */ setAssetSettingsPanelProps,
            /* should never change */ setQuery,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    return (
        <div ref={scrollContainerRef} className="flex-1 overflow-auto">
            <div className="flex flex-col w-min min-w-full h-full">
                {backend.type !== backendModule.BackendType.local && (
                    <div className="sticky top-0 h-0">
                        <div className="block sticky right-0 ml-auto w-29 px-2 pt-2.25 pb-1.75 z-1">
                            <div className="inline-flex gap-3">
                                {columnModule.EXTRA_COLUMNS.map(column => (
                                    <Button
                                        key={column}
                                        active={extraColumns.has(column)}
                                        image={columnModule.EXTRA_COLUMN_IMAGES[column]}
                                        onClick={event => {
                                            event.stopPropagation()
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
                )}
                {hiddenContextMenu}
                <Table<
                    assetTreeNode.AssetTreeNode,
                    AssetsTableState,
                    AssetRowState,
                    backendModule.AssetId
                >
                    scrollContainerRef={scrollContainerRef}
                    headerRowRef={headerRowRef}
                    footer={
                        <div
                            className="grow"
                            onDragOver={event => {
                                const payload = drag.ASSET_ROWS.lookup(event)
                                const filtered = payload?.filter(
                                    item => item.asset.parentId !== rootDirectoryId
                                )
                                if (filtered != null && filtered.length > 0) {
                                    event.preventDefault()
                                }
                            }}
                            onDrop={event => {
                                const payload = drag.ASSET_ROWS.lookup(event)
                                const filtered = payload?.filter(
                                    item => item.asset.parentId !== rootDirectoryId
                                )
                                if (filtered != null && filtered.length > 0) {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    unsetModal()
                                    dispatchAssetEvent({
                                        type: assetEventModule.AssetEventType.move,
                                        newParentKey: rootDirectoryId,
                                        newParentId: rootDirectoryId,
                                        ids: new Set(filtered.map(dragItem => dragItem.asset.id)),
                                    })
                                }
                            }}
                        />
                    }
                    rowComponent={AssetRow}
                    items={displayItems}
                    filter={filter}
                    isLoading={isLoading}
                    state={state}
                    initialRowState={INITIAL_ROW_STATE}
                    getKey={assetTreeNode.AssetTreeNode.getKey}
                    selectedKeys={selectedKeys}
                    setSelectedKeys={setSelectedKeys}
                    placeholder={
                        category === categorySwitcher.Category.trash
                            ? TRASH_PLACEHOLDER
                            : query.query !== ''
                            ? QUERY_PLACEHOLDER
                            : PLACEHOLDER
                    }
                    columns={columnModule.getColumnList(backend.type, extraColumns).map(column => ({
                        id: column,
                        className: columnModule.COLUMN_CSS_CLASS[column],
                        heading: columnModule.COLUMN_HEADING[column],
                        render: columnModule.COLUMN_RENDERER[column],
                    }))}
                    onContextMenu={(innerSelectedKeys, event, innerSetSelectedKeys) => {
                        event.preventDefault()
                        event.stopPropagation()
                        const modal = doRenderContextMenu(
                            innerSelectedKeys,
                            event,
                            innerSetSelectedKeys,
                            false
                        )
                        if (modal != null) {
                            setModal(modal)
                        }
                    }}
                    draggableRows
                    onRowDragStart={event => {
                        setSelectedKeys(oldSelectedKeys => {
                            const nodes = assetTree
                                .preorderTraversal()
                                .filter(node => oldSelectedKeys.has(node.key))
                            const payload: drag.AssetRowsDragPayload = nodes.map(node => ({
                                key: node.key,
                                asset: node.item,
                            }))
                            drag.setDragImageToBlank(event)
                            drag.ASSET_ROWS.bind(event, payload)
                            queueMicrotask(() => {
                                setModal(
                                    <DragModal
                                        event={event}
                                        className="flex flex-col rounded-2xl bg-frame-selected backdrop-blur-3xl"
                                        doCleanup={() => {
                                            drag.ASSET_ROWS.unbind(payload)
                                        }}
                                    >
                                        {nodes.map(node => (
                                            <AssetNameColumn
                                                key={node.key}
                                                keyProp={node.key}
                                                item={node.with({ depth: 0 })}
                                                state={state}
                                                // Default states.
                                                isSoleSelectedItem={false}
                                                selected={false}
                                                rowState={INITIAL_ROW_STATE}
                                                // The drag placeholder cannot be interacted with.
                                                setSelected={() => {}}
                                                setItem={() => {}}
                                                setRowState={() => {}}
                                            />
                                        ))}
                                    </DragModal>
                                )
                            })
                            return oldSelectedKeys
                        })
                    }}
                    onRowDragOver={(event, _, key) => {
                        setSelectedKeys(oldSelectedKeys => {
                            const payload = drag.LABELS.lookup(event)
                            if (payload != null) {
                                event.preventDefault()
                                event.stopPropagation()
                                const ids = oldSelectedKeys.has(key)
                                    ? oldSelectedKeys
                                    : new Set([key])
                                let labelsPresent = 0
                                for (const selectedKey of ids) {
                                    const labels = nodeMapRef.current.get(selectedKey)?.item.labels
                                    if (labels != null) {
                                        for (const label of labels) {
                                            if (payload.has(label)) {
                                                labelsPresent += 1
                                            }
                                        }
                                    }
                                }
                                const shouldAdd = labelsPresent * 2 < ids.size * payload.size
                                window.setTimeout(() => {
                                    dispatchAssetEvent({
                                        type: shouldAdd
                                            ? assetEventModule.AssetEventType.temporarilyAddLabels
                                            : assetEventModule.AssetEventType
                                                  .temporarilyRemoveLabels,
                                        ids,
                                        labelNames: payload,
                                    })
                                })
                            }
                            return oldSelectedKeys
                        })
                    }}
                    onRowDragEnd={() => {
                        setSelectedKeys(oldSelectedKeys => {
                            window.setTimeout(() => {
                                dispatchAssetEvent({
                                    type: assetEventModule.AssetEventType.temporarilyAddLabels,
                                    ids: oldSelectedKeys,
                                    labelNames: set.EMPTY,
                                })
                            })
                            return oldSelectedKeys
                        })
                    }}
                    onRowDrop={(event, _, key) => {
                        setSelectedKeys(oldSelectedKeys => {
                            const ids = oldSelectedKeys.has(key) ? oldSelectedKeys : new Set([key])
                            const payload = drag.LABELS.lookup(event)
                            if (payload != null) {
                                event.preventDefault()
                                event.stopPropagation()
                                let labelsPresent = 0
                                for (const selectedKey of ids) {
                                    const labels = nodeMapRef.current.get(selectedKey)?.item.labels
                                    if (labels != null) {
                                        for (const label of labels) {
                                            if (payload.has(label)) {
                                                labelsPresent += 1
                                            }
                                        }
                                    }
                                }
                                const shouldAdd = labelsPresent * 2 < ids.size * payload.size
                                window.setTimeout(() => {
                                    dispatchAssetEvent({
                                        type: shouldAdd
                                            ? assetEventModule.AssetEventType.addLabels
                                            : assetEventModule.AssetEventType.removeLabels,
                                        ids,
                                        labelNames: payload,
                                    })
                                })
                            } else {
                                window.setTimeout(() => {
                                    dispatchAssetEvent({
                                        type: assetEventModule.AssetEventType.temporarilyAddLabels,
                                        ids,
                                        labelNames: set.EMPTY,
                                    })
                                })
                            }
                            return oldSelectedKeys
                        })
                    }}
                    onDragLeave={event => {
                        const payload = drag.LABELS.lookup(event)
                        if (
                            payload != null &&
                            event.relatedTarget instanceof Node &&
                            !event.currentTarget.contains(event.relatedTarget)
                        ) {
                            setSelectedKeys(oldSelectedKeys => {
                                window.setTimeout(() => {
                                    dispatchAssetEvent({
                                        type: assetEventModule.AssetEventType.temporarilyAddLabels,
                                        ids: oldSelectedKeys,
                                        labelNames: set.EMPTY,
                                    })
                                })
                                return oldSelectedKeys
                            })
                        }
                    }}
                />
            </div>
        </div>
    )
}
