/** @file Table displaying a list of projects. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import * as array from '../array'
import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as assetTreeNode from '../assetTreeNode'
import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as hooks from '../../hooks'
import * as localStorageModule from '../localStorage'
import * as localStorageProvider from '../../providers/localStorage'
import * as permissions from '../permissions'
import * as presenceModule from '../presence'
import * as shortcuts from '../shortcuts'
import * as sorting from '../sorting'
import * as string from '../../string'
import * as style from '../style'
import * as uniqueString from '../../uniqueString'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import AssetRow from './assetRow'
import Button from './button'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenus from './contextMenus'
import GlobalContextMenu from './globalContextMenu'
import MenuEntry from './menuEntry'
import Table from './table'

// =================
// === Constants ===
// =================

/** The number of pixels the header bar should shrink when the extra tab selector is visible. */
const TABLE_HEADER_WIDTH_SHRINKAGE_PX = 274
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

// ===================================
// === insertAssetTreeNodeChildren ===
// ===================================

/** Return a list of nodes, plus new nodes created from a list of assets.
 * The list of children MUST be all of one specific asset type. */
function insertChildrenIntoAssetTreeNodeArray(
    nodes: assetTreeNode.AssetTreeNode[],
    children: backendModule.AnyAsset[],
    depth: number
) {
    const typeOrder = children[0] != null ? backendModule.ASSET_TYPE_ORDER[children[0].type] : 0
    return array.splicedBefore(
        nodes.filter(node => node.item.type !== backendModule.AssetType.specialEmpty),
        children.map(asset => assetTreeNode.assetTreeNodeFromAsset(asset, depth)),
        innerItem => backendModule.ASSET_TYPE_ORDER[innerItem.item.type] >= typeOrder
    )
}

/** Return a directory, with new children added into its list of children.
 * The list of children MUST be all of one specific asset type. */
function insertAssetTreeNodeChildren(
    item: assetTreeNode.AssetTreeNode,
    children: backendModule.AnyAsset[]
): assetTreeNode.AssetTreeNode {
    const newDepth = item.depth + 1
    return {
        ...item,
        children: insertChildrenIntoAssetTreeNodeArray(
            (item.children ?? []).filter(
                node => node.item.type !== backendModule.AssetType.specialEmpty
            ),
            children,
            newDepth
        ),
    }
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
    sortColumn: columnModule.SortableColumn | null
    setSortColumn: (column: columnModule.SortableColumn | null) => void
    sortDirection: sorting.SortDirection | null
    setSortDirection: (sortDirection: sorting.SortDirection | null) => void
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    doToggleDirectoryExpansion: (
        directoryId: backendModule.DirectoryId,
        key: backendModule.AssetId,
        title?: string
    ) => void
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (
        project: backendModule.ProjectAsset,
        setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
        switchPage: boolean
    ) => void
    doCloseIde: (project: backendModule.ProjectAsset) => void
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
    query: string
    initialProjectName: string | null
    projectStartupInfo: backendModule.ProjectStartupInfo | null
    /** These events will be dispatched the next time the assets list is refreshed, rather than
     * immediately. */
    queuedAssetEvents: assetEventModule.AssetEvent[]
    assetListEvents: assetListEventModule.AssetListEvent[]
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    doOpenIde: (
        project: backendModule.ProjectAsset,
        setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
        switchPage: boolean
    ) => void
    doCloseIde: (project: backendModule.ProjectAsset) => void
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
    const {
        query,
        initialProjectName,
        projectStartupInfo,
        queuedAssetEvents: rawQueuedAssetEvents,
        assetListEvents,
        dispatchAssetListEvent,
        assetEvents,
        dispatchAssetEvent,
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
    const { localStorage } = localStorageProvider.useLocalStorage()
    const [initialized, setInitialized] = React.useState(false)
    const [assetTree, setAssetTree] = React.useState<assetTreeNode.AssetTreeNode[]>([])
    const [isLoading, setIsLoading] = React.useState(true)
    const [extraColumns, setExtraColumns] = React.useState(
        () => new Set<columnModule.ExtraColumn>()
    )
    const [sortColumn, setSortColumn] = React.useState<columnModule.SortableColumn | null>(null)
    const [sortDirection, setSortDirection] = React.useState<sorting.SortDirection | null>(null)
    const [selectedKeys, setSelectedKeys] = React.useState(() => new Set<backendModule.AssetId>())
    const scrollContainerRef = React.useRef<HTMLDivElement>(null)
    const headerRowRef = React.useRef<HTMLTableRowElement>(null)
    const [, setQueuedAssetEvents] = React.useState<assetEventModule.AssetEvent[]>([])
    const [nameOfProjectToImmediatelyOpen, setNameOfProjectToImmediatelyOpen] =
        React.useState(initialProjectName)
    const nodeMap = React.useMemo(
        () =>
            new Map(
                assetTreeNode.assetTreePreorderTraversal(assetTree).map(asset => [asset.key, asset])
            ),
        [assetTree]
    )
    const filter = React.useMemo(() => {
        if (query === '') {
            return null
        } else {
            const regex = new RegExp(string.regexEscape(query), 'i')
            return (node: assetTreeNode.AssetTreeNode) => regex.test(node.item.title)
        }
    }, [query])
    const displayItems = React.useMemo(() => {
        if (sortColumn == null || sortDirection == null) {
            return assetTreeNode.assetTreePreorderTraversal(assetTree)
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
            return assetTreeNode.assetTreePreorderTraversal(assetTree, tree =>
                Array.from(tree).sort(compare)
            )
        }
    }, [assetTree, sortColumn, sortDirection])

    React.useEffect(() => {
        if (rawQueuedAssetEvents.length !== 0) {
            setQueuedAssetEvents(oldEvents => [...oldEvents, ...rawQueuedAssetEvents])
        }
    }, [rawQueuedAssetEvents])

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
            setAssetTree(
                newAssets.map(asset => ({
                    key: asset.id,
                    item: asset,
                    children: null,
                    depth: 0,
                }))
            )
            // The project name here might also be a string with project id, e.g. when opening
            // a project file from explorer on Windows.
            const isInitialProject = (asset: backendModule.AnyAsset) =>
                asset.title === initialProjectName || asset.id === initialProjectName
            if (nameOfProjectToImmediatelyOpen != null) {
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
                }
                setNameOfProjectToImmediatelyOpen(null)
            }
            setQueuedAssetEvents(oldQueuedAssetEvents => {
                if (oldQueuedAssetEvents.length !== 0) {
                    window.setTimeout(() => {
                        for (const event of oldQueuedAssetEvents) {
                            dispatchAssetEvent(event)
                        }
                    }, 0)
                }
                return []
            })
            if (!initialized) {
                setInitialized(true)
                if (initialProjectName != null) {
                    if (!newAssets.some(isInitialProject)) {
                        const errorMessage = `No project named '${initialProjectName}' was found.`
                        toastify.toast.error(errorMessage)
                        logger.error(`Error opening project on startup: ${errorMessage}`)
                    }
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
        if (initialized) {
            overwriteAssets([])
        }
        // `overwriteAssets` is a callback, not a dependency.
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
                const hasVerticalScrollbar =
                    scrollContainer.scrollHeight > scrollContainer.clientHeight
                const shrinkage =
                    TABLE_HEADER_WIDTH_SHRINKAGE_PX +
                    (hasVerticalScrollbar ? style.SCROLLBAR_WIDTH_PX : 0)
                const rightOffset = `calc(100vw - ${shrinkage}px + ${scrollContainer.scrollLeft}px)`
                headerRow.style.clipPath = `polygon(0 0, ${rightOffset} 0, ${rightOffset} 100%, 0 100%)`
            }
            const onScroll = () => {
                if (!isClipPathUpdateQueued) {
                    isClipPathUpdateQueued = true
                    requestAnimationFrame(updateClipPath)
                }
            }
            updateClipPath()
            scrollContainer.addEventListener('scroll', onScroll)
            return () => {
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

    const directoryListAbortControllersRef = React.useRef(
        new Map<backendModule.DirectoryId, AbortController>()
    )
    const doToggleDirectoryExpansion = React.useCallback(
        (directoryId: backendModule.DirectoryId, key: backendModule.AssetId, title?: string) => {
            const directory = nodeMap.get(key)
            if (directory?.children != null) {
                const abortController = directoryListAbortControllersRef.current.get(directoryId)
                if (abortController != null) {
                    abortController.abort()
                    directoryListAbortControllersRef.current.delete(directoryId)
                }
                setAssetTree(oldAssetTree =>
                    assetTreeNode.assetTreeMap(oldAssetTree, item =>
                        item.key !== key ? item : { ...item, children: null }
                    )
                )
            } else {
                setAssetTree(oldAssetTree =>
                    assetTreeNode.assetTreeMap(oldAssetTree, item =>
                        item.key !== key
                            ? item
                            : {
                                  ...item,
                                  children: [
                                      assetTreeNode.assetTreeNodeFromAsset(
                                          backendModule.createSpecialLoadingAsset(directoryId),
                                          item.depth + 1
                                      ),
                                  ],
                              }
                    )
                )
                void (async () => {
                    const abortController = new AbortController()
                    directoryListAbortControllersRef.current.set(directoryId, abortController)
                    const childAssets = await backend.listDirectory(
                        { parentId: directoryId },
                        title ?? null
                    )
                    if (!abortController.signal.aborted) {
                        setAssetTree(oldAssetTree =>
                            assetTreeNode.assetTreeMap(oldAssetTree, item => {
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
                                            assetTreeNode.assetTreeNodeFromAsset(
                                                child,
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
                                                  assetTreeNode.assetTreeNodeFromAsset(
                                                      specialEmptyAsset,
                                                      item.depth + 1
                                                  ),
                                              ]
                                            : initialChildren == null ||
                                              initialChildren.length === 0
                                            ? childAssetNodes
                                            : [...initialChildren, ...childAssetNodes].sort(
                                                  assetTreeNode.compareAssetTreeNodes
                                              )
                                    return {
                                        ...item,
                                        children,
                                    }
                                }
                            })
                        )
                    }
                })()
            }
        },
        [nodeMap, backend]
    )

    const getNewProjectName = React.useCallback(
        (templateId: string | null, parentKey: backendModule.DirectoryId | null) => {
            const prefix = `${templateId ?? 'New_Project'}_`
            const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
            const siblings = parentKey == null ? assetTree : nodeMap.get(parentKey)?.children ?? []
            const projectIndices = siblings
                .filter(node => backendModule.assetIsProject(node.item))
                .map(node => projectNameTemplate.exec(node.item.title)?.groups?.projectIndex)
                .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
            return `${prefix}${Math.max(0, ...projectIndices) + 1}`
        },
        [assetTree, nodeMap]
    )

    hooks.useEventHandler(assetListEvents, event => {
        switch (event.type) {
            case assetListEventModule.AssetListEventType.newFolder: {
                const siblings =
                    event.parentKey == null
                        ? assetTree
                        : nodeMap.get(event.parentKey)?.children ?? []
                const directoryIndices = siblings
                    .filter(node => backendModule.assetIsDirectory(node.item))
                    .map(node => DIRECTORY_NAME_REGEX.exec(node.item.title))
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
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    nodeMap.get(event.parentKey)?.children == null
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssetTree(oldAssetTree =>
                    event.parentKey == null
                        ? insertChildrenIntoAssetTreeNodeArray(oldAssetTree, [placeholderItem], 0)
                        : assetTreeNode.assetTreeMap(oldAssetTree, item =>
                              item.key !== event.parentKey
                                  ? item
                                  : insertAssetTreeNodeChildren(item, [placeholderItem])
                          )
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
                    projectState: {
                        type: backendModule.ProjectState.placeholder,
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        volume_id: '',
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        ...(organization != null ? { opened_by: organization.email } : {}),
                    },
                    type: backendModule.AssetType.project,
                }
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    nodeMap.get(event.parentKey)?.children == null
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssetTree(oldAssetTree =>
                    event.parentKey == null
                        ? insertChildrenIntoAssetTreeNodeArray(oldAssetTree, [placeholderItem], 0)
                        : assetTreeNode.assetTreeMap(oldAssetTree, item =>
                              item.key !== event.parentKey
                                  ? item
                                  : insertAssetTreeNodeChildren(item, [placeholderItem])
                          )
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
                const placeholderFiles = reversedFiles.filter(backendModule.fileIsNotProject).map(
                    (file): backendModule.FileAsset => ({
                        type: backendModule.AssetType.file,
                        id: backendModule.FileId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId,
                        permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: null,
                    })
                )
                const placeholderProjects = reversedFiles.filter(backendModule.fileIsProject).map(
                    (file): backendModule.ProjectAsset => ({
                        type: backendModule.AssetType.project,
                        id: backendModule.ProjectId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId,
                        permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: {
                            type: backendModule.ProjectState.new,
                            // eslint-disable-next-line @typescript-eslint/naming-convention
                            volume_id: '',
                            // eslint-disable-next-line @typescript-eslint/naming-convention
                            ...(organization != null ? { opened_by: organization.email } : {}),
                        },
                    })
                )
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    nodeMap.get(event.parentKey)?.children == null
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssetTree(oldAssetTree =>
                    event.parentKey == null
                        ? insertChildrenIntoAssetTreeNodeArray(
                              insertChildrenIntoAssetTreeNodeArray(
                                  oldAssetTree,
                                  placeholderFiles,
                                  0
                              ),
                              placeholderProjects,
                              0
                          )
                        : assetTreeNode.assetTreeMap(oldAssetTree, item =>
                              item.key !== event.parentKey
                                  ? item
                                  : insertAssetTreeNodeChildren(
                                        insertAssetTreeNodeChildren(item, placeholderFiles),
                                        placeholderProjects
                                    )
                          )
                )
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
                if (
                    event.parentId != null &&
                    event.parentKey != null &&
                    nodeMap.get(event.parentKey)?.children == null
                ) {
                    doToggleDirectoryExpansion(event.parentId, event.parentKey)
                }
                setAssetTree(oldAssetTree =>
                    event.parentKey == null
                        ? insertChildrenIntoAssetTreeNodeArray(oldAssetTree, [placeholderItem], 0)
                        : assetTreeNode.assetTreeMap(oldAssetTree, item =>
                              item.key !== event.parentKey
                                  ? item
                                  : insertAssetTreeNodeChildren(item, [placeholderItem])
                          )
                )
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.newSecret,
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
            case assetListEventModule.AssetListEventType.delete: {
                setAssetTree(oldAssetTree =>
                    assetTreeNode.assetTreeFilter(oldAssetTree, item => item.key !== event.key)
                )
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
                if (nodeMap.get(event.key)?.children != null) {
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

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): AssetsTableState => ({
            sortColumn,
            setSortColumn,
            sortDirection,
            setSortDirection,
            assetEvents,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            doToggleDirectoryExpansion,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        }),
        [
            sortColumn,
            sortDirection,
            assetEvents,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            doToggleDirectoryExpansion,
            /* should never change */ setSortColumn,
            /* should never change */ setSortDirection,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    return (
        <div ref={scrollContainerRef} className="flex-1 overflow-auto">
            <div className="flex flex-col w-min min-w-full h-full">
                {backend.type !== backendModule.BackendType.local && (
                    <div className="sticky top-0 h-0">
                        <div className="block sticky right-0 ml-auto w-29 p-2 z-1">
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
                )}
                <Table<
                    assetTreeNode.AssetTreeNode,
                    AssetsTableState,
                    AssetRowState,
                    backendModule.AssetId
                >
                    footer={<tfoot className="h-full"></tfoot>}
                    scrollContainerRef={scrollContainerRef}
                    headerRowRef={headerRowRef}
                    rowComponent={AssetRow}
                    items={displayItems}
                    filter={filter}
                    isLoading={isLoading}
                    state={state}
                    initialRowState={INITIAL_ROW_STATE}
                    getKey={assetTreeNode.getAssetTreeNodeKey}
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
                                        <MenuEntry
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
