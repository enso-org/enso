/** @file Table displaying a list of projects. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as eventHooks from '#/hooks/eventHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/dashboard/AssetPanel'
import type * as assetSearchBar from '#/layouts/dashboard/AssetSearchBar'
import AssetsTableContextMenu from '#/layouts/dashboard/AssetsTableContextMenu'
import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import DuplicateAssetsModal from '#/layouts/dashboard/DuplicateAssetsModal'

import Button from '#/components/Button'
import type * as assetRow from '#/components/dashboard/AssetRow'
import AssetRow from '#/components/dashboard/AssetRow'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import UninteractableNameColumn from '#/components/dashboard/column/UninteractableNameColumn'
import * as columnHeading from '#/components/dashboard/columnHeading'
import Label from '#/components/dashboard/Label'
import DragModal from '#/components/DragModal'
import Spinner, * as spinner from '#/components/Spinner'

import * as backendModule from '#/services/Backend'

import * as array from '#/utilities/array'
import type * as assetQuery from '#/utilities/AssetQuery'
import AssetQuery from '#/utilities/AssetQuery'
import AssetTreeNode from '#/utilities/AssetTreeNode'
import type * as color from '#/utilities/color'
import * as drag from '#/utilities/drag'
import * as fileInfo from '#/utilities/fileInfo'
import LocalStorage from '#/utilities/LocalStorage'
import type * as pasteDataModule from '#/utilities/pasteData'
import PasteType from '#/utilities/PasteType'
import * as permissions from '#/utilities/permissions'
import * as set from '#/utilities/set'
import * as shortcutManagerModule from '#/utilities/ShortcutManager'
import SortDirection from '#/utilities/SortDirection'
import Visibility from '#/utilities/visibility'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    extraColumns: columnUtils.ExtraColumn[]
  }
}

LocalStorage.registerKey('extraColumns', {
  tryParse: value => {
    const possibleColumns = Array.isArray(value) ? value : []
    const values = possibleColumns.filter(array.includesPredicate(columnUtils.EXTRA_COLUMNS))
    return values.length === 0 ? null : values
  },
})

// =================
// === Constants ===
// =================

/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE = 36
/** The number of pixels the header bar should shrink when the extra column selector is visible. */
const TABLE_HEADER_WIDTH_SHRINKAGE_PX = 116
/** The default placeholder row. */
const PLACEHOLDER = (
  <span className="opacity-75">
    You have no files. Go ahead and create one using the buttons above, or open a template from the
    home screen.
  </span>
)
/** A placeholder row for when a query (text or labels) is active. */
const QUERY_PLACEHOLDER = <span className="opacity-75">No files match the current filters.</span>
/** The placeholder row for the Trash category. */
const TRASH_PLACEHOLDER = <span className="opacity-75 px-1.5">Your trash is empty.</span>

const SUGGESTIONS_FOR_NO: assetSearchBar.Suggestion[] = [
  {
    render: () => 'no:label',
    addToQuery: query => query.addToLastTerm({ nos: ['label'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ nos: ['label'] }),
  },
  {
    render: () => 'no:description',
    addToQuery: query => query.addToLastTerm({ nos: ['description'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ nos: ['description'] }),
  },
]
const SUGGESTIONS_FOR_HAS: assetSearchBar.Suggestion[] = [
  {
    render: () => 'has:label',
    addToQuery: query => query.addToLastTerm({ negativeNos: ['label'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeNos: ['label'] }),
  },
  {
    render: () => 'has:description',
    addToQuery: query => query.addToLastTerm({ negativeNos: ['description'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeNos: ['description'] }),
  },
]
const SUGGESTIONS_FOR_TYPE: assetSearchBar.Suggestion[] = [
  {
    render: () => 'type:project',
    addToQuery: query => query.addToLastTerm({ types: ['project'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['project'] }),
  },
  {
    render: () => 'type:folder',
    addToQuery: query => query.addToLastTerm({ types: ['folder'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['folder'] }),
  },
  {
    render: () => 'type:file',
    addToQuery: query => query.addToLastTerm({ types: ['file'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['file'] }),
  },
  {
    render: () => 'type:connector',
    addToQuery: query => query.addToLastTerm({ types: ['connector'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['connector'] }),
  },
]
const SUGGESTIONS_FOR_NEGATIVE_TYPE: assetSearchBar.Suggestion[] = [
  {
    render: () => 'type:project',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['project'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['project'] }),
  },
  {
    render: () => 'type:folder',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['folder'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['folder'] }),
  },
  {
    render: () => 'type:file',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['file'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['file'] }),
  },
  {
    render: () => 'type:connector',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['connector'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['connector'] }),
  },
]

// =============================
// === Category to filter by ===
// =============================

const CATEGORY_TO_FILTER_BY: Record<Category, backendModule.FilterBy | null> = {
  [Category.recent]: null,
  [Category.home]: backendModule.FilterBy.active,
  [Category.trash]: backendModule.FilterBy.trashed,
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  isCloud: boolean
  numberOfSelectedItems: number
  category: Category
  labels: Map<backendModule.LabelName, backendModule.Label>
  deletedLabelNames: Set<backendModule.LabelName>
  hasPasteData: boolean
  sortColumn: columnUtils.SortableColumn | null
  setSortColumn: (column: columnUtils.SortableColumn | null) => void
  sortDirection: SortDirection | null
  setSortDirection: (sortDirection: SortDirection | null) => void
  query: AssetQuery
  setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  assetEvents: assetEvent.AssetEvent[]
  dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  setAssetPanelProps: React.Dispatch<
    React.SetStateAction<assetPanel.AssetPanelRequiredProps | null>
  >
  nodeMap: Readonly<React.MutableRefObject<ReadonlyMap<backendModule.AssetId, AssetTreeNode>>>
  doToggleDirectoryExpansion: (
    directory: backendModule.SmartDirectory,
    key: backendModule.AssetId,
    override?: boolean
  ) => void
  /** Called when the project is opened via the `ProjectActionButton`. */
  doOpenManually: (projectId: backendModule.ProjectId) => void
  doOpenEditor: (
    project: backendModule.SmartProject,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  doCloseEditor: (project: backendModule.ProjectAsset) => void
  doCreateLabel: (value: string, color: color.LChColor) => Promise<void>
  doCopy: () => void
  doCut: () => void
  doPaste: (newParentKey: backendModule.AssetId) => void
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
  setVisibility: (visibility: Visibility) => void
  isEditingName: boolean
  temporarilyAddedLabels: ReadonlySet<backendModule.LabelName>
  temporarilyRemovedLabels: ReadonlySet<backendModule.LabelName>
}

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
  isCloud: boolean
  rootDirectory: backendModule.SmartDirectory
  query: AssetQuery
  setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  setCanDownloadFiles: (canDownloadFiles: boolean) => void
  category: Category
  allLabels: Map<backendModule.LabelName, backendModule.Label>
  setSuggestions: (suggestions: assetSearchBar.Suggestion[]) => void
  initialProjectName: string | null
  projectStartupInfo: backendModule.ProjectStartupInfo | null
  deletedLabelNames: Set<backendModule.LabelName>
  assetListEvents: assetListEvent.AssetListEvent[]
  dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  assetEvents: assetEvent.AssetEvent[]
  dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  setAssetPanelProps: React.Dispatch<
    React.SetStateAction<assetPanel.AssetPanelRequiredProps | null>
  >
  doOpenEditor: (
    project: backendModule.SmartProject,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  doCloseEditor: (project: backendModule.ProjectAsset) => void
  doCreateLabel: (value: string, color: color.LChColor) => Promise<void>
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
  const { isCloud, rootDirectory, query, setQuery, setCanDownloadFiles, category } = props
  const { allLabels, deletedLabelNames, initialProjectName, projectStartupInfo } = props
  const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
  const { setAssetPanelProps, doOpenEditor, doCloseEditor: rawDoCloseEditor, doCreateLabel } = props
  const { setSuggestions } = props

  const { organization, user, accessToken } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { shortcutManager } = shortcutManagerProvider.useShortcutManager()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [initialized, setInitialized] = React.useState(false)
  const [isLoading, setIsLoading] = React.useState(true)
  const [extraColumns, setExtraColumns] = React.useState(() => new Set<columnUtils.ExtraColumn>())
  const [sortColumn, setSortColumn] = React.useState<columnUtils.SortableColumn | null>(null)
  const [sortDirection, setSortDirection] = React.useState<SortDirection | null>(null)
  const [selectedKeys, setSelectedKeys] = React.useState(() => new Set<backendModule.AssetId>())
  const [hasPasteData, setHasPasteData] = React.useState(false)
  const queuedProject = React.useRef(initialProjectName)
  const [assetTree, setAssetTree] = React.useState<AssetTreeNode>(() => {
    const rootParentDirectoryId = backendModule.DirectoryId('')
    return AssetTreeNode.fromSmartAsset(rootDirectory, rootParentDirectoryId, rootDirectory, -1)
  })
  const placeholder =
    category === Category.trash
      ? TRASH_PLACEHOLDER
      : query.query !== ''
      ? QUERY_PLACEHOLDER
      : PLACEHOLDER
  const scrollContainerRef = React.useRef<HTMLDivElement>(null)
  const headerRowRef = React.useRef<HTMLTableRowElement>(null)
  const pasteDataRef = React.useRef<pasteDataModule.PasteData<Set<backendModule.AssetId>> | null>(
    null
  )
  const nodeMapRef = React.useRef<ReadonlyMap<backendModule.AssetId, AssetTreeNode>>(
    new Map<backendModule.AssetId, AssetTreeNode>()
  )
  const filter = React.useMemo(
    () => (/^\s*$/.test(query.query) ? null : query.isMatch.bind(query)),
    [query]
  )
  const displayItems = React.useMemo(() => {
    if (sortColumn == null || sortDirection == null) {
      return assetTree.preorderTraversal()
    } else {
      const multiplier = {
        [SortDirection.ascending]: 1,
        [SortDirection.descending]: -1,
      }[sortDirection]
      let compare: (a: AssetTreeNode, b: AssetTreeNode) => number
      switch (sortColumn) {
        case columnUtils.Column.name: {
          compare = (a, b) => {
            const aTypeOrder = backendModule.ASSET_TYPE_ORDER[a.item.type]
            const bTypeOrder = backendModule.ASSET_TYPE_ORDER[b.item.type]
            const typeDelta = aTypeOrder - bTypeOrder
            const aTitle = a.item.value.title.toLowerCase()
            const bTitle = b.item.value.title.toLowerCase()
            if (typeDelta !== 0) {
              return typeDelta
            } else if (aTitle === bTitle) {
              const delta =
                a.item.value.title > b.item.value.title
                  ? 1
                  : a.item.value.title < b.item.value.title
                  ? -1
                  : 0
              return multiplier * delta
            } else {
              const delta = aTitle > bTitle ? 1 : aTitle < bTitle ? -1 : 0
              return multiplier * delta
            }
          }
          break
        }
        case columnUtils.Column.modified: {
          compare = (a, b) => {
            const aTypeOrder = backendModule.ASSET_TYPE_ORDER[a.item.type]
            const bTypeOrder = backendModule.ASSET_TYPE_ORDER[b.item.type]
            const typeDelta = aTypeOrder - bTypeOrder
            if (typeDelta !== 0) {
              return typeDelta
            } else {
              const aOrder = Number(new Date(a.item.value.modifiedAt))
              const bOrder = Number(new Date(b.item.value.modifiedAt))
              return multiplier * (aOrder - bOrder)
            }
          }
          break
        }
      }
      return assetTree.preorderTraversal(tree => [...tree].sort(compare))
    }
  }, [assetTree, sortColumn, sortDirection])
  const visibilities = React.useMemo(() => {
    const map = new Map<backendModule.AssetId, Visibility>()
    const processNode = (node: AssetTreeNode) => {
      let displayState = Visibility.hidden
      const visible = filter?.(node) ?? true
      for (const child of node.children ?? []) {
        if (visible && child.item.value.type === backendModule.AssetType.specialEmpty) {
          map.set(child.key, Visibility.visible)
        } else {
          processNode(child)
        }
        if (map.get(child.key) !== Visibility.hidden) {
          displayState = Visibility.faded
        }
      }
      if (visible) {
        displayState = Visibility.visible
      }
      map.set(node.key, displayState)
      return displayState
    }
    processNode(assetTree)
    return map
  }, [assetTree, filter])

  React.useEffect(() => {
    if (category === Category.trash) {
      setCanDownloadFiles(false)
    } else if (!isCloud) {
      setCanDownloadFiles(selectedKeys.size !== 0)
    } else {
      setCanDownloadFiles(
        selectedKeys.size !== 0 &&
          Array.from(selectedKeys).every(key => {
            const node = nodeMapRef.current.get(key)
            return node?.item.value.type === backendModule.AssetType.file
          })
      )
    }
  }, [category, selectedKeys, isCloud, /* should never change */ setCanDownloadFiles])

  React.useEffect(() => {
    const nodeToSuggestion = (
      node: AssetTreeNode,
      key: assetQuery.AssetQueryKey = 'names'
    ): assetSearchBar.Suggestion => ({
      render: () => `${key === 'names' ? '' : '-:'}${node.item.value.title}`,
      addToQuery: oldQuery => oldQuery.addToLastTerm({ [key]: [node.item.value.title] }),
      deleteFromQuery: oldQuery => oldQuery.deleteFromLastTerm({ [key]: [node.item.value.title] }),
    })
    const allVisibleNodes = () =>
      assetTree
        .preorderTraversal(children =>
          children.filter(child => visibilities.get(child.key) !== Visibility.hidden)
        )
        .filter(
          node =>
            visibilities.get(node.key) === Visibility.visible &&
            node.item.value.type !== backendModule.AssetType.specialEmpty &&
            node.item.value.type !== backendModule.AssetType.specialLoading
        )
    const allVisible = (negative = false) =>
      allVisibleNodes().map(node => nodeToSuggestion(node, negative ? 'negativeNames' : 'names'))
    const terms = AssetQuery.terms(query.query)
    const term = terms.find(otherTerm => otherTerm.values.length === 0) ?? terms[terms.length - 1]
    const termValues = term?.values ?? []
    const shouldOmitNames = terms.some(otherTerm => otherTerm.tag === 'name')
    if (termValues.length !== 0) {
      setSuggestions(shouldOmitNames ? [] : allVisible())
    } else {
      const negative = term?.tag?.startsWith('-') ?? false
      switch (term?.tag ?? null) {
        case null:
        case '':
        case '-':
        case 'name':
        case '-name': {
          setSuggestions(allVisible(negative))
          break
        }
        case 'no':
        case '-has': {
          setSuggestions(SUGGESTIONS_FOR_NO)
          break
        }
        case 'has':
        case '-no': {
          setSuggestions(SUGGESTIONS_FOR_HAS)
          break
        }
        case 'type': {
          setSuggestions(SUGGESTIONS_FOR_TYPE)
          break
        }
        case '-type': {
          setSuggestions(SUGGESTIONS_FOR_NEGATIVE_TYPE)
          break
        }
        case 'ext':
        case '-ext':
        case 'extension':
        case '-extension': {
          const extensions = allVisibleNodes()
            .filter(node => node.item.value.type === backendModule.AssetType.file)
            .map(node => fileInfo.fileExtension(node.item.value.title))
          setSuggestions(
            Array.from(
              new Set(extensions),
              (extension): assetSearchBar.Suggestion => ({
                render: () =>
                  AssetQuery.termToString({
                    tag: `${negative ? '-' : ''}extension`,
                    values: [extension],
                  }),
                addToQuery: oldQuery =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeExtensions: [extension] } : { extensions: [extension] }
                  ),
                deleteFromQuery: oldQuery =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeExtensions: [extension] } : { extensions: [extension] }
                  ),
              })
            )
          )
          break
        }
        case 'modified':
        case '-modified': {
          const modifieds = assetTree.preorderTraversal().map(node => {
            const date = new Date(node.item.value.modifiedAt)
            return `${date.getFullYear()}-${date.getMonth() + 1}-${date.getDate()}`
          })
          setSuggestions(
            Array.from(
              new Set(['today', ...modifieds]),
              (modified): assetSearchBar.Suggestion => ({
                render: () =>
                  AssetQuery.termToString({
                    tag: `${negative ? '-' : ''}modified`,
                    values: [modified],
                  }),
                addToQuery: oldQuery =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeModifieds: [modified] } : { modifieds: [modified] }
                  ),
                deleteFromQuery: oldQuery =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeModifieds: [modified] } : { modifieds: [modified] }
                  ),
              })
            )
          )
          break
        }
        case 'owner':
        case '-owner': {
          const owners = assetTree
            .preorderTraversal()
            .flatMap(node =>
              (node.item.value.permissions ?? [])
                .filter(permission => permission.permission === permissions.PermissionAction.own)
                .map(permission => permission.user.user_name)
            )
          setSuggestions(
            Array.from(
              new Set(owners),
              (owner): assetSearchBar.Suggestion => ({
                render: () =>
                  AssetQuery.termToString({
                    tag: `${negative ? '-' : ''}owner`,
                    values: [owner],
                  }),
                addToQuery: oldQuery =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeOwners: [owner] } : { owners: [owner] }
                  ),
                deleteFromQuery: oldQuery =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeOwners: [owner] } : { owners: [owner] }
                  ),
              })
            )
          )
          break
        }
        case 'label':
        case '-label': {
          setSuggestions(
            Array.from(
              allLabels.values(),
              (label): assetSearchBar.Suggestion => ({
                render: () => (
                  <Label active color={label.color} onClick={() => {}}>
                    {label.value}
                  </Label>
                ),
                addToQuery: oldQuery =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeLabels: [label.value] } : { labels: [label.value] }
                  ),
                deleteFromQuery: oldQuery =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeLabels: [label.value] } : { labels: [label.value] }
                  ),
              })
            )
          )

          break
        }
        default: {
          setSuggestions(shouldOmitNames ? [] : allVisible())
          break
        }
      }
    }
  }, [assetTree, query, visibilities, allLabels, /* should never change */ setSuggestions])

  React.useEffect(() => {
    setIsLoading(true)
  }, [isCloud, rootDirectory, category])

  React.useEffect(() => {
    const newNodeMap = new Map(assetTree.preorderTraversal().map(asset => [asset.key, asset]))
    newNodeMap.set(assetTree.key, assetTree)
    nodeMapRef.current = newNodeMap
  }, [assetTree])

  React.useEffect(() => {
    return shortcutManager.registerKeyboardHandlers({
      [shortcutManagerModule.KeyboardAction.cancelCut]: () => {
        if (pasteDataRef.current == null) {
          return false
        } else {
          dispatchAssetEvent({ type: AssetEventType.cancelCut, ids: pasteDataRef.current.data })
          pasteDataRef.current = null
          setHasPasteData(false)
          return
        }
      },
    })
  }, [/* should never change */ shortcutManager, /* should never change */ dispatchAssetEvent])

  React.useEffect(() => {
    if (isLoading) {
      queuedProject.current = initialProjectName
    } else {
      // The project name here might also be a string with project id, e.g. when opening
      // a project file from explorer on Windows.
      const isInitialProject = (asset: backendModule.AnyAsset) =>
        asset.title === initialProjectName || asset.id === initialProjectName
      const projectToLoad = assetTree
        .preorderTraversal()
        .map(node => node.item.value)
        .filter(backendModule.assetIsProject)
        .find(isInitialProject)
      if (projectToLoad != null) {
        window.setTimeout(() => {
          dispatchAssetEvent({
            type: AssetEventType.openProject,
            id: projectToLoad.id,
            shouldAutomaticallySwitchPage: true,
            runInBackground: false,
          })
        })
      } else if (initialProjectName != null) {
        toastAndLog(`Could not find project '${initialProjectName}'`)
      }
    }
    // This effect MUST only run when `initialProjectName` is changed.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [initialProjectName])

  const overwriteNodes = React.useCallback(
    (newAssets: backendModule.AnySmartAsset[]) => {
      setInitialized(true)
      const rootParentDirectoryId = backendModule.DirectoryId('')
      const newRootNode = new AssetTreeNode(
        rootDirectory.value.id,
        rootDirectory,
        rootParentDirectoryId,
        // This is INCORRECT, but it should not ever need to be accessed.
        rootDirectory,
        newAssets.map(asset =>
          AssetTreeNode.fromSmartAsset(asset, rootDirectory.value.id, rootDirectory, 0)
        ),
        -1
      )
      setAssetTree(newRootNode)
      // The project name here might also be a string with project id, e.g.
      // when opening a project file from explorer on Windows.
      const isInitialProject = (asset: backendModule.AnyAsset) =>
        asset.title === queuedProject.current || asset.id === queuedProject.current
      if (queuedProject.current != null) {
        const projectToLoad = newAssets
          .map(smartAsset => smartAsset.value)
          .filter(backendModule.assetIsProject)
          .find(isInitialProject)
        queuedProject.current = null
        if (projectToLoad != null) {
          window.setTimeout(() => {
            dispatchAssetEvent({
              type: AssetEventType.openProject,
              id: projectToLoad.id,
              shouldAutomaticallySwitchPage: true,
              runInBackground: false,
            })
          })
        } else {
          toastAndLog(`Could not find project '${queuedProject.current}'`)
        }
      }
    },
    [
      rootDirectory,
      /* should never change */ queuedProject,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ toastAndLog,
    ]
  )

  React.useEffect(() => {
    if (initialized) {
      overwriteNodes([])
    }
    // `overwriteNodes` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [rootDirectory, category])

  asyncEffectHooks.useAsyncEffect(
    null,
    async signal => {
      if (!isCloud) {
        const newAssets =
          category === Category.recent
            ? (await organization?.listRecentFiles()) ?? []
            : await rootDirectory.list({
                filterBy: CATEGORY_TO_FILTER_BY[category],
                labels: null,
              })
        if (!signal.aborted) {
          setIsLoading(false)
          overwriteNodes(newAssets)
        }
      } else {
        const queuedDirectoryListings = new Map<
          backendModule.AssetId,
          backendModule.AnySmartAsset[]
        >()
        const withChildren = (node: AssetTreeNode): AssetTreeNode => {
          const queuedListing = queuedDirectoryListings.get(node.item.value.id)
          if (queuedListing == null || node.item.type !== backendModule.AssetType.directory) {
            return node
          } else {
            const directory = node.item
            const directoryAsset = node.item.value
            const depth = node.depth + 1
            return node.with({
              children: queuedListing.map(asset =>
                withChildren(
                  AssetTreeNode.fromSmartAsset(asset, directoryAsset.id, directory, depth)
                )
              ),
            })
          }
        }
        for (const entry of nodeMapRef.current.values()) {
          if (entry.item.type === backendModule.AssetType.directory && entry.children != null) {
            void entry.item.list({ filterBy: CATEGORY_TO_FILTER_BY[category], labels: null }).then(
              assets => {
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
              },
              error => {
                toastAndLog(null, error)
              }
            )
          }
        }
        try {
          const newAssets =
            category === Category.recent
              ? (await organization?.listRecentFiles()) ?? []
              : await rootDirectory.list({
                  filterBy: CATEGORY_TO_FILTER_BY[category],
                  labels: null,
                })
          if (!signal.aborted) {
            setIsLoading(false)
            overwriteNodes(newAssets)
          }
        } catch (error) {
          if (!signal.aborted) {
            setIsLoading(false)
            toastAndLog(null, error)
          }
        }
      }
    },
    [rootDirectory, category, accessToken, organization]
  )

  React.useEffect(() => {
    const savedExtraColumns = localStorage.get('extraColumns')
    if (savedExtraColumns != null) {
      setExtraColumns(new Set(savedExtraColumns))
    }
  }, [/* should never change */ localStorage])

  // Clip the header bar so that the background behind the extra colums selector is visible.
  React.useEffect(() => {
    const headerRow = headerRowRef.current
    const scrollContainer = scrollContainerRef.current
    if (isCloud && headerRow != null && scrollContainer != null) {
      let isClipPathUpdateQueued = false
      const updateClipPath = () => {
        isClipPathUpdateQueued = false
        const rightOffset = `${
          scrollContainer.clientWidth + scrollContainer.scrollLeft - TABLE_HEADER_WIDTH_SHRINKAGE_PX
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
  }, [isCloud])

  React.useEffect(() => {
    if (initialized) {
      localStorage.set('extraColumns', Array.from(extraColumns))
    }
  }, [extraColumns, initialized, /* should never change */ localStorage])

  React.useEffect(() => {
    if (selectedKeys.size !== 1) {
      setAssetPanelProps(null)
    }
  }, [selectedKeys.size, /* should never change */ setAssetPanelProps])

  const directoryListAbortControllersRef = React.useRef(
    new Map<backendModule.DirectoryId, AbortController>()
  )
  const doToggleDirectoryExpansion = React.useCallback(
    (directory: backendModule.SmartDirectory, key: backendModule.AssetId, override?: boolean) => {
      const directoryNode = nodeMapRef.current.get(key)
      const isExpanded = directoryNode?.children != null
      const shouldExpand = override ?? !isExpanded
      if (shouldExpand === isExpanded) {
        // This is fine, as this is near the top of a very long function.
        // eslint-disable-next-line no-restricted-syntax
        return
      }
      if (!shouldExpand) {
        const abortController = directoryListAbortControllersRef.current.get(directory.value.id)
        if (abortController != null) {
          abortController.abort()
          directoryListAbortControllersRef.current.delete(directory.value.id)
        }
        setAssetTree(oldAssetTree =>
          oldAssetTree.map(item => (item.key !== key ? item : item.with({ children: null })))
        )
      } else {
        setAssetTree(oldAssetTree =>
          oldAssetTree.map(item =>
            item.key !== key
              ? item
              : item.with({
                  children: [
                    AssetTreeNode.fromSmartAsset(
                      directory.createSpecialLoadingAsset(),
                      key,
                      directory,
                      item.depth + 1
                    ),
                  ],
                })
          )
        )
        void (async () => {
          const abortController = new AbortController()
          directoryListAbortControllersRef.current.set(directory.value.id, abortController)
          const filterBy = CATEGORY_TO_FILTER_BY[category]
          const childAssets = await directory.list({ filterBy, labels: null })
          if (!abortController.signal.aborted) {
            setAssetTree(oldAssetTree =>
              oldAssetTree.map(item => {
                if (item.key !== key) {
                  return item
                } else {
                  const initialChildren = item.children?.filter(
                    child => child.item.value.type !== backendModule.AssetType.specialLoading
                  )
                  const childAssetsMap = new Map(childAssets.map(asset => [asset.value.id, asset]))
                  for (const child of initialChildren ?? []) {
                    const newChild = childAssetsMap.get(child.item.value.id)
                    if (newChild != null) {
                      child.item = newChild
                      childAssetsMap.delete(child.item.value.id)
                    }
                  }
                  const childAssetNodes = Array.from(childAssetsMap.values(), child =>
                    AssetTreeNode.fromSmartAsset(child, key, directory, item.depth + 1)
                  )
                  const specialEmptyAsset =
                    (initialChildren != null && initialChildren.length !== 0) ||
                    childAssetNodes.length !== 0
                      ? null
                      : directory.createSpecialEmptyAsset()
                  const children =
                    specialEmptyAsset != null
                      ? [
                          AssetTreeNode.fromSmartAsset(
                            specialEmptyAsset,
                            key,
                            directory,
                            item.depth + 1
                          ),
                        ]
                      : initialChildren == null || initialChildren.length === 0
                      ? childAssetNodes
                      : [...initialChildren, ...childAssetNodes].sort(AssetTreeNode.compare)
                  return item.with({ children })
                }
              })
            )
          }
        })()
      }
    },
    [category]
  )

  const deleteAsset = React.useCallback((key: backendModule.AssetId) => {
    setAssetTree(oldAssetTree => oldAssetTree.filter(item => item.key !== key))
  }, [])

  /** All items must have the same type. */
  const insertAssets = React.useCallback(
    (
      assets: backendModule.AnySmartAsset[],
      parentKey: backendModule.AssetId,
      parent: backendModule.SmartDirectory
    ) => {
      setAssetTree(oldAssetTree =>
        oldAssetTree.withHomogeneousDescendantsInserted(assets, parentKey, parent)
      )
    },
    []
  )

  eventHooks.useEventHandler(assetListEvents, event => {
    switch (event.type) {
      case AssetListEventType.newFolder: {
        const siblings = nodeMapRef.current.get(event.parentKey)?.children ?? []
        const directoryIndices = siblings
          .map(node => node.item.value)
          .filter(backendModule.assetIsDirectory)
          .map(item => /^New Folder (?<directoryIndex>\d+)$/.exec(item.title))
          .map(match => match?.groups?.directoryIndex)
          .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        const title = `New Folder ${Math.max(0, ...directoryIndices) + 1}`
        const permission = permissions.tryGetSingletonOwnerPermission(organization, user)
        const placeholderItem = event.parent.createPlaceholderDirectory(title, permission)
        doToggleDirectoryExpansion(event.parent, event.parentKey, true)
        insertAssets([placeholderItem], event.parentKey, event.parent)
        break
      }
      case AssetListEventType.newProject: {
        const siblings = nodeMapRef.current.get(event.parentKey)?.children ?? []
        const prefix = event.templateName ?? 'New Project'
        const projectNameRegex = new RegExp(`^${prefix} (?<projectIndex>\\d+)$`)
        const projectIndices = siblings
          .map(node => node.item.value)
          .filter(backendModule.assetIsProject)
          .map(item => projectNameRegex.exec(item.title))
          .map(match => match?.groups?.projectIndex)
          .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        const projectName = `${prefix} ${Math.max(0, ...projectIndices) + 1}`
        const permission = permissions.tryGetSingletonOwnerPermission(organization, user)
        const placeholderItem = event.parent.createPlaceholderProject(
          projectName,
          event.templateId,
          permission
        )
        doToggleDirectoryExpansion(event.parent, event.parentKey, true)
        insertAssets([placeholderItem], event.parentKey, event.parent)
        break
      }
      case AssetListEventType.uploadFiles: {
        const reversedFiles = Array.from(event.files).reverse()
        const siblingNodes = nodeMapRef.current.get(event.parentKey)?.children ?? []
        const siblings = siblingNodes.map(node => node.item)
        const siblingFiles = siblings.filter(backendModule.smartAssetIsFile)
        const siblingProjects = siblings.filter(backendModule.smartAssetIsProject)
        const siblingFileTitles = new Set(siblingFiles.map(asset => asset.value.title))
        const siblingProjectTitles = new Set(siblingProjects.map(asset => asset.value.title))
        const files = reversedFiles.filter(backendModule.fileIsNotProject)
        const projects = reversedFiles.filter(backendModule.fileIsProject)
        const duplicateFiles = files.filter(file => siblingFileTitles.has(file.name))
        const duplicateProjects = projects.filter(project =>
          siblingProjectTitles.has(backendModule.stripProjectExtension(project.name))
        )
        const permission = permissions.tryGetSingletonOwnerPermission(organization, user)
        if (duplicateFiles.length === 0 && duplicateProjects.length === 0) {
          const placeholderFiles = files.map(file =>
            event.parent.createPlaceholderFile(file.name, file, permission)
          )
          const placeholderProjects = projects.map(project =>
            event.parent.createPlaceholderProject(project.name, project, permission)
          )
          doToggleDirectoryExpansion(event.parent, event.parentKey, true)
          insertAssets(placeholderFiles, event.parentKey, event.parent)
          insertAssets(placeholderProjects, event.parentKey, event.parent)
        } else {
          const siblingFilesByName = new Map(siblingFiles.map(file => [file.value.title, file]))
          const siblingProjectsByName = new Map(
            siblingProjects.map(project => [project.value.title, project])
          )
          const conflictingFiles = duplicateFiles.map(file => ({
            // This is SAFE, as `duplicateFiles` only contains files that have siblings
            // with the same name.
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            current: siblingFilesByName.get(file.name)!,
            new: event.parent.createPlaceholderFile(file.name, file, permission),
            file,
          }))
          const conflictingProjects = duplicateProjects.map(project => {
            const basename = backendModule.stripProjectExtension(project.name)
            return {
              // This is SAFE, as `duplicateProjects` only contains projects that have
              // siblings with the same name.
              // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
              current: siblingProjectsByName.get(basename)!,
              new: event.parent.createPlaceholderProject(basename, project, permission),
              file: project,
            }
          })
          setModal(
            <DuplicateAssetsModal
              parentKey={event.parentKey}
              parent={event.parent}
              conflictingFiles={conflictingFiles}
              conflictingProjects={conflictingProjects}
              dispatchAssetEvent={dispatchAssetEvent}
              dispatchAssetListEvent={dispatchAssetListEvent}
              siblingFileNames={siblingFilesByName.keys()}
              siblingProjectNames={siblingProjectsByName.keys()}
              nonConflictingCount={
                files.length +
                projects.length -
                conflictingFiles.length -
                conflictingProjects.length
              }
              doUploadNonConflicting={() => {
                doToggleDirectoryExpansion(event.parent, event.parentKey, true)
                const newFiles = files
                  .filter(file => !siblingFileTitles.has(file.name))
                  .map(file => event.parent.createPlaceholderFile(file.name, file, permission))
                const newProjects = projects
                  .filter(
                    project =>
                      !siblingProjectTitles.has(backendModule.stripProjectExtension(project.name))
                  )
                  .map(project =>
                    event.parent.createPlaceholderProject(
                      backendModule.stripProjectExtension(project.name),
                      project,
                      permission
                    )
                  )
                insertAssets(newFiles, event.parentKey, event.parent)
                insertAssets(newProjects, event.parentKey, event.parent)
              }}
            />
          )
        }
        break
      }
      case AssetListEventType.newSecret: {
        doToggleDirectoryExpansion(event.parent, event.parentKey, true)
        const permission = permissions.tryGetSingletonOwnerPermission(organization, user)
        const placeholderItem = event.parent.createPlaceholderSecret(
          event.name,
          event.value,
          permission
        )
        insertAssets([placeholderItem], event.parentKey, event.parent)
        break
      }
      case AssetListEventType.insertAssets: {
        setAssetTree(oldAssetTree =>
          oldAssetTree.withDescendantsInserted(event.assets, event.parentKey, event.parent)
        )
        break
      }
      case AssetListEventType.willDelete: {
        if (selectedKeys.has(event.key)) {
          setSelectedKeys(oldSelectedKeys => {
            const newSelectedKeys = new Set(oldSelectedKeys)
            newSelectedKeys.delete(event.key)
            return newSelectedKeys
          })
        }
        break
      }
      case AssetListEventType.copy: {
        const ids = new Set<backendModule.AssetId>()
        const { items, newParentKey, newParent } = event
        const getKey = (asset: backendModule.AnyAsset) => {
          const newId = backendModule.createPlaceholderAssetId(asset.type)
          ids.add(newId)
          return newId
        }
        setAssetTree(oldAssetTree =>
          oldAssetTree.withDescendantsInserted(items, newParentKey, newParent, getKey)
        )
        dispatchAssetEvent({ type: AssetEventType.copy, ids, newParentKey, newParent })
        break
      }
      case AssetListEventType.move: {
        deleteAsset(event.key)
        insertAssets([event.item], event.newParentKey, event.newParent)
        break
      }
      case AssetListEventType.delete: {
        deleteAsset(event.key)
        break
      }
      case AssetListEventType.removeSelf: {
        dispatchAssetEvent({
          type: AssetEventType.removeSelf,
          id: event.id,
        })
        break
      }
      case AssetListEventType.closeFolder: {
        doToggleDirectoryExpansion(event.folder, event.key, false)
        break
      }
    }
  })

  const doOpenManually = React.useCallback(
    (projectId: backendModule.ProjectId) => {
      dispatchAssetEvent({
        type: AssetEventType.openProject,
        id: projectId,
        shouldAutomaticallySwitchPage: true,
        runInBackground: false,
      })
    },
    [/* should never change */ dispatchAssetEvent]
  )

  const doCloseEditor = React.useCallback(
    (project: backendModule.ProjectAsset) => {
      if (project.id === projectStartupInfo?.projectAsset.value.id) {
        dispatchAssetEvent({
          type: AssetEventType.cancelOpeningAllProjects,
        })
        rawDoCloseEditor(project)
      }
    },
    [projectStartupInfo, rawDoCloseEditor, /* should never change */ dispatchAssetEvent]
  )

  const doCopy = React.useCallback(() => {
    unsetModal()
    setSelectedKeys(oldSelectedKeys => {
      queueMicrotask(() => {
        pasteDataRef.current = { type: PasteType.copy, data: oldSelectedKeys }
        setHasPasteData(true)
      })
      return oldSelectedKeys
    })
  }, [/* should never change */ unsetModal])

  const doCut = React.useCallback(() => {
    unsetModal()
    setSelectedKeys(oldSelectedKeys => {
      queueMicrotask(() => {
        if (pasteDataRef.current != null) {
          dispatchAssetEvent({ type: AssetEventType.cancelCut, ids: pasteDataRef.current.data })
        }
        pasteDataRef.current = { type: PasteType.move, data: oldSelectedKeys }
        setHasPasteData(true)
        dispatchAssetEvent({ type: AssetEventType.cut, ids: oldSelectedKeys })
      })
      return new Set()
    })
  }, [/* should never change */ unsetModal, /* should never change */ dispatchAssetEvent])

  const doPaste = React.useCallback(
    (newParentKey: backendModule.AssetId) => {
      unsetModal()
      const pasteData = pasteDataRef.current
      if (pasteData != null) {
        const newParentNode = nodeMapRef.current.get(newParentKey)
        if (pasteData.data.has(newParentKey)) {
          toastAndLog('Cannot paste a folder into itself')
        } else if (newParentNode == null) {
          toastAndLog('New parent folder was not found')
        } else if (newParentNode.item.type !== backendModule.AssetType.directory) {
          toastAndLog('New parent is not a folder')
        } else {
          const newParent = newParentNode.item
          doToggleDirectoryExpansion(newParentNode.item, newParentKey, true)
          if (pasteData.type === PasteType.copy) {
            const items = Array.from(pasteData.data, id => nodeMapRef.current.get(id)).flatMap(
              asset => (asset ? [asset.item] : [])
            )
            dispatchAssetListEvent({
              type: AssetListEventType.copy,
              items,
              newParentKey,
              newParent,
            })
          } else {
            const ids = pasteData.data
            dispatchAssetEvent({ type: AssetEventType.move, ids, newParentKey, newParent })
          }
          pasteDataRef.current = null
        }
      }
    },
    [
      doToggleDirectoryExpansion,
      /* should never change */ toastAndLog,
      /* should never change */ unsetModal,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const hiddenContextMenu = React.useMemo(
    () => (
      <AssetsTableContextMenu
        hidden
        isCloud={isCloud}
        category={category}
        hasPasteData={hasPasteData}
        selectedKeys={selectedKeys}
        nodeMapRef={nodeMapRef}
        event={{ pageX: 0, pageY: 0 }}
        setSelectedKeys={setSelectedKeys}
        dispatchAssetEvent={dispatchAssetEvent}
        dispatchAssetListEvent={dispatchAssetListEvent}
        doCopy={doCopy}
        doCut={doCut}
        doPaste={doPaste}
      />
    ),
    [
      isCloud,
      category,
      hasPasteData,
      selectedKeys,
      doCopy,
      doCut,
      doPaste,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const onDragOver = (event: React.DragEvent<Element>) => {
    const payload = drag.ASSET_ROWS.lookup(event)
    const filtered = payload?.filter(item => item.asset.parentId !== rootDirectory.value.id)
    if (filtered != null && filtered.length > 0) {
      event.preventDefault()
    }
  }

  const state = React.useMemo(
    // The type MUST be here to trigger excess property errors at typecheck time.
    (): AssetsTableState => ({
      isCloud,
      numberOfSelectedItems: selectedKeys.size,
      category,
      labels: allLabels,
      deletedLabelNames,
      hasPasteData,
      sortColumn,
      setSortColumn,
      sortDirection,
      setSortDirection,
      query,
      setQuery,
      assetEvents,
      dispatchAssetEvent,
      dispatchAssetListEvent,
      setAssetPanelProps,
      nodeMap: nodeMapRef,
      doToggleDirectoryExpansion,
      doOpenManually,
      doOpenEditor,
      doCloseEditor,
      doCreateLabel,
      doCopy,
      doCut,
      doPaste,
    }),
    [
      isCloud,
      selectedKeys.size,
      category,
      allLabels,
      deletedLabelNames,
      hasPasteData,
      sortColumn,
      sortDirection,
      assetEvents,
      query,
      doToggleDirectoryExpansion,
      doOpenManually,
      doOpenEditor,
      doCloseEditor,
      doCreateLabel,
      doCopy,
      doCut,
      doPaste,
      /* should never change */ setAssetPanelProps,
      /* should never change */ setQuery,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
  const [previouslySelectedKey, setPreviouslySelectedKey] =
    React.useState<backendModule.AssetId | null>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)

  // This is required to prevent the table body from overlapping the table header, because
  // the table header is transparent.
  React.useEffect(() => {
    const body = bodyRef.current
    const scrollContainer = scrollContainerRef.current
    if (body != null && scrollContainer != null) {
      let isClipPathUpdateQueued = false
      const updateClipPath = () => {
        isClipPathUpdateQueued = false
        body.style.clipPath = `inset(${scrollContainer.scrollTop}px 0 0 0)`
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
  }, [/* should never change */ scrollContainerRef])

  React.useEffect(() => {
    const onDocumentClick = (event: MouseEvent) => {
      if (
        !shortcutManager.matchesMouseAction(
          shortcutManagerModule.MouseAction.selectAdditional,
          event
        ) &&
        !shortcutManager.matchesMouseAction(
          shortcutManagerModule.MouseAction.selectAdditionalRange,
          event
        ) &&
        selectedKeys.size !== 0
      ) {
        setSelectedKeys(new Set())
      }
    }
    document.addEventListener('click', onDocumentClick)
    return () => {
      document.removeEventListener('click', onDocumentClick)
    }
  }, [selectedKeys, /* should never change */ setSelectedKeys, shortcutManager])

  React.useEffect(() => {
    if (isLoading) {
      // Ensure the spinner stays in the "initial" state for at least one frame,
      // to ensure the CSS animation begins at the initial state.
      requestAnimationFrame(() => {
        setSpinnerState(spinner.SpinnerState.loadingFast)
      })
    } else {
      setSpinnerState(spinner.SpinnerState.initial)
    }
  }, [isLoading])

  const onRowClick = React.useCallback(
    (innerRowProps: assetRow.AssetRowInnerProps, event: React.MouseEvent) => {
      const { key } = innerRowProps
      event.stopPropagation()
      const getNewlySelectedKeys = () => {
        if (previouslySelectedKey == null) {
          return [key]
        } else {
          const index1 = displayItems.findIndex(
            innerItem => AssetTreeNode.getKey(innerItem) === previouslySelectedKey
          )
          const index2 = displayItems.findIndex(
            innerItem => AssetTreeNode.getKey(innerItem) === key
          )
          const selectedItems =
            index1 <= index2
              ? displayItems.slice(index1, index2 + 1)
              : displayItems.slice(index2, index1 + 1)
          return selectedItems.map(AssetTreeNode.getKey)
        }
      }
      if (
        shortcutManager.matchesMouseAction(shortcutManagerModule.MouseAction.selectRange, event)
      ) {
        setSelectedKeys(new Set(getNewlySelectedKeys()))
      } else if (
        shortcutManager.matchesMouseAction(
          shortcutManagerModule.MouseAction.selectAdditionalRange,
          event
        )
      ) {
        setSelectedKeys(
          oldSelectedItems => new Set([...oldSelectedItems, ...getNewlySelectedKeys()])
        )
      } else if (
        shortcutManager.matchesMouseAction(
          shortcutManagerModule.MouseAction.selectAdditional,
          event
        )
      ) {
        setSelectedKeys(oldSelectedItems =>
          set.withPresence(oldSelectedItems, key, !oldSelectedItems.has(key))
        )
      } else {
        setSelectedKeys(new Set([key]))
      }
      setPreviouslySelectedKey(key)
    },
    [
      displayItems,
      previouslySelectedKey,
      shortcutManager,
      /* should never change */ setSelectedKeys,
    ]
  )

  const columns = columnUtils.getColumnList(isCloud, extraColumns)

  const headerRow = (
    <tr ref={headerRowRef} className="sticky top-0">
      {columns.map(column => {
        // This is a React component, even though it does not contain JSX.
        // eslint-disable-next-line no-restricted-syntax
        const Heading = columnHeading.COLUMN_HEADING[column]
        return (
          <th
            key={column}
            className={`text-sm font-semibold ${columnUtils.COLUMN_CSS_CLASS[column]}`}
          >
            <Heading state={state} />
          </th>
        )
      })}
    </tr>
  )

  const itemRows = isLoading ? (
    <tr className="h-8">
      <td colSpan={columns.length} className="bg-transparent">
        <div className="grid justify-around w-full">
          <Spinner size={LOADING_SPINNER_SIZE} state={spinnerState} />
        </div>
      </td>
    </tr>
  ) : (
    displayItems.map(item => {
      const key = AssetTreeNode.getKey(item)
      const isSelected = selectedKeys.has(key)
      const isSoleSelectedItem = selectedKeys.size === 1 && isSelected
      return (
        <AssetRow
          key={key}
          columns={columns}
          item={item}
          state={state}
          visibility={visibilities.get(item.key) ?? null}
          selected={isSelected}
          setSelected={selected => {
            setSelectedKeys(oldSelectedKeys => set.withPresence(oldSelectedKeys, key, selected))
          }}
          isSoleSelectedItem={isSoleSelectedItem}
          allowContextMenu={selectedKeys.size === 0 || !isSelected || isSoleSelectedItem}
          onClick={onRowClick}
          onContextMenu={(_innerProps, event) => {
            if (!isSelected) {
              event.preventDefault()
              event.stopPropagation()
              setPreviouslySelectedKey(key)
              setSelectedKeys(new Set([key]))
            }
          }}
          onDragStart={event => {
            if (!selectedKeys.has(key)) {
              setPreviouslySelectedKey(key)
              setSelectedKeys(new Set([key]))
            }
            setSelectedKeys(oldSelectedKeys => {
              const nodes = assetTree
                .preorderTraversal()
                .filter(node => oldSelectedKeys.has(node.key))
              const payload: drag.AssetRowsDragPayload = nodes.map(node => ({
                key: node.key,
                asset: node.item.value,
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
                      <UninteractableNameColumn
                        key={node.key}
                        item={node.with({ depth: 0 })}
                        state={state}
                      />
                    ))}
                  </DragModal>
                )
              })
              return oldSelectedKeys
            })
          }}
          onDragOver={event => {
            setSelectedKeys(oldSelectedKeys => {
              const labelNames = drag.LABELS.lookup(event)
              if (labelNames != null) {
                event.preventDefault()
                event.stopPropagation()
                const ids = oldSelectedKeys.has(key) ? oldSelectedKeys : new Set([key])
                // Expand ids to include ids of children as well.
                for (const node of assetTree.preorderTraversal()) {
                  if (ids.has(node.key) && node.children != null) {
                    for (const child of node.children) {
                      ids.add(child.key)
                    }
                  }
                }
                let labelsPresent = 0
                for (const selectedKey of ids) {
                  const labels = nodeMapRef.current.get(selectedKey)?.item.value.labels
                  if (labels != null) {
                    for (const label of labels) {
                      if (labelNames.has(label)) {
                        labelsPresent += 1
                      }
                    }
                  }
                }
                window.setTimeout(() => {
                  if (labelsPresent * 2 < ids.size * labelNames.size) {
                    dispatchAssetEvent({
                      type: AssetEventType.temporarilyAddLabels,
                      ids,
                      labelNames,
                    })
                  } else {
                    dispatchAssetEvent({
                      type: AssetEventType.temporarilyRemoveLabels,
                      ids,
                      labelNames,
                    })
                  }
                })
              }
              return oldSelectedKeys
            })
          }}
          onDragEnd={() => {
            setSelectedKeys(oldSelectedKeys => {
              window.setTimeout(() => {
                dispatchAssetEvent({
                  type: AssetEventType.temporarilyAddLabels,
                  ids: oldSelectedKeys,
                  labelNames: set.EMPTY,
                })
              })
              return oldSelectedKeys
            })
          }}
          onDrop={event => {
            setSelectedKeys(oldSelectedKeys => {
              const ids = oldSelectedKeys.has(key) ? new Set(oldSelectedKeys) : new Set([key])
              // Expand ids to include ids of children as well.
              for (const node of assetTree.preorderTraversal()) {
                if (ids.has(node.key) && node.children != null) {
                  for (const child of node.children) {
                    ids.add(child.key)
                  }
                }
              }
              const labelNames = drag.LABELS.lookup(event) ?? set.EMPTY
              if (labelNames === set.EMPTY) {
                window.setTimeout(() => {
                  dispatchAssetEvent({ type: AssetEventType.temporarilyAddLabels, ids, labelNames })
                })
              } else {
                event.preventDefault()
                event.stopPropagation()
                let labelsPresent = 0
                for (const selectedKey of ids) {
                  const labels = nodeMapRef.current.get(selectedKey)?.item.value.labels
                  if (labels != null) {
                    for (const label of labels) {
                      if (labelNames.has(label)) {
                        labelsPresent += 1
                      }
                    }
                  }
                }
                window.setTimeout(() => {
                  if (labelsPresent * 2 < ids.size * labelNames.size) {
                    dispatchAssetEvent({ type: AssetEventType.addLabels, ids, labelNames })
                  } else {
                    dispatchAssetEvent({ type: AssetEventType.removeLabels, ids, labelNames })
                  }
                })
              }
              return oldSelectedKeys
            })
          }}
        />
      )
    })
  )

  const table = (
    <div
      className="grow flex flex-col"
      onContextMenu={event => {
        event.preventDefault()
        event.stopPropagation()
        setModal(
          <AssetsTableContextMenu
            isCloud={isCloud}
            category={category}
            hasPasteData={hasPasteData}
            selectedKeys={selectedKeys}
            nodeMapRef={nodeMapRef}
            event={event}
            setSelectedKeys={setSelectedKeys}
            dispatchAssetEvent={dispatchAssetEvent}
            dispatchAssetListEvent={dispatchAssetListEvent}
            doCopy={doCopy}
            doCut={doCut}
            doPaste={doPaste}
          />
        )
      }}
      onDragLeave={event => {
        const payload = drag.LABELS.lookup(event)
        if (
          payload != null &&
          event.relatedTarget instanceof Node &&
          !event.currentTarget.contains(event.relatedTarget)
        ) {
          setSelectedKeys(ids => {
            window.setTimeout(() => {
              const labelNames = set.EMPTY
              dispatchAssetEvent({ type: AssetEventType.temporarilyAddLabels, ids, labelNames })
            })
            return ids
          })
        }
      }}
    >
      <table className="rounded-rows table-fixed border-collapse">
        <thead>{headerRow}</thead>
        <tbody ref={bodyRef}>
          {itemRows}
          <tr className="h-8 hidden first:table-row">
            <td colSpan={columns.length} className="bg-transparent">
              {placeholder}
            </td>
          </tr>
        </tbody>
      </table>
      <div
        className="grow"
        onDragEnter={onDragOver}
        onDragOver={onDragOver}
        onDrop={event => {
          const payload = drag.ASSET_ROWS.lookup(event)
          const filtered = payload?.filter(item => item.asset.parentId !== rootDirectory.value.id)
          if (filtered != null && filtered.length > 0) {
            event.preventDefault()
            event.stopPropagation()
            unsetModal()
            dispatchAssetEvent({
              type: AssetEventType.move,
              newParentKey: rootDirectory.value.id,
              newParent: rootDirectory,
              ids: new Set(filtered.map(dragItem => dragItem.asset.id)),
            })
          }
        }}
      />
    </div>
  )

  return (
    <div ref={scrollContainerRef} className="flex-1 overflow-auto">
      <div className="flex flex-col w-min min-w-full h-full">
        {isCloud && (
          <div className="sticky top-0 h-0 flex flex-col">
            <div className="block sticky right-0 self-end w-29 px-2 pt-2.25 pb-1.75 z-1">
              <div className="inline-flex gap-3">
                {columnUtils.EXTRA_COLUMNS.map(column => (
                  <Button
                    key={column}
                    active={extraColumns.has(column)}
                    image={columnUtils.EXTRA_COLUMN_IMAGES[column]}
                    alt={`${extraColumns.has(column) ? 'Show' : 'Hide'} ${
                      columnUtils.COLUMN_NAME[column]
                    }`}
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
        {table}
      </div>
    </div>
  )
}
