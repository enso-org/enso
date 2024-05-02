/** @file Table displaying a list of projects. */
import * as React from 'react'

import * as toast from 'react-toastify'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as eventHooks from '#/hooks/eventHooks'
import * as scrollHooks from '#/hooks/scrollHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import type * as assetRow from '#/components/dashboard/AssetRow'
import AssetRow from '#/components/dashboard/AssetRow'
import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import NameColumn from '#/components/dashboard/column/NameColumn'
import * as columnHeading from '#/components/dashboard/columnHeading'
import Label from '#/components/dashboard/Label'
import SelectionBrush from '#/components/SelectionBrush'
import Spinner, * as spinner from '#/components/Spinner'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'

import DragModal from '#/modals/DragModal'
import DuplicateAssetsModal from '#/modals/DuplicateAssetsModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'

import * as array from '#/utilities/array'
import type * as assetQuery from '#/utilities/AssetQuery'
import AssetQuery from '#/utilities/AssetQuery'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import AssetTreeNode from '#/utilities/AssetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as drag from '#/utilities/drag'
import * as fileInfo from '#/utilities/fileInfo'
import type * as geometry from '#/utilities/geometry'
import * as inputBindingsModule from '#/utilities/inputBindings'
import LocalStorage from '#/utilities/LocalStorage'
import type * as pasteDataModule from '#/utilities/pasteData'
import PasteType from '#/utilities/PasteType'
import * as permissions from '#/utilities/permissions'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import * as set from '#/utilities/set'
import * as sorting from '#/utilities/sorting'
import * as string from '#/utilities/string'
import * as uniqueString from '#/utilities/uniqueString'
import Visibility from '#/utilities/Visibility'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly enabledColumns: columnUtils.Column[]
  }
}

LocalStorage.registerKey('enabledColumns', {
  tryParse: value => {
    const possibleColumns = Array.isArray(value) ? value : []
    const values = possibleColumns.filter(array.includesPredicate(columnUtils.CLOUD_COLUMNS))
    return values.length === 0 ? null : values
  },
})

// =================
// === Constants ===
// =================

/** If the drag pointer is less than this distance away from the top or bottom of the
 * scroll container, then the scroll container automatically scrolls upwards if the cursor is near
 * the top of the scroll container, or downwards if the cursor is near the bottom. */
const AUTOSCROLL_THRESHOLD_PX = 50
/** An arbitrary constant that controls the speed of autoscroll. */
const AUTOSCROLL_SPEED = 100
/** The autoscroll speed is `AUTOSCROLL_SPEED / (distance + AUTOSCROLL_DAMPENING)`. */
const AUTOSCROLL_DAMPENING = 10
/** The height of the header row. */
const HEADER_HEIGHT_PX = 33
/** The height of each row in the table body. MUST be identical to the value as set by the
 * Tailwind styling. */
const ROW_HEIGHT_PX = 32
/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE_PX = 36
/** The number of pixels the header bar should shrink when the column selector is visible,
 * assuming 0 icons are visible in the column selector. */
const COLUMNS_SELECTOR_BASE_WIDTH_PX = 4
/** The number of pixels the header bar should shrink per collapsed column. */
const COLUMNS_SELECTOR_ICON_WIDTH_PX = 28

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
    render: () => 'type:secret',
    addToQuery: query => query.addToLastTerm({ types: ['secret'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['secret'] }),
  },
  {
    render: () => 'type:datalink',
    addToQuery: query => query.addToLastTerm({ types: ['datalink'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['datalink'] }),
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

// ===================================
// === insertAssetTreeNodeChildren ===
// ===================================

/** Return a directory, with new children added into its list of children.
 * All children MUST have the same asset type. */
function insertAssetTreeNodeChildren(
  item: assetTreeNode.AnyAssetTreeNode,
  children: backendModule.AnyAsset[],
  directoryKey: backendModule.DirectoryId,
  directoryId: backendModule.DirectoryId
): assetTreeNode.AnyAssetTreeNode {
  const depth = item.depth + 1
  const typeOrder = children[0] != null ? backendModule.ASSET_TYPE_ORDER[children[0].type] : 0
  const nodes = (item.children ?? []).filter(
    node => node.item.type !== backendModule.AssetType.specialEmpty
  )
  const nodesToInsert = children.map(asset =>
    AssetTreeNode.fromAsset(asset, directoryKey, directoryId, depth)
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
  item: assetTreeNode.AnyAssetTreeNode,
  children: backendModule.AnyAsset[],
  directoryKey: backendModule.DirectoryId,
  directoryId: backendModule.DirectoryId,
  getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
): assetTreeNode.AnyAssetTreeNode {
  const depth = item.depth + 1
  const nodes = (item.children ?? []).filter(
    node => node.item.type !== backendModule.AssetType.specialEmpty
  )
  const byType: Readonly<Record<backendModule.AssetType, backendModule.AnyAsset[]>> = {
    [backendModule.AssetType.directory]: [],
    [backendModule.AssetType.project]: [],
    [backendModule.AssetType.file]: [],
    [backendModule.AssetType.dataLink]: [],
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
        AssetTreeNode.fromAsset(
          asset,
          directoryKey,
          directoryId,
          depth,
          getKey?.(asset) ?? asset.id
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

// =========================
// === DragSelectionInfo ===
// =========================

/** Information related to a drag selection. */
interface DragSelectionInfo {
  readonly initialIndex: number
  readonly start: number
  readonly end: number
}

// =============================
// === Category to filter by ===
// =============================

const CATEGORY_TO_FILTER_BY: Readonly<Record<Category, backendModule.FilterBy | null>> = {
  [Category.recent]: null,
  [Category.home]: backendModule.FilterBy.active,
  [Category.trash]: backendModule.FilterBy.trashed,
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  readonly selectedKeys: React.MutableRefObject<ReadonlySet<backendModule.AssetId>>
  readonly scrollContainerRef: React.RefObject<HTMLElement>
  readonly visibilities: ReadonlyMap<backendModule.AssetId, Visibility>
  readonly category: Category
  readonly labels: Map<backendModule.LabelName, backendModule.Label>
  readonly deletedLabelNames: Set<backendModule.LabelName>
  readonly hasPasteData: boolean
  readonly setPasteData: (pasteData: pasteDataModule.PasteData<Set<backendModule.AssetId>>) => void
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly setSortInfo: (sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  readonly assetEvents: assetEvent.AssetEvent[]
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly nodeMap: Readonly<
    React.MutableRefObject<ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>>
  >
  readonly hideColumn: (column: columnUtils.Column) => void
  readonly doToggleDirectoryExpansion: (
    directoryId: backendModule.DirectoryId,
    key: backendModule.DirectoryId,
    title?: string | null,
    override?: boolean
  ) => void
  /** Called when the project is opened via the `ProjectActionButton`. */
  readonly doOpenManually: (projectId: backendModule.ProjectId) => void
  readonly doOpenEditor: (
    project: backendModule.ProjectAsset,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
  readonly doCreateLabel: (value: string, color: backendModule.LChColor) => Promise<void>
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId
  ) => void
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
  readonly setVisibility: (visibility: Visibility) => void
  readonly isEditingName: boolean
  readonly temporarilyAddedLabels: ReadonlySet<backendModule.LabelName>
  readonly temporarilyRemovedLabels: ReadonlySet<backendModule.LabelName>
}

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
  readonly hidden: boolean
  readonly hideRows: boolean
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setCanDownload: (canDownload: boolean) => void
  readonly category: Category
  readonly allLabels: Map<backendModule.LabelName, backendModule.Label>
  readonly setSuggestions: (suggestions: assetSearchBar.Suggestion[]) => void
  readonly initialProjectName: string | null
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly deletedLabelNames: Set<backendModule.LabelName>
  /** These events will be dispatched the next time the assets list is refreshed, rather than
   * immediately. */
  readonly queuedAssetEvents: assetEvent.AssetEvent[]
  readonly assetListEvents: assetListEvent.AssetListEvent[]
  readonly dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  readonly assetEvents: assetEvent.AssetEvent[]
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly targetDirectoryNodeRef: React.MutableRefObject<assetTreeNode.AnyAssetTreeNode<backendModule.DirectoryAsset> | null>
  readonly doOpenEditor: (
    project: backendModule.ProjectAsset,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
  readonly doCreateLabel: (value: string, color: backendModule.LChColor) => Promise<void>
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
  const { hidden, hideRows, query, setQuery, setCanDownload, category, allLabels } = props
  const { setSuggestions, deletedLabelNames, initialProjectName, projectStartupInfo } = props
  const { queuedAssetEvents: rawQueuedAssetEvents } = props
  const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
  const { setAssetPanelProps, doOpenEditor, doCloseEditor: rawDoCloseEditor, doCreateLabel } = props
  const { targetDirectoryNodeRef, setIsAssetPanelTemporarilyVisible } = props

  const { user, accessToken } = authProvider.useNonPartialUserSession()
  const { backend } = backendProvider.useBackend()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [initialized, setInitialized] = React.useState(false)
  const [isLoading, setIsLoading] = React.useState(true)
  const [enabledColumns, setEnabledColumns] = React.useState(columnUtils.DEFAULT_ENABLED_COLUMNS)
  const [sortInfo, setSortInfo] =
    React.useState<sorting.SortInfo<columnUtils.SortableColumn> | null>(null)
  const [selectedKeys, setSelectedKeysRaw] = React.useState<ReadonlySet<backendModule.AssetId>>(
    () => new Set()
  )
  const selectedKeysRef = React.useRef(selectedKeys)
  const [pasteData, setPasteData] = React.useState<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const [, setQueuedAssetEvents] = React.useState<assetEvent.AssetEvent[]>([])
  const [, setNameOfProjectToImmediatelyOpen] = React.useState(initialProjectName)
  const rootDirectoryId = React.useMemo(
    () => backend.rootDirectoryId(user) ?? backendModule.DirectoryId(''),
    [backend, user]
  )
  const [assetTree, setAssetTree] = React.useState<assetTreeNode.AnyAssetTreeNode>(() => {
    const rootParentDirectoryId = backendModule.DirectoryId('')
    return AssetTreeNode.fromAsset(
      backendModule.createRootDirectoryAsset(rootDirectoryId),
      rootParentDirectoryId,
      rootParentDirectoryId,
      -1
    )
  })
  const isCloud = backend.type === backendModule.BackendType.remote
  /** Events sent when the asset list was still loading. */
  const queuedAssetListEventsRef = React.useRef<assetListEvent.AssetListEvent[]>([])
  const rootRef = React.useRef<HTMLDivElement | null>(null)
  const headerRowRef = React.useRef<HTMLTableRowElement>(null)
  const assetTreeRef = React.useRef<assetTreeNode.AnyAssetTreeNode>(assetTree)
  const pasteDataRef = React.useRef<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const nodeMapRef = React.useRef<
    ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>
  >(new Map<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>())
  const filter = React.useMemo(() => {
    const globCache: Record<string, RegExp> = {}
    if (/^\s*$/.test(query.query)) {
      return null
    } else {
      return (node: assetTreeNode.AnyAssetTreeNode) => {
        if (
          node.item.type === backendModule.AssetType.specialEmpty ||
          node.item.type === backendModule.AssetType.specialLoading
        ) {
          // This is FINE, as these assets have no meaning info to match with.
          // eslint-disable-next-line no-restricted-syntax
          return false
        }
        const assetType =
          node.item.type === backendModule.AssetType.directory
            ? 'folder'
            : node.item.type === backendModule.AssetType.dataLink
              ? 'datalink'
              : String(node.item.type)
        const assetExtension =
          node.item.type !== backendModule.AssetType.file
            ? null
            : fileInfo.fileExtension(node.item.title).toLowerCase()
        const assetModifiedAt = new Date(node.item.modifiedAt)
        const labels: string[] = node.item.labels ?? []
        const lowercaseName = node.item.title.toLowerCase()
        const lowercaseDescription = node.item.description?.toLowerCase() ?? ''
        const owners =
          node.item.permissions
            ?.filter(permission => permission.permission === permissions.PermissionAction.own)
            .map(owner => owner.user.name) ?? []
        const globMatch = (glob: string, match: string) => {
          const regex = (globCache[glob] =
            globCache[glob] ??
            new RegExp('^' + string.regexEscape(glob).replace(/(?:\\\*)+/g, '.*') + '$', 'i'))
          return regex.test(match)
        }
        const isAbsent = (type: string) => {
          switch (type) {
            case 'label':
            case 'labels': {
              return labels.length === 0
            }
            case 'name': {
              // Should never be true, but handle it just in case.
              return lowercaseName === ''
            }
            case 'description': {
              return lowercaseDescription === ''
            }
            case 'extension': {
              // Should never be true, but handle it just in case.
              return assetExtension === ''
            }
          }
          // Things like `no:name` and `no:owner` are never true.
          return false
        }
        const parseDate = (date: string) => {
          const lowercase = date.toLowerCase()
          switch (lowercase) {
            case 'today': {
              return new Date()
            }
          }
          return new Date(date)
        }
        const matchesDate = (date: string) => {
          const parsed = parseDate(date)
          return (
            parsed.getFullYear() === assetModifiedAt.getFullYear() &&
            parsed.getMonth() === assetModifiedAt.getMonth() &&
            parsed.getDate() === assetModifiedAt.getDate()
          )
        }
        const isEmpty = (values: string[]) =>
          values.length === 0 || (values.length === 1 && values[0] === '')
        const filterTag = (
          positive: string[][],
          negative: string[][],
          predicate: (value: string) => boolean
        ) =>
          positive.every(values => isEmpty(values) || values.some(predicate)) &&
          negative.every(values => !values.some(predicate))
        return (
          filterTag(query.nos, query.negativeNos, no => isAbsent(no.toLowerCase())) &&
          filterTag(query.keywords, query.negativeKeywords, keyword =>
            lowercaseName.includes(keyword.toLowerCase())
          ) &&
          filterTag(query.names, query.negativeNames, name => globMatch(name, lowercaseName)) &&
          filterTag(query.labels, query.negativeLabels, label =>
            labels.some(assetLabel => globMatch(label, assetLabel))
          ) &&
          filterTag(query.types, query.negativeTypes, type => type === assetType) &&
          filterTag(
            query.extensions,
            query.negativeExtensions,
            extension => extension.toLowerCase() === assetExtension
          ) &&
          filterTag(query.descriptions, query.negativeDescriptions, description =>
            lowercaseDescription.includes(description.toLowerCase())
          ) &&
          filterTag(query.modifieds, query.negativeModifieds, matchesDate) &&
          filterTag(query.owners, query.negativeOwners, owner =>
            owners.some(assetOwner => globMatch(owner, assetOwner))
          )
        )
      }
    }
  }, [query])
  const displayItems = React.useMemo(() => {
    if (sortInfo == null) {
      return assetTree.preorderTraversal()
    } else {
      const multiplier = sortInfo.direction === sorting.SortDirection.ascending ? 1 : -1
      let compare: (a: assetTreeNode.AnyAssetTreeNode, b: assetTreeNode.AnyAssetTreeNode) => number
      switch (sortInfo.field) {
        case columnUtils.Column.name: {
          compare = (a, b) => {
            const aTitle = a.item.title.toLowerCase()
            const bTitle = b.item.title.toLowerCase()
            if (aTitle === bTitle) {
              const delta = a.item.title > b.item.title ? 1 : a.item.title < b.item.title ? -1 : 0
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
            const aOrder = Number(new Date(a.item.modifiedAt))
            const bOrder = Number(new Date(b.item.modifiedAt))
            return multiplier * (aOrder - bOrder)
          }
          break
        }
      }
      return assetTree.preorderTraversal(tree => [...tree].sort(compare))
    }
  }, [assetTree, sortInfo])
  const visibilities = React.useMemo(() => {
    const map = new Map<backendModule.AssetId, Visibility>()
    const processNode = (node: assetTreeNode.AnyAssetTreeNode) => {
      let displayState = Visibility.hidden
      const visible = filter?.(node) ?? true
      for (const child of node.children ?? []) {
        if (visible && child.item.type === backendModule.AssetType.specialEmpty) {
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
  const visibleItems = React.useMemo(
    () => displayItems.filter(item => visibilities.get(item.key) !== Visibility.hidden),
    [displayItems, visibilities]
  )

  React.useEffect(() => {
    if (selectedKeys.size === 0) {
      targetDirectoryNodeRef.current = null
    } else if (selectedKeys.size === 1) {
      const [soleKey] = selectedKeys
      const node = soleKey == null ? null : nodeMapRef.current.get(soleKey)
      if (node != null && node.isType(backendModule.AssetType.directory)) {
        targetDirectoryNodeRef.current = node
      }
    } else {
      let commonDirectoryKey: backendModule.AssetId | null = null
      let otherCandidateDirectoryKey: backendModule.AssetId | null = null
      for (const key of selectedKeys) {
        const node = nodeMapRef.current.get(key)
        if (node != null) {
          if (commonDirectoryKey == null) {
            commonDirectoryKey = node.directoryKey
            otherCandidateDirectoryKey =
              node.item.type === backendModule.AssetType.directory ? node.key : null
          } else if (node.key === commonDirectoryKey || node.directoryKey === commonDirectoryKey) {
            otherCandidateDirectoryKey = null
          } else if (
            otherCandidateDirectoryKey != null &&
            (node.key === otherCandidateDirectoryKey ||
              node.directoryKey === otherCandidateDirectoryKey)
          ) {
            commonDirectoryKey = otherCandidateDirectoryKey
            otherCandidateDirectoryKey = null
          } else {
            // No match; there is no common parent directory for the entire selection.
            commonDirectoryKey = null
            break
          }
        }
      }
      const node = commonDirectoryKey == null ? null : nodeMapRef.current.get(commonDirectoryKey)
      if (node != null && node.isType(backendModule.AssetType.directory)) {
        targetDirectoryNodeRef.current = node
      }
    }
  }, [targetDirectoryNodeRef, selectedKeys])

  React.useEffect(() => {
    const nodeToSuggestion = (
      node: assetTreeNode.AnyAssetTreeNode,
      key: assetQuery.AssetQueryKey = 'names'
    ): assetSearchBar.Suggestion => ({
      render: () => `${key === 'names' ? '' : '-:'}${node.item.title}`,
      addToQuery: oldQuery => oldQuery.addToLastTerm({ [key]: [node.item.title] }),
      deleteFromQuery: oldQuery => oldQuery.deleteFromLastTerm({ [key]: [node.item.title] }),
    })
    const allVisibleNodes = () =>
      assetTree
        .preorderTraversal(children =>
          children.filter(child => visibilities.get(child.key) !== Visibility.hidden)
        )
        .filter(
          node =>
            visibilities.get(node.key) === Visibility.visible &&
            node.item.type !== backendModule.AssetType.specialEmpty &&
            node.item.type !== backendModule.AssetType.specialLoading
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
          setSuggestions(isCloud ? SUGGESTIONS_FOR_NO : [])
          break
        }
        case 'has':
        case '-no': {
          setSuggestions(isCloud ? SUGGESTIONS_FOR_HAS : [])
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
            .filter(node => node.item.type === backendModule.AssetType.file)
            .map(node => fileInfo.fileExtension(node.item.title))
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
            const date = new Date(node.item.modifiedAt)
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
              (node.item.permissions ?? [])
                .filter(permission => permission.permission === permissions.PermissionAction.own)
                .map(permission => permission.user.name)
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
            !isCloud
              ? []
              : Array.from(
                  allLabels.values(),
                  (label): assetSearchBar.Suggestion => ({
                    render: () => (
                      <Label active color={label.color} onPress={() => {}}>
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
  }, [isCloud, assetTree, query, visibilities, allLabels, /* should never change */ setSuggestions])

  React.useEffect(() => {
    if (rawQueuedAssetEvents.length !== 0) {
      setQueuedAssetEvents(oldEvents => [...oldEvents, ...rawQueuedAssetEvents])
    }
  }, [rawQueuedAssetEvents])

  React.useEffect(() => {
    setIsLoading(true)
  }, [backend, category])

  React.useEffect(() => {
    assetTreeRef.current = assetTree
    const newNodeMap = new Map(assetTree.preorderTraversal().map(asset => [asset.key, asset]))
    newNodeMap.set(assetTree.key, assetTree)
    nodeMapRef.current = newNodeMap
  }, [assetTree])

  React.useEffect(() => {
    pasteDataRef.current = pasteData
  }, [pasteData])

  React.useEffect(() => {
    if (!hidden) {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        cancelCut: () => {
          if (pasteDataRef.current == null) {
            return false
          } else {
            dispatchAssetEvent({ type: AssetEventType.cancelCut, ids: pasteDataRef.current.data })
            setPasteData(null)
            return
          }
        },
      })
    }
  }, [
    hidden,
    /* should never change */ inputBindings,
    /* should never change */ dispatchAssetEvent,
  ])

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
        window.setTimeout(() => {
          dispatchAssetEvent({
            type: AssetEventType.openProject,
            id: projectToLoad.id,
            shouldAutomaticallySwitchPage: true,
            runInBackground: false,
          })
        })
      } else if (initialProjectName != null) {
        toastAndLog('findProjectError', null, initialProjectName)
      }
    }
    // This effect MUST only run when `initialProjectName` is changed.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [initialProjectName])

  const setSelectedKeys = React.useCallback(
    (newSelectedKeys: ReadonlySet<backendModule.AssetId>) => {
      selectedKeysRef.current = newSelectedKeys
      setSelectedKeysRaw(newSelectedKeys)
      if (!isCloud) {
        setCanDownload(
          newSelectedKeys.size !== 0 &&
            Array.from(newSelectedKeys).every(key => {
              const node = nodeMapRef.current.get(key)
              return node?.item.type === backendModule.AssetType.project
            })
        )
      } else {
        setCanDownload(
          newSelectedKeys.size !== 0 &&
            Array.from(newSelectedKeys).every(key => {
              const node = nodeMapRef.current.get(key)
              return (
                node?.item.type === backendModule.AssetType.project ||
                node?.item.type === backendModule.AssetType.file ||
                node?.item.type === backendModule.AssetType.dataLink
              )
            })
        )
      }
    },
    [isCloud, /* should never change */ setCanDownload]
  )

  const clearSelectedKeys = React.useCallback(() => {
    setSelectedKeys(new Set())
  }, [setSelectedKeys])

  const overwriteNodes = React.useCallback(
    (newAssets: backendModule.AnyAsset[]) => {
      mostRecentlySelectedIndexRef.current = null
      selectionStartIndexRef.current = null
      // This is required, otherwise we are using an outdated
      // `nameOfProjectToImmediatelyOpen`.
      setNameOfProjectToImmediatelyOpen(oldNameOfProjectToImmediatelyOpen => {
        setInitialized(true)
        const rootParentDirectoryId = backendModule.DirectoryId('')
        const rootDirectory = backendModule.createRootDirectoryAsset(rootDirectoryId)
        const newRootNode = new AssetTreeNode(
          rootDirectory,
          rootParentDirectoryId,
          rootParentDirectoryId,
          newAssets.map(asset =>
            AssetTreeNode.fromAsset(asset, rootDirectory.id, rootDirectory.id, 0)
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
            window.setTimeout(() => {
              dispatchAssetEvent({
                type: AssetEventType.openProject,
                id: projectToLoad.id,
                shouldAutomaticallySwitchPage: true,
                runInBackground: false,
              })
            })
          } else {
            toastAndLog('findProjectError', null, oldNameOfProjectToImmediatelyOpen)
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
      toastAndLog,
      /* should never change */ setNameOfProjectToImmediatelyOpen,
      /* should never change */ dispatchAssetEvent,
    ]
  )

  React.useEffect(() => {
    if (initialized) {
      overwriteNodes([])
    }
    // `overwriteAssets` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [backend, category])

  asyncEffectHooks.useAsyncEffect(
    null,
    async signal => {
      setSelectedKeys(new Set())
      try {
        const newAssets = await backend
          .listDirectory(
            {
              parentId: null,
              filterBy: CATEGORY_TO_FILTER_BY[category],
              recentProjects: category === Category.recent,
              labels: null,
            },
            // The root directory has no name. This is also SAFE, as there is a different error
            // message when the directory is the root directory (when `parentId == null`).
            '(root)'
          )
          .catch(error => {
            toastAndLog('listRootFolderBackendError', error)
            throw error
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
    },
    [category, accessToken, user, backend, setSelectedKeys]
  )

  React.useEffect(() => {
    const savedEnabledColumns = localStorage.get('enabledColumns')
    if (savedEnabledColumns != null) {
      setEnabledColumns(new Set(savedEnabledColumns))
    }
  }, [/* should never change */ localStorage])

  React.useEffect(() => {
    if (initialized) {
      localStorage.set('enabledColumns', [...enabledColumns])
    }
  }, [enabledColumns, initialized, /* should never change */ localStorage])

  React.useEffect(() => {
    if (selectedKeysRef.current.size !== 1) {
      setAssetPanelProps(null)
      setIsAssetPanelTemporarilyVisible(false)
    }
  }, [
    selectedKeysRef.current.size,
    /* should never change */ setAssetPanelProps,
    /* should never change */ setIsAssetPanelTemporarilyVisible,
  ])

  const directoryListAbortControllersRef = React.useRef(
    new Map<backendModule.DirectoryId, AbortController>()
  )
  const doToggleDirectoryExpansion = React.useCallback(
    (
      directoryId: backendModule.DirectoryId,
      key: backendModule.DirectoryId,
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
          oldAssetTree.map(item => (item.key !== key ? item : item.with({ children: null })))
        )
      } else {
        setAssetTree(oldAssetTree =>
          oldAssetTree.map(item =>
            item.key !== key
              ? item
              : item.with({
                  children: [
                    AssetTreeNode.fromAsset(
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
          const displayedTitle = title ?? nodeMapRef.current.get(key)?.item.title ?? '(unknown)'
          const childAssets = await backend
            .listDirectory(
              {
                parentId: directoryId,
                filterBy: CATEGORY_TO_FILTER_BY[category],
                recentProjects: category === Category.recent,
                labels: null,
              },
              displayedTitle
            )
            .catch(error => {
              toastAndLog('listFolderBackendError', error, displayedTitle)
              throw error
            })
          if (!abortController.signal.aborted) {
            setAssetTree(oldAssetTree =>
              oldAssetTree.map(item => {
                if (item.key !== key) {
                  return item
                } else {
                  const initialChildren = item.children?.filter(
                    child => child.item.type !== backendModule.AssetType.specialLoading
                  )
                  const childAssetsMap = new Map(childAssets.map(asset => [asset.id, asset]))
                  for (const child of initialChildren ?? []) {
                    const newChild = childAssetsMap.get(child.item.id)
                    if (newChild != null) {
                      child.item = newChild
                      childAssetsMap.delete(child.item.id)
                    }
                  }
                  const childAssetNodes = Array.from(childAssetsMap.values(), child =>
                    AssetTreeNode.fromAsset(child, key, directoryId, item.depth + 1)
                  )
                  const specialEmptyAsset: backendModule.SpecialEmptyAsset | null =
                    (initialChildren != null && initialChildren.length !== 0) ||
                    childAssetNodes.length !== 0
                      ? null
                      : backendModule.createSpecialEmptyAsset(directoryId)
                  const children =
                    specialEmptyAsset != null
                      ? [
                          AssetTreeNode.fromAsset(
                            specialEmptyAsset,
                            key,
                            directoryId,
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
    [category, backend, toastAndLog]
  )

  const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
  const [keyboardSelectedIndex, setKeyboardSelectedIndex] = React.useState<number | null>(null)
  const mostRecentlySelectedIndexRef = React.useRef<number | null>(null)
  const selectionStartIndexRef = React.useRef<number | null>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)

  const setMostRecentlySelectedIndex = React.useCallback(
    (index: number | null, isKeyboard = false) => {
      mostRecentlySelectedIndexRef.current = index
      setKeyboardSelectedIndex(isKeyboard ? index : null)
    },
    []
  )

  React.useEffect(() => {
    const body = bodyRef.current
    if (body == null) {
      return
    } else {
      return navigator2D.register(body, {
        focusPrimaryChild: () => {
          setMostRecentlySelectedIndex(0, true)
        },
      })
    }
  }, [navigator2D, setMostRecentlySelectedIndex])

  // This is not a React component, even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const onKeyDown = (event: React.KeyboardEvent) => {
    const prevIndex = mostRecentlySelectedIndexRef.current
    const item = prevIndex == null ? null : visibleItems[prevIndex]
    if (selectedKeysRef.current.size === 1 && item != null) {
      switch (event.key) {
        case 'Enter':
        case ' ': {
          if (event.key === ' ' && event.ctrlKey) {
            const keys = selectedKeysRef.current
            setSelectedKeys(set.withPresence(keys, item.key, !keys.has(item.key)))
          } else {
            switch (item.type) {
              case backendModule.AssetType.directory: {
                event.preventDefault()
                event.stopPropagation()
                doToggleDirectoryExpansion(item.item.id, item.key)
                break
              }
              case backendModule.AssetType.project: {
                event.preventDefault()
                event.stopPropagation()
                dispatchAssetEvent({
                  type: AssetEventType.openProject,
                  id: item.item.id,
                  runInBackground: false,
                  shouldAutomaticallySwitchPage: true,
                })
                break
              }
              case backendModule.AssetType.dataLink: {
                event.preventDefault()
                event.stopPropagation()
                setIsAssetPanelTemporarilyVisible(true)
                break
              }
              case backendModule.AssetType.secret: {
                event.preventDefault()
                event.stopPropagation()
                const id = item.item.id
                setModal(
                  <UpsertSecretModal
                    id={item.item.id}
                    name={item.item.title}
                    doCreate={async (_name, value) => {
                      try {
                        await backend.updateSecret(id, { value }, item.item.title)
                      } catch (error) {
                        toastAndLog(null, error)
                      }
                    }}
                  />
                )
                break
              }
              default: {
                break
              }
            }
          }
          break
        }
        case 'ArrowLeft': {
          if (item.type === backendModule.AssetType.directory) {
            if (item.children != null) {
              // The folder is expanded; collapse it.
              event.preventDefault()
              event.stopPropagation()
              doToggleDirectoryExpansion(item.item.id, item.key, null, false)
            } else if (prevIndex != null) {
              // Focus parent if there is one.
              let index = prevIndex - 1
              let possibleParent = visibleItems[index]
              while (possibleParent != null && index >= 0) {
                if (possibleParent.depth < item.depth) {
                  event.preventDefault()
                  event.stopPropagation()
                  setSelectedKeys(new Set([possibleParent.key]))
                  setMostRecentlySelectedIndex(index, true)
                  break
                }
                index -= 1
                possibleParent = visibleItems[index]
              }
            }
          }
          break
        }
        case 'ArrowRight': {
          if (item.type === backendModule.AssetType.directory && item.children == null) {
            // The folder is collapsed; expand it.
            event.preventDefault()
            event.stopPropagation()
            doToggleDirectoryExpansion(item.item.id, item.key, null, true)
          }
          break
        }
      }
    }
    switch (event.key) {
      case ' ': {
        if (event.ctrlKey && item != null) {
          const keys = selectedKeysRef.current
          setSelectedKeys(set.withPresence(keys, item.key, !keys.has(item.key)))
        }
        break
      }
      case 'Escape': {
        setSelectedKeys(new Set())
        setMostRecentlySelectedIndex(null)
        selectionStartIndexRef.current = null
        break
      }
      case 'ArrowUp':
      case 'ArrowDown': {
        if (!event.shiftKey) {
          selectionStartIndexRef.current = null
        }
        let index = prevIndex ?? 0
        let oldIndex = index
        if (prevIndex != null) {
          let itemType = visibleItems[index]?.item.type
          do {
            oldIndex = index
            index =
              event.key === 'ArrowUp'
                ? Math.max(0, index - 1)
                : Math.min(visibleItems.length - 1, index + 1)
            itemType = visibleItems[index]?.item.type
          } while (
            index !== oldIndex &&
            (itemType === backendModule.AssetType.specialEmpty ||
              itemType === backendModule.AssetType.specialLoading)
          )
          if (
            itemType === backendModule.AssetType.specialEmpty ||
            itemType === backendModule.AssetType.specialLoading
          ) {
            index = prevIndex
          }
        }
        setMostRecentlySelectedIndex(index, true)
        if (event.shiftKey) {
          event.preventDefault()
          event.stopPropagation()
          // On Windows, Ctrl+Shift+Arrow behaves the same as Shift+Arrow.
          if (selectionStartIndexRef.current == null) {
            selectionStartIndexRef.current = prevIndex ?? 0
          }
          const startIndex = Math.min(index, selectionStartIndexRef.current)
          const endIndex = Math.max(index, selectionStartIndexRef.current) + 1
          const selection = visibleItems.slice(startIndex, endIndex)
          setSelectedKeys(new Set(selection.map(newItem => newItem.key)))
        } else if (event.ctrlKey) {
          event.preventDefault()
          event.stopPropagation()
          selectionStartIndexRef.current = null
        } else if (index !== prevIndex) {
          event.preventDefault()
          event.stopPropagation()
          const newItem = visibleItems[index]
          if (newItem != null) {
            setSelectedKeys(new Set([newItem.key]))
          }
          selectionStartIndexRef.current = null
        } else {
          // The arrow key will escape this container. In that case, do not stop propagation
          // and let `navigator2D` navigate to a different container.
          setSelectedKeys(new Set())
          selectionStartIndexRef.current = null
        }
        break
      }
    }
  }

  React.useEffect(() => {
    const onClick = () => {
      setKeyboardSelectedIndex(null)
    }

    document.addEventListener('click', onClick, { capture: true })
    return () => {
      document.removeEventListener('click', onClick, { capture: true })
    }
  }, [setMostRecentlySelectedIndex])

  const getNewProjectName = React.useCallback(
    (templateName: string | null, parentKey: backendModule.DirectoryId | null) => {
      const prefix = `${templateName ?? 'New Project'} `
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
      parentKey: backendModule.DirectoryId | null,
      parentId: backendModule.DirectoryId | null
    ) => {
      const actualParentKey = parentKey ?? rootDirectoryId
      const actualParentId = parentId ?? rootDirectoryId
      setAssetTree(oldAssetTree =>
        oldAssetTree.map(item =>
          item.key !== actualParentKey
            ? item
            : insertAssetTreeNodeChildren(item, assets, actualParentKey, actualParentId)
        )
      )
    },
    [rootDirectoryId]
  )

  const insertArbitraryAssets = React.useCallback(
    (
      assets: backendModule.AnyAsset[],
      parentKey: backendModule.DirectoryId | null,
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

  // This is not a React component, even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const onAssetListEvent = (event: assetListEvent.AssetListEvent) => {
    switch (event.type) {
      case AssetListEventType.newFolder: {
        const siblings = nodeMapRef.current.get(event.parentKey)?.children ?? []
        const directoryIndices = siblings
          .map(node => node.item)
          .filter(backendModule.assetIsDirectory)
          .map(item => /^New Folder (?<directoryIndex>\d+)$/.exec(item.title))
          .map(match => match?.groups?.directoryIndex)
          .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        const title = `New Folder ${Math.max(0, ...directoryIndices) + 1}`
        const placeholderItem: backendModule.DirectoryAsset = {
          type: backendModule.AssetType.directory,
          id: backendModule.DirectoryId(uniqueString.uniqueString()),
          title,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryGetSingletonOwnerPermission(user),
          projectState: null,
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId)
        dispatchAssetEvent({
          type: AssetEventType.newFolder,
          placeholderId: placeholderItem.id,
        })
        break
      }
      case AssetListEventType.newProject: {
        const projectName = getNewProjectName(event.preferredName, event.parentId)
        const dummyId = backendModule.ProjectId(uniqueString.uniqueString())
        const path =
          backend instanceof LocalBackend ? backend.joinPath(event.parentId, projectName) : null
        const placeholderItem: backendModule.ProjectAsset = {
          type: backendModule.AssetType.project,
          id: dummyId,
          title: projectName,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryGetSingletonOwnerPermission(user),
          projectState: {
            type: backendModule.ProjectState.placeholder,
            volumeId: '',
            ...(user != null ? { openedBy: user.email } : {}),
            ...(path != null ? { path } : {}),
          },
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId)
        dispatchAssetEvent({
          type: AssetEventType.newProject,
          placeholderId: dummyId,
          templateId: event.templateId,
          datalinkId: event.datalinkId,
          onSpinnerStateChange: event.onSpinnerStateChange,
        })
        break
      }
      case AssetListEventType.uploadFiles: {
        const localBackend = backend instanceof LocalBackend ? backend : null
        const reversedFiles = Array.from(event.files).reverse()
        const siblingNodes = nodeMapRef.current.get(event.parentKey)?.children ?? []
        const siblings = siblingNodes.map(node => node.item)
        const siblingFiles = siblings.filter(backendModule.assetIsFile)
        const siblingProjects = siblings.filter(backendModule.assetIsProject)
        const siblingFileTitles = new Set(siblingFiles.map(asset => asset.title))
        const siblingProjectTitles = new Set(siblingProjects.map(asset => asset.title))
        const files = reversedFiles.filter(backendModule.fileIsNotProject)
        const projects = reversedFiles.filter(backendModule.fileIsProject)
        const duplicateFiles = files.filter(file => siblingFileTitles.has(file.name))
        const duplicateProjects = projects.filter(project =>
          siblingProjectTitles.has(backendModule.stripProjectExtension(project.name))
        )
        const ownerPermission = permissions.tryGetSingletonOwnerPermission(user)
        if (duplicateFiles.length === 0 && duplicateProjects.length === 0) {
          const placeholderFiles = files.map(file =>
            backendModule.createPlaceholderFileAsset(file.name, event.parentId, ownerPermission)
          )
          const placeholderProjects = projects.map(project => {
            const basename = backendModule.stripProjectExtension(project.name)
            return backendModule.createPlaceholderProjectAsset(
              basename,
              event.parentId,
              ownerPermission,
              user,
              localBackend?.joinPath(event.parentId, basename) ?? null
            )
          })
          doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
          insertAssets(placeholderFiles, event.parentKey, event.parentId)
          insertAssets(placeholderProjects, event.parentKey, event.parentId)
          dispatchAssetEvent({
            type: AssetEventType.uploadFiles,
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
        } else {
          const siblingFilesByName = new Map(siblingFiles.map(file => [file.title, file]))
          const siblingProjectsByName = new Map(
            siblingProjects.map(project => [project.title, project])
          )
          const conflictingFiles = duplicateFiles.map(file => ({
            // This is SAFE, as `duplicateFiles` only contains files that have siblings
            // with the same name.
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            current: siblingFilesByName.get(file.name)!,
            new: backendModule.createPlaceholderFileAsset(
              file.name,
              event.parentId,
              ownerPermission
            ),
            file,
          }))
          const conflictingProjects = duplicateProjects.map(project => {
            const basename = backendModule.stripProjectExtension(project.name)
            return {
              // This is SAFE, as `duplicateProjects` only contains projects that have
              // siblings with the same name.
              // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
              current: siblingProjectsByName.get(basename)!,
              new: backendModule.createPlaceholderProjectAsset(
                basename,
                event.parentId,
                ownerPermission,
                user,
                localBackend?.joinPath(event.parentId, basename) ?? null
              ),
              file: project,
            }
          })
          setModal(
            <DuplicateAssetsModal
              parentKey={event.parentKey}
              parentId={event.parentId}
              conflictingFiles={conflictingFiles}
              conflictingProjects={conflictingProjects}
              dispatchAssetEvent={dispatchAssetEvent}
              dispatchAssetListEvent={dispatchAssetListEvent}
              siblingFileNames={siblingFilesByName.keys()}
              siblingProjectNames={siblingProjectsByName.keys()}
              nonConflictingFileCount={files.length - conflictingFiles.length}
              nonConflictingProjectCount={projects.length - conflictingProjects.length}
              doUploadNonConflicting={() => {
                doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
                const fileMap = new Map<backendModule.AssetId, File>()
                const newFiles = files
                  .filter(file => !siblingFileTitles.has(file.name))
                  .map(file => {
                    const asset = backendModule.createPlaceholderFileAsset(
                      file.name,
                      event.parentId,
                      ownerPermission
                    )
                    fileMap.set(asset.id, file)
                    return asset
                  })
                const newProjects = projects
                  .filter(
                    project =>
                      !siblingProjectTitles.has(backendModule.stripProjectExtension(project.name))
                  )
                  .map(project => {
                    const basename = backendModule.stripProjectExtension(project.name)
                    const asset = backendModule.createPlaceholderProjectAsset(
                      basename,
                      event.parentId,
                      ownerPermission,
                      user,
                      localBackend?.joinPath(event.parentId, basename) ?? null
                    )
                    fileMap.set(asset.id, project)
                    return asset
                  })
                insertAssets(newFiles, event.parentKey, event.parentId)
                insertAssets(newProjects, event.parentKey, event.parentId)
                dispatchAssetEvent({
                  type: AssetEventType.uploadFiles,
                  files: fileMap,
                })
              }}
            />
          )
        }
        break
      }
      case AssetListEventType.newDataLink: {
        const placeholderItem: backendModule.DataLinkAsset = {
          type: backendModule.AssetType.dataLink,
          id: backendModule.ConnectorId(uniqueString.uniqueString()),
          title: event.name,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryGetSingletonOwnerPermission(user),
          projectState: null,
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId)
        dispatchAssetEvent({
          type: AssetEventType.newDataLink,
          placeholderId: placeholderItem.id,
          value: event.value,
        })
        break
      }
      case AssetListEventType.newSecret: {
        const placeholderItem: backendModule.SecretAsset = {
          type: backendModule.AssetType.secret,
          id: backendModule.SecretId(uniqueString.uniqueString()),
          title: event.name,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryGetSingletonOwnerPermission(user),
          projectState: null,
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId)
        dispatchAssetEvent({
          type: AssetEventType.newSecret,
          placeholderId: placeholderItem.id,
          value: event.value,
        })
        break
      }
      case AssetListEventType.insertAssets: {
        insertArbitraryAssets(event.assets, event.parentKey, event.parentId)
        break
      }
      case AssetListEventType.willDelete: {
        if (selectedKeysRef.current.has(event.key)) {
          const newSelectedKeys = new Set(selectedKeysRef.current)
          newSelectedKeys.delete(event.key)
          setSelectedKeys(newSelectedKeys)
        }
        break
      }
      case AssetListEventType.copy: {
        const ids = new Set<backendModule.AssetId>()
        const getKey = (asset: backendModule.AnyAsset) => {
          const newId = backendModule.createPlaceholderAssetId(asset.type)
          ids.add(newId)
          return newId
        }
        insertArbitraryAssets(event.items, event.newParentKey, event.newParentId, getKey)
        dispatchAssetEvent({
          type: AssetEventType.copy,
          ids,
          newParentKey: event.newParentKey,
          newParentId: event.newParentId,
        })
        break
      }
      case AssetListEventType.move: {
        deleteAsset(event.key)
        insertAssets([event.item], event.newParentKey, event.newParentId)
        break
      }
      case AssetListEventType.delete: {
        deleteAsset(event.key)
        break
      }
      case AssetListEventType.emptyTrash: {
        if (category !== Category.trash) {
          toastAndLog('canOnlyEmptyTrashWhenInTrash')
        } else if (assetTree.children != null) {
          const ids = new Set(assetTree.children.map(child => child.item.id))
          // This is required to prevent an infinite loop,
          window.setTimeout(() => {
            dispatchAssetEvent({ type: AssetEventType.deleteForever, ids })
          })
        }
        break
      }
      case AssetListEventType.removeSelf: {
        dispatchAssetEvent({ type: AssetEventType.removeSelf, id: event.id })
        break
      }
      case AssetListEventType.closeFolder: {
        doToggleDirectoryExpansion(event.id, event.key, null, false)
        break
      }
    }
  }
  const onAssetListEventRef = React.useRef(onAssetListEvent)
  onAssetListEventRef.current = onAssetListEvent
  eventHooks.useEventHandler(assetListEvents, event => {
    if (!isLoading) {
      onAssetListEvent(event)
    } else {
      queuedAssetListEventsRef.current.push(event)
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
      if (project.id === projectStartupInfo?.projectAsset.id) {
        rawDoCloseEditor(project)
      }
    },
    [projectStartupInfo, rawDoCloseEditor]
  )

  const doCopy = React.useCallback(() => {
    unsetModal()
    setPasteData({ type: PasteType.copy, data: selectedKeysRef.current })
  }, [/* should never change */ unsetModal])

  const doCut = React.useCallback(() => {
    unsetModal()
    if (pasteData != null) {
      dispatchAssetEvent({ type: AssetEventType.cancelCut, ids: pasteData.data })
    }
    setPasteData({ type: PasteType.move, data: selectedKeysRef.current })
    dispatchAssetEvent({ type: AssetEventType.cut, ids: selectedKeysRef.current })
    setSelectedKeys(new Set())
  }, [
    pasteData,
    setSelectedKeys,
    /* should never change */ unsetModal,
    /* should never change */ dispatchAssetEvent,
  ])

  const doPaste = React.useCallback(
    (newParentKey: backendModule.DirectoryId, newParentId: backendModule.DirectoryId) => {
      unsetModal()
      if (pasteData != null) {
        if (pasteData.data.has(newParentKey)) {
          toast.toast.error('Cannot paste a folder into itself.')
        } else {
          doToggleDirectoryExpansion(newParentId, newParentKey, null, true)
          if (pasteData.type === PasteType.copy) {
            const assets = Array.from(pasteData.data, id => nodeMapRef.current.get(id)).flatMap(
              asset => (asset ? [asset.item] : [])
            )
            dispatchAssetListEvent({
              type: AssetListEventType.copy,
              items: assets,
              newParentId,
              newParentKey,
            })
          } else {
            dispatchAssetEvent({
              type: AssetEventType.move,
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
      doToggleDirectoryExpansion,
      /* should never change */ unsetModal,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const hideColumn = React.useCallback((column: columnUtils.Column) => {
    setEnabledColumns(columns => set.withPresence(columns, column, false))
  }, [])

  const hiddenContextMenu = React.useMemo(
    () => (
      <AssetsTableContextMenu
        hidden
        category={category}
        pasteData={pasteData}
        selectedKeys={selectedKeys}
        clearSelectedKeys={clearSelectedKeys}
        nodeMapRef={nodeMapRef}
        event={{ pageX: 0, pageY: 0 }}
        dispatchAssetEvent={dispatchAssetEvent}
        dispatchAssetListEvent={dispatchAssetListEvent}
        doCopy={doCopy}
        doCut={doCut}
        doPaste={doPaste}
      />
    ),
    [
      category,
      selectedKeys,
      pasteData,
      doCopy,
      doCut,
      doPaste,
      /* should never change */ clearSelectedKeys,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const onDragOver = (event: React.DragEvent<Element>) => {
    const payload = drag.ASSET_ROWS.lookup(event)
    const filtered = payload?.filter(item => item.asset.parentId !== rootDirectoryId)
    if ((filtered != null && filtered.length > 0) || event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
    }
  }

  const state = React.useMemo<AssetsTableState>(
    // The type MUST be here to trigger excess property errors at typecheck time.
    () => ({
      visibilities,
      selectedKeys: selectedKeysRef,
      scrollContainerRef: rootRef,
      category,
      labels: allLabels,
      deletedLabelNames,
      hasPasteData: pasteData != null,
      setPasteData,
      sortInfo,
      setSortInfo,
      query,
      setQuery,
      assetEvents,
      dispatchAssetEvent,
      dispatchAssetListEvent,
      setAssetPanelProps,
      setIsAssetPanelTemporarilyVisible,
      nodeMap: nodeMapRef,
      hideColumn,
      doToggleDirectoryExpansion,
      doOpenManually,
      doOpenEditor: doOpenEditor,
      doCloseEditor: doCloseEditor,
      doCreateLabel,
      doCopy,
      doCut,
      doPaste,
    }),
    [
      visibilities,
      category,
      allLabels,
      deletedLabelNames,
      pasteData,
      sortInfo,
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
      /* should never change */ hideColumn,
      /* should never change */ setAssetPanelProps,
      /* should never change */ setIsAssetPanelTemporarilyVisible,
      /* should never change */ setQuery,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  // This is required to prevent the table body from overlapping the table header, because
  // the table header is transparent.
  const onScroll = scrollHooks.useOnScroll(() => {
    if (bodyRef.current != null && rootRef.current != null) {
      bodyRef.current.style.clipPath = `inset(${rootRef.current.scrollTop}px 0 0 0)`
    }
    if (
      backend.type === backendModule.BackendType.remote &&
      rootRef.current != null &&
      headerRowRef.current != null
    ) {
      const hiddenColumnsCount = columnUtils.CLOUD_COLUMNS.length - enabledColumns.size
      const shrinkBy =
        COLUMNS_SELECTOR_BASE_WIDTH_PX + COLUMNS_SELECTOR_ICON_WIDTH_PX * hiddenColumnsCount
      const rightOffset = rootRef.current.clientWidth + rootRef.current.scrollLeft - shrinkBy
      headerRowRef.current.style.clipPath = `polygon(0 0, ${rightOffset}px 0, ${rightOffset}px 100%, 0 100%)`
    }
  }, [enabledColumns.size])

  React.useEffect(
    () =>
      inputBindings.attach(
        sanitizedEventTargets.document.body,
        'click',
        {
          selectAdditional: () => {},
          selectAdditionalRange: () => {},
          [inputBindingsModule.DEFAULT_HANDLER]: () => {
            if (selectedKeysRef.current.size !== 0) {
              setSelectedKeys(new Set())
              setMostRecentlySelectedIndex(null)
            }
          },
        },
        false
      ),
    [
      setSelectedKeys,
      /* should never change */ inputBindings,
      /* should never change */ setMostRecentlySelectedIndex,
    ]
  )

  React.useEffect(() => {
    if (isLoading) {
      // Ensure the spinner stays in the "initial" state for at least one frame,
      // to ensure the CSS animation begins at the initial state.
      requestAnimationFrame(() => {
        setSpinnerState(spinner.SpinnerState.loadingFast)
      })
    } else {
      const queuedAssetEvents = queuedAssetListEventsRef.current
      if (queuedAssetEvents.length !== 0) {
        queuedAssetListEventsRef.current = []
        for (const event of queuedAssetEvents) {
          onAssetListEventRef.current(event)
        }
      }
      setSpinnerState(spinner.SpinnerState.initial)
    }
  }, [isLoading])

  const calculateNewKeys = React.useCallback(
    (
      event: MouseEvent | React.MouseEvent,
      keys: backendModule.AssetId[],
      getRange: () => backendModule.AssetId[]
    ) => {
      event.stopPropagation()
      let result = new Set<backendModule.AssetId>()
      inputBindings.handler({
        selectRange: () => {
          result = new Set(getRange())
        },
        selectAdditionalRange: () => {
          result = new Set([...selectedKeysRef.current, ...getRange()])
        },
        selectAdditional: () => {
          const newSelectedKeys = new Set(selectedKeysRef.current)
          let count = 0
          for (const key of keys) {
            if (selectedKeysRef.current.has(key)) {
              count += 1
            }
          }
          for (const key of keys) {
            const add = count * 2 < keys.length
            set.setPresence(newSelectedKeys, key, add)
          }
          result = newSelectedKeys
        },
        [inputBindingsModule.DEFAULT_HANDLER]: () => {
          result = new Set(keys)
        },
      })(event, false)
      return result
    },
    [/* should never change */ inputBindings]
  )

  // Only non-`null` when it is different to`selectedKeys`.
  const [visuallySelectedKeysOverride, setVisuallySelectedKeysOverride] =
    React.useState<ReadonlySet<backendModule.AssetId> | null>(null)

  const dragSelectionChangeLoopHandle = React.useRef(0)
  const dragSelectionRangeRef = React.useRef<DragSelectionInfo | null>(null)
  const onSelectionDrag = React.useCallback(
    (rectangle: geometry.DetailedRectangle, event: MouseEvent) => {
      if (mostRecentlySelectedIndexRef.current != null) {
        setKeyboardSelectedIndex(null)
      }
      cancelAnimationFrame(dragSelectionChangeLoopHandle.current)
      const scrollContainer = rootRef.current
      if (scrollContainer != null) {
        const rect = scrollContainer.getBoundingClientRect()
        if (rectangle.signedHeight <= 0 && scrollContainer.scrollTop > 0) {
          const distanceToTop = Math.max(0, rectangle.top - rect.top - HEADER_HEIGHT_PX)
          if (distanceToTop < AUTOSCROLL_THRESHOLD_PX) {
            scrollContainer.scrollTop -= Math.floor(
              AUTOSCROLL_SPEED / (distanceToTop + AUTOSCROLL_DAMPENING)
            )
            dragSelectionChangeLoopHandle.current = requestAnimationFrame(() => {
              onSelectionDrag(rectangle, event)
            })
          }
        }
        if (
          rectangle.signedHeight >= 0 &&
          scrollContainer.scrollTop + rect.height < scrollContainer.scrollHeight
        ) {
          const distanceToBottom = Math.max(0, rect.bottom - rectangle.bottom)
          if (distanceToBottom < AUTOSCROLL_THRESHOLD_PX) {
            scrollContainer.scrollTop += Math.floor(
              AUTOSCROLL_SPEED / (distanceToBottom + AUTOSCROLL_DAMPENING)
            )
            dragSelectionChangeLoopHandle.current = requestAnimationFrame(() => {
              onSelectionDrag(rectangle, event)
            })
          }
        }
        const overlapsHorizontally = rect.right > rectangle.left && rect.left < rectangle.right
        const selectionTop = Math.max(0, rectangle.top - rect.top - HEADER_HEIGHT_PX)
        const selectionBottom = Math.max(
          0,
          Math.min(rect.height, rectangle.bottom - rect.top - HEADER_HEIGHT_PX)
        )
        const range = dragSelectionRangeRef.current
        if (!overlapsHorizontally) {
          dragSelectionRangeRef.current = null
        } else if (range == null) {
          const topIndex = (selectionTop + scrollContainer.scrollTop) / ROW_HEIGHT_PX
          const bottomIndex = (selectionBottom + scrollContainer.scrollTop) / ROW_HEIGHT_PX
          dragSelectionRangeRef.current = {
            initialIndex: rectangle.signedHeight < 0 ? bottomIndex : topIndex,
            start: Math.floor(topIndex),
            end: Math.ceil(bottomIndex),
          }
        } else {
          const topIndex = (selectionTop + scrollContainer.scrollTop) / ROW_HEIGHT_PX
          const bottomIndex = (selectionBottom + scrollContainer.scrollTop) / ROW_HEIGHT_PX
          const endIndex = rectangle.signedHeight < 0 ? topIndex : bottomIndex
          dragSelectionRangeRef.current = {
            initialIndex: range.initialIndex,
            start: Math.floor(Math.min(range.initialIndex, endIndex)),
            end: Math.ceil(Math.max(range.initialIndex, endIndex)),
          }
        }
        if (range == null) {
          setVisuallySelectedKeysOverride(null)
        } else {
          const keys = displayItems.slice(range.start, range.end).map(node => node.key)
          setVisuallySelectedKeysOverride(calculateNewKeys(event, keys, () => []))
        }
      }
    },
    [displayItems, calculateNewKeys]
  )

  const onSelectionDragEnd = React.useCallback(
    (event: MouseEvent) => {
      const range = dragSelectionRangeRef.current
      if (range != null) {
        const keys = displayItems.slice(range.start, range.end).map(node => node.key)
        setSelectedKeys(calculateNewKeys(event, keys, () => []))
      }
      setVisuallySelectedKeysOverride(null)
      dragSelectionRangeRef.current = null
    },
    [displayItems, calculateNewKeys, /* should never change */ setSelectedKeys]
  )

  const onSelectionDragCancel = React.useCallback(() => {
    setVisuallySelectedKeysOverride(null)
    dragSelectionRangeRef.current = null
  }, [])

  const onRowClick = React.useCallback(
    (innerRowProps: assetRow.AssetRowInnerProps, event: React.MouseEvent) => {
      const { key } = innerRowProps
      event.stopPropagation()
      const newIndex = visibleItems.findIndex(innerItem => AssetTreeNode.getKey(innerItem) === key)
      const getRange = () => {
        if (mostRecentlySelectedIndexRef.current == null) {
          return [key]
        } else {
          const index1 = mostRecentlySelectedIndexRef.current
          const index2 = newIndex
          const startIndex = Math.min(index1, index2)
          const endIndex = Math.max(index1, index2) + 1
          return visibleItems.slice(startIndex, endIndex).map(AssetTreeNode.getKey)
        }
      }
      setSelectedKeys(calculateNewKeys(event, [key], getRange))
      setMostRecentlySelectedIndex(newIndex)
      if (!event.shiftKey) {
        selectionStartIndexRef.current = null
      }
    },
    [
      visibleItems,
      calculateNewKeys,
      /* should never change */ setSelectedKeys,
      /* should never change */ setMostRecentlySelectedIndex,
    ]
  )

  const columns = columnUtils.getColumnList(backend.type, enabledColumns)

  const headerRow = (
    <tr ref={headerRowRef} className="sticky top-[1px] text-sm font-semibold">
      {columns.map(column => {
        // This is a React component, even though it does not contain JSX.
        // eslint-disable-next-line no-restricted-syntax
        const Heading = columnHeading.COLUMN_HEADING[column]
        return (
          <th key={column} className={columnUtils.COLUMN_CSS_CLASS[column]}>
            <Heading state={state} />
          </th>
        )
      })}
    </tr>
  )

  const itemRows = isLoading ? (
    <tr className="h-row">
      <td colSpan={columns.length} className="bg-transparent">
        <div className="grid w-container justify-around">
          <Spinner size={LOADING_SPINNER_SIZE_PX} state={spinnerState} />
        </div>
      </td>
    </tr>
  ) : (
    displayItems.map((item, i) => {
      const key = AssetTreeNode.getKey(item)
      const isSelected = (visuallySelectedKeysOverride ?? selectedKeys).has(key)
      const isSoleSelected = selectedKeys.size === 1 && isSelected
      return (
        <AssetRow
          key={key}
          columns={columns}
          item={item}
          state={state}
          hidden={hideRows || visibilities.get(item.key) === Visibility.hidden}
          selected={isSelected}
          setSelected={selected => {
            setSelectedKeys(set.withPresence(selectedKeysRef.current, key, selected))
          }}
          isSoleSelected={isSoleSelected}
          isKeyboardSelected={
            keyboardSelectedIndex != null && item === visibleItems[keyboardSelectedIndex]
          }
          grabKeyboardFocus={() => {
            setSelectedKeys(new Set([key]))
            setMostRecentlySelectedIndex(i, true)
          }}
          allowContextMenu={selectedKeysRef.current.size === 0 || !isSelected || isSoleSelected}
          onClick={onRowClick}
          onContextMenu={(_innerProps, event) => {
            if (!isSelected) {
              event.preventDefault()
              event.stopPropagation()
              setMostRecentlySelectedIndex(visibleItems.indexOf(item))
              selectionStartIndexRef.current = null
              setSelectedKeys(new Set([key]))
            }
          }}
          onDragStart={event => {
            let newSelectedKeys = selectedKeysRef.current
            if (!newSelectedKeys.has(key)) {
              setMostRecentlySelectedIndex(visibleItems.indexOf(item))
              selectionStartIndexRef.current = null
              newSelectedKeys = new Set([key])
              setSelectedKeys(newSelectedKeys)
            }
            const nodes = assetTree
              .preorderTraversal()
              .filter(node => newSelectedKeys.has(node.key))
            const payload: drag.AssetRowsDragPayload = nodes.map(node => ({
              key: node.key,
              asset: node.item,
            }))
            event.dataTransfer.setData(
              'application/vnd.enso.assets+json',
              JSON.stringify(nodes.map(node => node.key))
            )
            drag.setDragImageToBlank(event)
            drag.ASSET_ROWS.bind(event, payload)
            setModal(
              <DragModal
                event={event}
                className="flex flex-col rounded-default bg-selected-frame backdrop-blur-default"
                doCleanup={() => {
                  drag.ASSET_ROWS.unbind(payload)
                }}
              >
                {nodes.map(node => (
                  <NameColumn
                    key={node.key}
                    keyProp={node.key}
                    item={node.with({ depth: 0 })}
                    state={state}
                    // Default states.
                    isSoleSelected={false}
                    selected={false}
                    rowState={assetRowUtils.INITIAL_ROW_STATE}
                    // The drag placeholder cannot be interacted with.
                    setSelected={() => {}}
                    setItem={() => {}}
                    setRowState={() => {}}
                    isEditable={false}
                  />
                ))}
              </DragModal>
            )
          }}
          onDragOver={event => {
            const payload = drag.LABELS.lookup(event)
            if (payload != null) {
              event.preventDefault()
              event.stopPropagation()
              const ids = new Set(
                selectedKeysRef.current.has(key) ? selectedKeysRef.current : [key]
              )
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
                    ? AssetEventType.temporarilyAddLabels
                    : AssetEventType.temporarilyRemoveLabels,
                  ids,
                  labelNames: payload,
                })
              })
            }
          }}
          onDragEnd={() => {
            dispatchAssetEvent({
              type: AssetEventType.temporarilyAddLabels,
              ids: selectedKeysRef.current,
              labelNames: set.EMPTY,
            })
          }}
          onDrop={event => {
            const ids = new Set(selectedKeysRef.current.has(key) ? selectedKeysRef.current : [key])
            // Expand ids to include ids of descendants as well.
            for (const node of assetTree.preorderTraversal()) {
              if (ids.has(node.key) && node.children != null) {
                for (const child of node.children) {
                  ids.add(child.key)
                }
              }
            }
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
              dispatchAssetEvent({
                type: shouldAdd ? AssetEventType.addLabels : AssetEventType.removeLabels,
                ids,
                labelNames: payload,
              })
            } else {
              dispatchAssetEvent({
                type: AssetEventType.temporarilyAddLabels,
                ids,
                labelNames: set.EMPTY,
              })
            }
          }}
        />
      )
    })
  )

  const table = (
    <div
      className="flex grow flex-col"
      onContextMenu={event => {
        event.preventDefault()
        event.stopPropagation()
        setModal(
          <AssetsTableContextMenu
            category={category}
            pasteData={pasteData}
            selectedKeys={selectedKeys}
            clearSelectedKeys={clearSelectedKeys}
            nodeMapRef={nodeMapRef}
            event={event}
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
          dispatchAssetEvent({
            type: AssetEventType.temporarilyAddLabels,
            ids: selectedKeysRef.current,
            labelNames: set.EMPTY,
          })
        }
      }}
    >
      <table className="table-fixed border-collapse rounded-rows">
        <thead>{headerRow}</thead>
        <tbody ref={bodyRef}>
          {itemRows}
          <tr className="hidden h-row first:table-row">
            <td colSpan={columns.length} className="bg-transparent">
              {category === Category.trash ? (
                query.query !== '' ? (
                  <aria.Text className="px-cell-x placeholder">
                    {getText('noFilesMatchTheCurrentFilters')}
                  </aria.Text>
                ) : (
                  <aria.Text className="px-cell-x placeholder">
                    {getText('yourTrashIsEmpty')}
                  </aria.Text>
                )
              ) : query.query !== '' ? (
                <aria.Text className="px-cell-x placeholder">
                  {getText('noFilesMatchTheCurrentFilters')}
                </aria.Text>
              ) : (
                <aria.Text className="px-cell-x placeholder">{getText('youHaveNoFiles')}</aria.Text>
              )}
            </td>
          </tr>
        </tbody>
      </table>
      <div
        data-testid="root-directory-dropzone"
        className="grow"
        onClick={() => {
          setSelectedKeys(new Set())
        }}
        onDragEnter={onDragOver}
        onDragOver={onDragOver}
        onDrop={event => {
          const payload = drag.ASSET_ROWS.lookup(event)
          const filtered = payload?.filter(item => item.asset.parentId !== rootDirectoryId)
          if (filtered != null && filtered.length > 0) {
            event.preventDefault()
            event.stopPropagation()
            unsetModal()
            dispatchAssetEvent({
              type: AssetEventType.move,
              newParentKey: rootDirectoryId,
              newParentId: rootDirectoryId,
              ids: new Set(filtered.map(dragItem => dragItem.asset.id)),
            })
          } else if (event.dataTransfer.types.includes('Files')) {
            event.preventDefault()
            event.stopPropagation()
            dispatchAssetListEvent({
              type: AssetListEventType.uploadFiles,
              parentKey: rootDirectoryId,
              parentId: rootDirectoryId,
              files: Array.from(event.dataTransfer.files),
            })
          }
        }}
      />
    </div>
  )

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          {...aria.mergeProps<JSX.IntrinsicElements['div']>()(innerProps, {
            ref: rootRef,
            className: 'flex-1 overflow-auto container-size',
            onKeyDown,
            onScroll,
            onBlur: event => {
              if (
                event.relatedTarget instanceof HTMLElement &&
                !event.currentTarget.contains(event.relatedTarget)
              ) {
                setKeyboardSelectedIndex(null)
              }
            },
          })}
        >
          {!hidden && hiddenContextMenu}
          {!hidden && (
            <SelectionBrush
              onDrag={onSelectionDrag}
              onDragEnd={onSelectionDragEnd}
              onDragCancel={onSelectionDragCancel}
            />
          )}
          <div className="flex h-max min-h-full w-max min-w-full flex-col">
            {isCloud && (
              <div className="flex-0 sticky top flex h flex-col">
                <div
                  data-testid="extra-columns"
                  className="sticky right flex self-end px-extra-columns-panel-x py-extra-columns-panel-y"
                >
                  <FocusArea direction="horizontal">
                    {columnsBarProps => (
                      <div
                        {...aria.mergeProps<JSX.IntrinsicElements['div']>()(columnsBarProps, {
                          className: 'inline-flex gap-icons',
                          onFocus: () => {
                            setKeyboardSelectedIndex(null)
                          },
                        })}
                      >
                        {columnUtils.CLOUD_COLUMNS.filter(
                          column => !enabledColumns.has(column)
                        ).map(column => (
                          <Button
                            key={column}
                            active
                            image={columnUtils.COLUMN_ICONS[column]}
                            alt={getText(columnUtils.COLUMN_SHOW_TEXT_ID[column])}
                            onPress={() => {
                              const newExtraColumns = new Set(enabledColumns)
                              if (enabledColumns.has(column)) {
                                newExtraColumns.delete(column)
                              } else {
                                newExtraColumns.add(column)
                              }
                              setEnabledColumns(newExtraColumns)
                            }}
                          />
                        ))}
                      </div>
                    )}
                  </FocusArea>
                </div>
              </div>
            )}
            <div className="flex h-full w-min min-w-full grow flex-col">{table}</div>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
