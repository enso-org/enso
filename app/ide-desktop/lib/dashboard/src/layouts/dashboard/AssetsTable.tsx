/** @file Table displaying a list of projects. */
import * as React from 'react'

import * as toast from 'react-toastify'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'
import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as eventHooks from '#/hooks/eventHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import type * as assetSearchBar from '#/layouts/dashboard/AssetSearchBar'
import type * as assetSettingsPanel from '#/layouts/dashboard/AssetSettingsPanel'
import AssetsTableContextMenu from '#/layouts/dashboard/AssetsTableContextMenu'
import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import DuplicateAssetsModal from '#/layouts/dashboard/DuplicateAssetsModal'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as shortcutsProvider from '#/providers/ShortcutsProvider'
import * as backendModule from '#/services/backend'
import * as array from '#/utilities/array'
import * as assetQuery from '#/utilities/assetQuery'
import * as assetTreeNode from '#/utilities/assetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as drag from '#/utilities/drag'
import * as fileInfo from '#/utilities/fileInfo'
import * as localStorageModule from '#/utilities/localStorage'
import type * as pasteDataModule from '#/utilities/pasteData'
import PasteType from '#/utilities/PasteType'
import * as permissions from '#/utilities/permissions'
import * as set from '#/utilities/set'
import * as shortcutsModule from '#/utilities/shortcuts'
import * as sorting from '#/utilities/sorting'
import * as string from '#/utilities/string'
import * as uniqueString from '#/utilities/uniqueString'
import Visibility from '#/utilities/visibility'

import Button from '#/components/Button'
import type * as assetRow from '#/components/dashboard/AssetRow'
import AssetRow from '#/components/dashboard/AssetRow'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import NameColumn from '#/components/dashboard/column/NameColumn'
import * as columnHeading from '#/components/dashboard/columnHeading'
import Label from '#/components/dashboard/Label'
import DragModal from '#/components/DragModal'
import Spinner, * as spinner from '#/components/Spinner'

// =================
// === Constants ===
// =================

/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE = 36
/** The number of pixels the header bar should shrink when the extra column selector is visible. */
const TABLE_HEADER_WIDTH_SHRINKAGE_PX = 116
/** A value that represents that the first argument is less than the second argument, in a
 * sorting function. */
const COMPARE_LESS_THAN = -1
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
        assetTreeNode.AssetTreeNode.fromAsset(asset, directoryKey, directoryId, depth, getKey)
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

const CATEGORY_TO_FILTER_BY: Record<Category, backendModule.FilterBy | null> = {
  [Category.recent]: null,
  [Category.drafts]: null,
  [Category.home]: backendModule.FilterBy.active,
  [Category.root]: null,
  [Category.trash]: backendModule.FilterBy.trashed,
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  numberOfSelectedItems: number
  visibilities: ReadonlyMap<backendModule.AssetId, Visibility>
  category: Category
  labels: Map<backendModule.LabelName, backendModule.Label>
  deletedLabelNames: Set<backendModule.LabelName>
  hasPasteData: boolean
  setPasteData: (pasteData: pasteDataModule.PasteData<Set<backendModule.AssetId>>) => void
  sortColumn: columnUtils.SortableColumn | null
  setSortColumn: (column: columnUtils.SortableColumn | null) => void
  sortDirection: sorting.SortDirection | null
  setSortDirection: (sortDirection: sorting.SortDirection | null) => void
  query: assetQuery.AssetQuery
  setQuery: React.Dispatch<React.SetStateAction<assetQuery.AssetQuery>>
  dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  assetEvents: assetEvent.AssetEvent[]
  dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
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
  setVisibility: (visibility: Visibility) => void
  isEditingName: boolean
  temporarilyAddedLabels: ReadonlySet<backendModule.LabelName>
  temporarilyRemovedLabels: ReadonlySet<backendModule.LabelName>
}

/** The default {@link AssetRowState} associated with a {@link AssetRow}. */
const INITIAL_ROW_STATE = Object.freeze<AssetRowState>({
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
  setCanDownloadFiles: (canDownloadFiles: boolean) => void
  category: Category
  allLabels: Map<backendModule.LabelName, backendModule.Label>
  setSuggestions: (suggestions: assetSearchBar.Suggestion[]) => void
  initialProjectName: string | null
  projectStartupInfo: backendModule.ProjectStartupInfo | null
  deletedLabelNames: Set<backendModule.LabelName>
  /** These events will be dispatched the next time the assets list is refreshed, rather than
   * immediately. */
  queuedAssetEvents: assetEvent.AssetEvent[]
  assetListEvents: assetListEvent.AssetListEvent[]
  dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  assetEvents: assetEvent.AssetEvent[]
  dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
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
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
  const { query, setQuery, setCanDownloadFiles, category, allLabels, setSuggestions } = props
  const { deletedLabelNames, initialProjectName, projectStartupInfo } = props
  const { queuedAssetEvents: rawQueuedAssetEvents } = props
  const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
  const { setAssetSettingsPanelProps, doOpenIde, doCloseIde: rawDoCloseIde, doCreateLabel } = props

  const { organization, user, accessToken } = authProvider.useNonPartialUserSession()
  const { backend } = backendProvider.useBackend()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { shortcuts } = shortcutsProvider.useShortcuts()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [initialized, setInitialized] = React.useState(false)
  const [isLoading, setIsLoading] = React.useState(true)
  const [extraColumns, setExtraColumns] = React.useState(() => new Set<columnUtils.ExtraColumn>())
  const [sortColumn, setSortColumn] = React.useState<columnUtils.SortableColumn | null>(null)
  const [sortDirection, setSortDirection] = React.useState<sorting.SortDirection | null>(null)
  const [selectedKeys, setSelectedKeys] = React.useState(() => new Set<backendModule.AssetId>())
  const [pasteData, setPasteData] = React.useState<pasteDataModule.PasteData<
    Set<backendModule.AssetId>
  > | null>(null)
  const [, setQueuedAssetEvents] = React.useState<assetEvent.AssetEvent[]>([])
  const [, setNameOfProjectToImmediatelyOpen] = React.useState(initialProjectName)
  const rootDirectoryId = React.useMemo(
    () => organization?.rootDirectoryId ?? backendModule.DirectoryId(''),
    [organization]
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
  const placeholder =
    category === Category.trash
      ? TRASH_PLACEHOLDER
      : query.query !== ''
      ? QUERY_PLACEHOLDER
      : PLACEHOLDER
  const scrollContainerRef = React.useRef<HTMLDivElement>(null)
  const headerRowRef = React.useRef<HTMLTableRowElement>(null)
  const assetTreeRef = React.useRef<assetTreeNode.AssetTreeNode>(assetTree)
  const pasteDataRef = React.useRef<pasteDataModule.PasteData<Set<backendModule.AssetId>> | null>(
    null
  )
  const nodeMapRef = React.useRef<ReadonlyMap<backendModule.AssetId, assetTreeNode.AssetTreeNode>>(
    new Map<backendModule.AssetId, assetTreeNode.AssetTreeNode>()
  )
  const filter = React.useMemo(() => {
    const globCache: Record<string, RegExp> = {}
    if (/^\s*$/.test(query.query)) {
      return null
    } else {
      return (node: assetTreeNode.AssetTreeNode) => {
        const assetType =
          node.item.type === backendModule.AssetType.directory
            ? 'folder'
            : node.item.type === backendModule.AssetType.secret
            ? 'connector'
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
            .map(owner => owner.user.user_name) ?? []
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
        case columnUtils.Column.name: {
          compare = (a, b) =>
            multiplier *
            (a.item.title > b.item.title ? 1 : a.item.title < b.item.title ? COMPARE_LESS_THAN : 0)

          break
        }
        case columnUtils.Column.modified: {
          compare = (a, b) =>
            multiplier * (Number(new Date(a.item.modifiedAt)) - Number(new Date(b.item.modifiedAt)))
          break
        }
      }
      return assetTree.preorderTraversal(tree => Array.from(tree).sort(compare))
    }
  }, [assetTree, sortColumn, sortDirection])
  const visibilities = React.useMemo(() => {
    const map = new Map<backendModule.AssetId, Visibility>()
    const processNode = (node: assetTreeNode.AssetTreeNode) => {
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
            return node?.item.type === backendModule.AssetType.file
          })
      )
    }
  }, [category, selectedKeys, isCloud, /* should never change */ setCanDownloadFiles])

  React.useEffect(() => {
    const nodeToSuggestion = (
      node: assetTreeNode.AssetTreeNode,
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
    const terms = assetQuery.AssetQuery.terms(query.query)
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
            .filter(node => node.item.type === backendModule.AssetType.file)
            .map(node => fileInfo.fileExtension(node.item.title))
          setSuggestions(
            Array.from(
              new Set(extensions),
              (extension): assetSearchBar.Suggestion => ({
                render: () =>
                  assetQuery.AssetQuery.termToString({
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
                  assetQuery.AssetQuery.termToString({
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
                .map(permission => permission.user.user_name)
            )
          setSuggestions(
            Array.from(
              new Set(owners),
              (owner): assetSearchBar.Suggestion => ({
                render: () =>
                  assetQuery.AssetQuery.termToString({
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
    return shortcuts.registerKeyboardHandlers({
      [shortcutsModule.KeyboardAction.cancelCut]: () => {
        if (pasteDataRef.current == null) {
          return false
        } else {
          dispatchAssetEvent({
            type: AssetEventType.cancelCut,
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
            assetTreeNode.AssetTreeNode.fromAsset(asset, rootDirectory.id, rootDirectory.id, 0)
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

  asyncEffectHooks.useAsyncEffect(
    null,
    async signal => {
      switch (backend.type) {
        case backendModule.BackendType.local: {
          const newAssets = await backend.listDirectory(
            {
              parentId: null,
              filterBy: CATEGORY_TO_FILTER_BY[category],
              recentProjects: category === Category.recent,
              labels: null,
            },
            null
          )
          if (!signal.aborted) {
            setIsLoading(false)
            overwriteNodes(newAssets)
          }
          break
        }
        case backendModule.BackendType.remote: {
          const queuedDirectoryListings = new Map<backendModule.AssetId, backendModule.AnyAsset[]>()
          const withChildren = (node: assetTreeNode.AssetTreeNode): assetTreeNode.AssetTreeNode => {
            const queuedListing = queuedDirectoryListings.get(node.item.id)
            if (queuedListing == null || !backendModule.assetIsDirectory(node.item)) {
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
            if (backendModule.assetIsDirectory(entry.item) && entry.children != null) {
              const id = entry.item.id
              void backend
                .listDirectory(
                  {
                    parentId: id,
                    filterBy: CATEGORY_TO_FILTER_BY[category],
                    recentProjects: category === Category.recent,
                    labels: null,
                  },
                  entry.item.title
                )
                .then(
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
            const newAssets = await backend.listDirectory(
              {
                parentId: null,
                filterBy: CATEGORY_TO_FILTER_BY[category],
                recentProjects: category === Category.recent,
                labels: null,
              },
              null
            )
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
  }, [backend.type])

  React.useEffect(() => {
    if (initialized) {
      localStorage.set(localStorageModule.LocalStorageKey.extraColumns, Array.from(extraColumns))
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
          oldAssetTree.map(item => (item.key !== key ? item : item.with({ children: null })))
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
              recentProjects: category === Category.recent,
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
                    assetTreeNode.AssetTreeNode.fromAsset(child, key, directoryId, item.depth + 1)
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
                      : initialChildren == null || initialChildren.length === 0
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

  eventHooks.useEventHandler(assetListEvents, event => {
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
          permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
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
        const projectName = getNewProjectName(event.templateName, event.parentId)
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
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId)
        dispatchAssetEvent({
          type: AssetEventType.newProject,
          placeholderId: dummyId,
          templateId: event.templateId,
          onSpinnerStateChange: event.onSpinnerStateChange,
        })
        break
      }
      case AssetListEventType.uploadFiles: {
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
        const ownerPermission = permissions.tryGetSingletonOwnerPermission(organization, user)
        if (duplicateFiles.length === 0 && duplicateProjects.length === 0) {
          const placeholderFiles = files.map(file =>
            backendModule.createPlaceholderFileAsset(file.name, event.parentId, ownerPermission)
          )
          const placeholderProjects = projects.map(project =>
            backendModule.createPlaceholderProjectAsset(
              project.name,
              event.parentId,
              ownerPermission,
              organization
            )
          )
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
                organization
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
              nonConflictingCount={
                files.length +
                projects.length -
                conflictingFiles.length -
                conflictingProjects.length
              }
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
                    const asset = backendModule.createPlaceholderProjectAsset(
                      backendModule.stripProjectExtension(project.name),
                      event.parentId,
                      ownerPermission,
                      organization
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
      case AssetListEventType.newSecret: {
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
      case AssetListEventType.removeSelf: {
        dispatchAssetEvent({
          type: AssetEventType.removeSelf,
          id: event.id,
        })
        break
      }
      case AssetListEventType.closeFolder: {
        doToggleDirectoryExpansion(event.id, event.key, null, false)
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

  const doCloseIde = React.useCallback(
    (project: backendModule.ProjectAsset) => {
      if (project.id === projectStartupInfo?.projectAsset.id) {
        dispatchAssetEvent({
          type: AssetEventType.cancelOpeningAllProjects,
        })
        rawDoCloseIde(project)
      }
    },
    [projectStartupInfo, rawDoCloseIde, /* should never change */ dispatchAssetEvent]
  )

  const doCopy = React.useCallback(() => {
    unsetModal()
    setSelectedKeys(oldSelectedKeys => {
      queueMicrotask(() => {
        setPasteData({ type: PasteType.copy, data: oldSelectedKeys })
      })
      return oldSelectedKeys
    })
  }, [/* should never change */ unsetModal])

  const doCut = React.useCallback(() => {
    unsetModal()
    setSelectedKeys(oldSelectedKeys => {
      queueMicrotask(() => {
        if (pasteData != null) {
          dispatchAssetEvent({
            type: AssetEventType.cancelCut,
            ids: pasteData.data,
          })
        }
        setPasteData({ type: PasteType.move, data: oldSelectedKeys })
        dispatchAssetEvent({
          type: AssetEventType.cut,
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

  const hiddenContextMenu = React.useMemo(
    () => (
      <AssetsTableContextMenu
        hidden
        category={category}
        pasteData={pasteData}
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
      category,
      pasteData,
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
    const filtered = payload?.filter(item => item.asset.parentId !== rootDirectoryId)
    if (filtered != null && filtered.length > 0) {
      event.preventDefault()
    }
  }

  const state = React.useMemo(
    // The type MUST be here to trigger excess property errors at typecheck time.
    (): AssetsTableState => ({
      visibilities,
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
      visibilities,
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
        !shortcuts.matchesMouseAction(shortcutsModule.MouseAction.selectAdditional, event) &&
        !shortcuts.matchesMouseAction(shortcutsModule.MouseAction.selectAdditionalRange, event) &&
        selectedKeys.size !== 0
      ) {
        setSelectedKeys(new Set())
      }
    }
    document.addEventListener('click', onDocumentClick)
    return () => {
      document.removeEventListener('click', onDocumentClick)
    }
  }, [selectedKeys, /* should never change */ setSelectedKeys, shortcuts])

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
            innerItem => assetTreeNode.AssetTreeNode.getKey(innerItem) === previouslySelectedKey
          )
          const index2 = displayItems.findIndex(
            innerItem => assetTreeNode.AssetTreeNode.getKey(innerItem) === key
          )
          const selectedItems =
            index1 <= index2
              ? displayItems.slice(index1, index2 + 1)
              : displayItems.slice(index2, index1 + 1)
          return selectedItems.map(assetTreeNode.AssetTreeNode.getKey)
        }
      }
      if (shortcuts.matchesMouseAction(shortcutsModule.MouseAction.selectRange, event)) {
        setSelectedKeys(new Set(getNewlySelectedKeys()))
      } else if (
        shortcuts.matchesMouseAction(shortcutsModule.MouseAction.selectAdditionalRange, event)
      ) {
        setSelectedKeys(
          oldSelectedItems => new Set([...oldSelectedItems, ...getNewlySelectedKeys()])
        )
      } else if (
        shortcuts.matchesMouseAction(shortcutsModule.MouseAction.selectAdditional, event)
      ) {
        setSelectedKeys(oldSelectedItems => {
          const newItems = new Set(oldSelectedItems)
          if (oldSelectedItems.has(key)) {
            newItems.delete(key)
          } else {
            newItems.add(key)
          }
          return newItems
        })
      } else {
        setSelectedKeys(new Set([key]))
      }
      setPreviouslySelectedKey(key)
    },
    [displayItems, previouslySelectedKey, shortcuts, /* should never change */ setSelectedKeys]
  )

  const columns = columnUtils.getColumnList(backend.type, extraColumns).map(column => ({
    id: column,
    className: columnUtils.COLUMN_CSS_CLASS[column],
    heading: columnHeading.COLUMN_HEADING[column],
    render: columnModule.COLUMN_RENDERER[column],
  }))

  const headerRow = (
    <tr ref={headerRowRef} className="sticky top-0">
      {columns.map(column => {
        // This is a React component, even though it does not contain JSX.
        // eslint-disable-next-line no-restricted-syntax
        const Heading = column.heading
        return (
          <th key={column.id} className={`text-sm font-semibold ${column.className}`}>
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
      const key = assetTreeNode.AssetTreeNode.getKey(item)
      const isSelected = selectedKeys.has(key)
      const isSoleSelectedItem = selectedKeys.size === 1 && isSelected
      return (
        <AssetRow
          columns={columns}
          // The following two lines are safe; the type error occurs because a property
          // with a conditional type is being destructured.
          // eslint-disable-next-line no-restricted-syntax
          state={state as never}
          // eslint-disable-next-line no-restricted-syntax
          initialRowState={INITIAL_ROW_STATE as never}
          key={key}
          keyProp={key}
          item={item}
          hidden={visibilities.get(item.key) === Visibility.hidden}
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
          draggable={true}
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
                      <NameColumn
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
          onDragOver={event => {
            setSelectedKeys(oldSelectedKeys => {
              const payload = drag.LABELS.lookup(event)
              if (payload != null) {
                event.preventDefault()
                event.stopPropagation()
                const ids = oldSelectedKeys.has(key) ? oldSelectedKeys : new Set([key])
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
                    type: shouldAdd ? AssetEventType.addLabels : AssetEventType.removeLabels,
                    ids,
                    labelNames: payload,
                  })
                })
              } else {
                window.setTimeout(() => {
                  dispatchAssetEvent({
                    type: AssetEventType.temporarilyAddLabels,
                    ids,
                    labelNames: set.EMPTY,
                  })
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
            category={category}
            pasteData={pasteData}
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
          }
        }}
      />
    </div>
  )

  return (
    <div ref={scrollContainerRef} className="flex-1 overflow-auto">
      <div className="flex flex-col w-min min-w-full h-full">
        {backend.type !== backendModule.BackendType.local && (
          <div className="sticky top-0 h-0">
            <div className="block sticky right-0 ml-auto w-29 px-2 pt-2.25 pb-1.75 z-1">
              <div className="inline-flex gap-3">
                {columnUtils.EXTRA_COLUMNS.map(column => (
                  <Button
                    key={column}
                    active={extraColumns.has(column)}
                    image={columnUtils.EXTRA_COLUMN_IMAGES[column]}
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
