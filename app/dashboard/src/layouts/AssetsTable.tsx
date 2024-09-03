/** @file Table displaying a list of projects. */
import * as React from 'react'

import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import * as toast from 'react-toastify'
import invariant from 'tiny-invariant'
import * as z from 'zod'

import DropFilesImage from '#/assets/drop_files.svg'

import * as mimeTypes from '#/data/mimeTypes'

import * as autoScrollHooks from '#/hooks/autoScrollHooks'
import {
  backendMutationOptions,
  useBackendQuery,
  useListTags,
  useListUserGroups,
  useListUsers,
} from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as intersectionHooks from '#/hooks/intersectionHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import useOnScroll from '#/hooks/useOnScroll'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import {
  useDriveStore,
  useSetCanDownload,
  useSetSelectedKeys,
  useSetTargetDirectory,
  useSetVisuallySelectedKeys,
} from '#/providers/DriveProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'
import * as projectsProvider from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import type { Category } from '#/layouts/CategorySwitcher/Category'

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
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

import DragModal from '#/modals/DragModal'
import DuplicateAssetsModal from '#/modals/DuplicateAssetsModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import { isSpecialReadonlyDirectoryId } from '#/services/RemoteBackend'

import { ErrorDisplay } from '#/components/ErrorBoundary'
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
import * as tailwindMerge from '#/utilities/tailwindMerge'
import * as uniqueString from '#/utilities/uniqueString'
import Visibility from '#/utilities/Visibility'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly enabledColumns: readonly columnUtils.Column[]
  }
}

LocalStorage.registerKey('enabledColumns', {
  schema: z.enum(columnUtils.CLOUD_ENTERPRISE_COLUMNS).array().readonly(),
})

// =================
// === Constants ===
// =================

/** If the ratio of intersection between the main dropzone that should be visible, and the
 * scrollable container, is below this value, then the backup dropzone will be shown. */
const MINIMUM_DROPZONE_INTERSECTION_RATIO = 0.5
/** The height of each row in the table body. MUST be identical to the value as set by the
 * Tailwind styling. */
const ROW_HEIGHT_PX = 38
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
    addToQuery: (query) => query.addToLastTerm({ nos: ['label'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ nos: ['label'] }),
  },
  {
    render: () => 'no:description',
    addToQuery: (query) => query.addToLastTerm({ nos: ['description'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ nos: ['description'] }),
  },
]
const SUGGESTIONS_FOR_HAS: assetSearchBar.Suggestion[] = [
  {
    render: () => 'has:label',
    addToQuery: (query) => query.addToLastTerm({ negativeNos: ['label'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeNos: ['label'] }),
  },
  {
    render: () => 'has:description',
    addToQuery: (query) => query.addToLastTerm({ negativeNos: ['description'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeNos: ['description'] }),
  },
]
const SUGGESTIONS_FOR_TYPE: assetSearchBar.Suggestion[] = [
  {
    render: () => 'type:project',
    addToQuery: (query) => query.addToLastTerm({ types: ['project'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['project'] }),
  },
  {
    render: () => 'type:folder',
    addToQuery: (query) => query.addToLastTerm({ types: ['folder'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['folder'] }),
  },
  {
    render: () => 'type:file',
    addToQuery: (query) => query.addToLastTerm({ types: ['file'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['file'] }),
  },
  {
    render: () => 'type:secret',
    addToQuery: (query) => query.addToLastTerm({ types: ['secret'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['secret'] }),
  },
  {
    render: () => 'type:datalink',
    addToQuery: (query) => query.addToLastTerm({ types: ['datalink'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['datalink'] }),
  },
]
const SUGGESTIONS_FOR_NEGATIVE_TYPE: assetSearchBar.Suggestion[] = [
  {
    render: () => 'type:project',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['project'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['project'] }),
  },
  {
    render: () => 'type:folder',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['folder'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['folder'] }),
  },
  {
    render: () => 'type:file',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['file'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['file'] }),
  },
  {
    render: () => 'type:datalink',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['datalink'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['datalink'] }),
  },
]

// ===================================
// === insertAssetTreeNodeChildren ===
// ===================================

/** Return a directory, with new children added into its list of children.
 * All children MUST have the same asset type. */
function insertAssetTreeNodeChildren(
  item: assetTreeNode.AnyAssetTreeNode,
  children: readonly backendModule.AnyAsset[],
  directoryKey: backendModule.DirectoryId,
  directoryId: backendModule.DirectoryId,
  getInitialAssetEvents: (id: backendModule.AssetId) => readonly assetEvent.AssetEvent[] | null,
): assetTreeNode.AnyAssetTreeNode {
  const depth = item.depth + 1
  const typeOrder = children[0] != null ? backendModule.ASSET_TYPE_ORDER[children[0].type] : 0
  const nodes = (item.children ?? []).filter(
    (node) => node.item.type !== backendModule.AssetType.specialEmpty,
  )
  const nodesToInsert = children.map((asset) =>
    AssetTreeNode.fromAsset(
      asset,
      directoryKey,
      directoryId,
      depth,
      `${item.path}/${asset.title}`,
      getInitialAssetEvents(asset.id),
    ),
  )
  const newNodes = array.splicedBefore(
    nodes,
    nodesToInsert,
    (innerItem) => backendModule.ASSET_TYPE_ORDER[innerItem.item.type] >= typeOrder,
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
  getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null,
  getInitialAssetEvents: (
    id: backendModule.AssetId,
  ) => readonly assetEvent.AssetEvent[] | null = () => null,
): assetTreeNode.AnyAssetTreeNode {
  const depth = item.depth + 1
  const nodes = (item.children ?? []).filter(
    (node) => node.item.type !== backendModule.AssetType.specialEmpty,
  )
  const byType: Readonly<Record<backendModule.AssetType, backendModule.AnyAsset[]>> = {
    [backendModule.AssetType.directory]: [],
    [backendModule.AssetType.project]: [],
    [backendModule.AssetType.file]: [],
    [backendModule.AssetType.datalink]: [],
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
      const nodesToInsert = childrenOfSpecificType.map((asset) =>
        AssetTreeNode.fromAsset(
          asset,
          directoryKey,
          directoryId,
          depth,
          `${item.path}/${asset.title}`,
          getInitialAssetEvents(asset.id),
          getKey?.(asset) ?? asset.id,
        ),
      )
      newNodes = array.splicedBefore(
        newNodes,
        nodesToInsert,
        (innerItem) => backendModule.ASSET_TYPE_ORDER[innerItem.item.type] >= typeOrder,
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

const CATEGORY_TO_FILTER_BY: Readonly<Record<Category['type'], backendModule.FilterBy | null>> = {
  cloud: backendModule.FilterBy.active,
  local: backendModule.FilterBy.active,
  recent: null,
  trash: backendModule.FilterBy.trashed,
  user: backendModule.FilterBy.active,
  team: backendModule.FilterBy.active,
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'local-directory': backendModule.FilterBy.active,
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  readonly backend: Backend
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly scrollContainerRef: React.RefObject<HTMLElement>
  readonly visibilities: ReadonlyMap<backendModule.AssetId, Visibility>
  readonly category: Category
  readonly hasPasteData: boolean
  readonly setPasteData: (pasteData: pasteDataModule.PasteData<Set<backendModule.AssetId>>) => void
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly setSortInfo: (sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly nodeMap: Readonly<
    React.MutableRefObject<ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>>
  >
  readonly pasteData: Readonly<
    React.MutableRefObject<pasteDataModule.PasteData<ReadonlySet<backendModule.AssetId>> | null>
  >
  readonly hideColumn: (column: columnUtils.Column) => void
  readonly doToggleDirectoryExpansion: (
    directoryId: backendModule.DirectoryId,
    key: backendModule.DirectoryId,
    title?: string | null,
    override?: boolean,
  ) => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
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
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setSuggestions: React.Dispatch<
    React.SetStateAction<readonly assetSearchBar.Suggestion[]>
  >
  readonly category: Category
  readonly initialProjectName: string | null
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly targetDirectoryNodeRef: React.MutableRefObject<assetTreeNode.AnyAssetTreeNode<backendModule.DirectoryAsset> | null>
  readonly assetManagementApiRef: React.Ref<AssetManagementApi>
}

/**
 * The API for managing assets in the table.
 */
export interface AssetManagementApi {
  readonly getAsset: (id: backendModule.AssetId) => backendModule.AnyAsset | null
  readonly setAsset: (id: backendModule.AssetId, asset: backendModule.AnyAsset) => void
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
  const { hidden, query, setQuery, category, assetManagementApiRef } = props
  const { setSuggestions, initialProjectName } = props
  const { setAssetPanelProps, targetDirectoryNodeRef, setIsAssetPanelTemporarilyVisible } = props

  const openedProjects = projectsProvider.useLaunchedProjects()
  const doOpenProject = projectHooks.useOpenProject()
  const setCanDownload = useSetCanDownload()

  const { user } = authProvider.useFullUserSession()
  const backend = backendProvider.useBackend(category)
  const labels = useListTags(backend)
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const previousCategoryRef = React.useRef(category)
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const setTargetDirectoryRaw = useSetTargetDirectory()
  const didLoadingProjectManagerFail = backendProvider.useDidLoadingProjectManagerFail()
  const reconnectToProjectManager = backendProvider.useReconnectToProjectManager()
  const [enabledColumns, setEnabledColumns] = React.useState(columnUtils.DEFAULT_ENABLED_COLUMNS)
  const [sortInfo, setSortInfo] =
    React.useState<sorting.SortInfo<columnUtils.SortableColumn> | null>(null)
  const driveStore = useDriveStore()
  const setSelectedKeys = useSetSelectedKeys()
  const setVisuallySelectedKeys = useSetVisuallySelectedKeys()
  const updateAssetRef = React.useRef<
    Record<backendModule.AnyAsset['id'], (asset: backendModule.AnyAsset) => void>
  >({})
  const [pasteData, setPasteData] = React.useState<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const [, setQueuedAssetEvents] = React.useState<assetEvent.AssetEvent[]>([])
  const nameOfProjectToImmediatelyOpenRef = React.useRef(initialProjectName)
  const users = useListUsers(backend)
  const userGroups = useListUserGroups(backend)
  const organizationQuery = useSuspenseQuery({
    queryKey: [backend.type, 'getOrganization'],
    queryFn: () => backend.getOrganization(),
  })
  const organization = organizationQuery.data
  const [localRootDirectory] = localStorageProvider.useLocalStorageState('localRootDirectory')
  const rootDirectoryId = React.useMemo(() => {
    const localRootPath = localRootDirectory != null ? backendModule.Path(localRootDirectory) : null
    const id =
      'homeDirectoryId' in category ?
        category.homeDirectoryId
      : backend.rootDirectoryId(user, organization, localRootPath)
    invariant(id, 'Missing root directory')
    return id
  }, [category, backend, user, organization, localRootDirectory])
  const [assetTree, setAssetTree] = React.useState<assetTreeNode.AnyAssetTreeNode>(() => {
    const rootParentDirectoryId = backendModule.DirectoryId('')
    const rootPath = 'rootPath' in category ? category.rootPath : backend.rootPath
    return AssetTreeNode.fromAsset(
      backendModule.createRootDirectoryAsset(rootDirectoryId),
      rootParentDirectoryId,
      rootParentDirectoryId,
      -1,
      rootPath,
      null,
    )
  })
  const [isDraggingFiles, setIsDraggingFiles] = React.useState(false)
  const [droppedFilesCount, setDroppedFilesCount] = React.useState(0)
  const isCloud = backend.type === backendModule.BackendType.remote
  /** Events sent when the asset list was still loading. */
  const queuedAssetListEventsRef = React.useRef<assetListEvent.AssetListEvent[]>([])
  const rootRef = React.useRef<HTMLDivElement | null>(null)
  const cleanupRootRef = React.useRef(() => {})
  const mainDropzoneRef = React.useRef<HTMLButtonElement | null>(null)
  const lastSelectedIdsRef = React.useRef<
    backendModule.AssetId | ReadonlySet<backendModule.AssetId> | null
  >(null)
  const headerRowRef = React.useRef<HTMLTableRowElement>(null)
  const assetTreeRef = React.useRef<assetTreeNode.AnyAssetTreeNode>(assetTree)
  const pasteDataRef = React.useRef<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const nodeMapRef = React.useRef<
    ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>
  >(new Map<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>())
  const isAssetContextMenuVisible =
    category.type !== 'cloud' || user.plan == null || user.plan === backendModule.Plan.solo
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
          node.item.type === backendModule.AssetType.directory ? 'folder'
          : node.item.type === backendModule.AssetType.datalink ? 'datalink'
          : String(node.item.type)
        const assetExtension =
          node.item.type !== backendModule.AssetType.file ?
            null
          : fileInfo.fileExtension(node.item.title).toLowerCase()
        const assetModifiedAt = new Date(node.item.modifiedAt)
        const nodeLabels: readonly string[] = node.item.labels ?? []
        const lowercaseName = node.item.title.toLowerCase()
        const lowercaseDescription = node.item.description?.toLowerCase() ?? ''
        const owners =
          node.item.permissions
            ?.filter((permission) => permission.permission === permissions.PermissionAction.own)
            .map(backendModule.getAssetPermissionName) ?? []
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
              return nodeLabels.length === 0
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
          predicate: (value: string) => boolean,
        ) =>
          positive.every((values) => isEmpty(values) || values.some(predicate)) &&
          negative.every((values) => !values.some(predicate))
        return (
          filterTag(query.nos, query.negativeNos, (no) => isAbsent(no.toLowerCase())) &&
          filterTag(query.keywords, query.negativeKeywords, (keyword) =>
            lowercaseName.includes(keyword.toLowerCase()),
          ) &&
          filterTag(query.names, query.negativeNames, (name) => globMatch(name, lowercaseName)) &&
          filterTag(query.labels, query.negativeLabels, (label) =>
            nodeLabels.some((assetLabel) => globMatch(label, assetLabel)),
          ) &&
          filterTag(query.types, query.negativeTypes, (type) => type === assetType) &&
          filterTag(
            query.extensions,
            query.negativeExtensions,
            (extension) => extension.toLowerCase() === assetExtension,
          ) &&
          filterTag(query.descriptions, query.negativeDescriptions, (description) =>
            lowercaseDescription.includes(description.toLowerCase()),
          ) &&
          filterTag(query.modifieds, query.negativeModifieds, matchesDate) &&
          filterTag(query.owners, query.negativeOwners, (owner) =>
            owners.some((assetOwner) => globMatch(owner, assetOwner)),
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
              const delta =
                a.item.title > b.item.title ? 1
                : a.item.title < b.item.title ? -1
                : 0
              return multiplier * delta
            } else {
              const delta =
                aTitle > bTitle ? 1
                : aTitle < bTitle ? -1
                : 0
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
      return assetTree.preorderTraversal((tree) => [...tree].sort(compare))
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
    () => displayItems.filter((item) => visibilities.get(item.key) !== Visibility.hidden),
    [displayItems, visibilities],
  )

  const isMainDropzoneVisible = intersectionHooks.useIntersectionRatio(
    rootRef,
    mainDropzoneRef,
    MINIMUM_DROPZONE_INTERSECTION_RATIO,
    (ratio) => ratio >= MINIMUM_DROPZONE_INTERSECTION_RATIO,
    true,
  )

  const updateSecret = useMutation(backendMutationOptions(backend, 'updateSecret')).mutateAsync
  React.useEffect(() => {
    previousCategoryRef.current = category
  })

  const setTargetDirectory = useEventCallback(
    (page: AssetTreeNode<backendModule.DirectoryAsset> | null) => {
      setTargetDirectoryRaw(page)
      targetDirectoryNodeRef.current = page
    },
  )

  React.useEffect(
    () =>
      driveStore.subscribe(({ selectedKeys }, { selectedKeys: oldSelectedKeys }) => {
        if (selectedKeys !== oldSelectedKeys) {
          if (selectedKeys.size === 0) {
            setTargetDirectory(null)
          } else if (selectedKeys.size === 1) {
            const [soleKey] = selectedKeys
            const node = soleKey == null ? null : nodeMapRef.current.get(soleKey)
            if (node != null && node.isType(backendModule.AssetType.directory)) {
              setTargetDirectory(node)
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
                } else if (
                  node.key === commonDirectoryKey ||
                  node.directoryKey === commonDirectoryKey
                ) {
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
            const node =
              commonDirectoryKey == null ? null : nodeMapRef.current.get(commonDirectoryKey)
            if (node != null && node.isType(backendModule.AssetType.directory)) {
              setTargetDirectory(node)
            }
          }
        }
      }),
    [driveStore, setTargetDirectory],
  )

  React.useEffect(() => {
    const nodeToSuggestion = (
      node: assetTreeNode.AnyAssetTreeNode,
      key: assetQuery.AssetQueryKey = 'names',
    ): assetSearchBar.Suggestion => ({
      render: () => `${key === 'names' ? '' : '-:'}${node.item.title}`,
      addToQuery: (oldQuery) => oldQuery.addToLastTerm({ [key]: [node.item.title] }),
      deleteFromQuery: (oldQuery) => oldQuery.deleteFromLastTerm({ [key]: [node.item.title] }),
    })
    const allVisibleNodes = () =>
      assetTree
        .preorderTraversal((children) =>
          children.filter((child) => visibilities.get(child.key) !== Visibility.hidden),
        )
        .filter(
          (node) =>
            visibilities.get(node.key) === Visibility.visible &&
            node.item.type !== backendModule.AssetType.specialEmpty &&
            node.item.type !== backendModule.AssetType.specialLoading,
        )
    const allVisible = (negative = false) =>
      allVisibleNodes().map((node) => nodeToSuggestion(node, negative ? 'negativeNames' : 'names'))
    const terms = AssetQuery.terms(query.query)
    const term = terms.find((otherTerm) => otherTerm.values.length === 0) ?? terms[terms.length - 1]
    const termValues = term?.values ?? []
    const shouldOmitNames = terms.some((otherTerm) => otherTerm.tag === 'name')
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
            .filter((node) => node.item.type === backendModule.AssetType.file)
            .map((node) => fileInfo.fileExtension(node.item.title))
          setSuggestions(
            Array.from(
              new Set(extensions),
              (extension): assetSearchBar.Suggestion => ({
                render: () =>
                  AssetQuery.termToString({
                    tag: `${negative ? '-' : ''}extension`,
                    values: [extension],
                  }),
                addToQuery: (oldQuery) =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeExtensions: [extension] } : { extensions: [extension] },
                  ),
                deleteFromQuery: (oldQuery) =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeExtensions: [extension] } : { extensions: [extension] },
                  ),
              }),
            ),
          )
          break
        }
        case 'modified':
        case '-modified': {
          const modifieds = assetTree.preorderTraversal().map((node) => {
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
                addToQuery: (oldQuery) =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeModifieds: [modified] } : { modifieds: [modified] },
                  ),
                deleteFromQuery: (oldQuery) =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeModifieds: [modified] } : { modifieds: [modified] },
                  ),
              }),
            ),
          )
          break
        }
        case 'owner':
        case '-owner': {
          const owners = assetTree
            .preorderTraversal()
            .flatMap((node) =>
              (node.item.permissions ?? [])
                .filter((permission) => permission.permission === permissions.PermissionAction.own)
                .map(backendModule.getAssetPermissionName),
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
                addToQuery: (oldQuery) =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeOwners: [owner] } : { owners: [owner] },
                  ),
                deleteFromQuery: (oldQuery) =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeOwners: [owner] } : { owners: [owner] },
                  ),
              }),
            ),
          )
          break
        }
        case 'label':
        case '-label': {
          setSuggestions(
            (labels ?? []).map(
              (label): assetSearchBar.Suggestion => ({
                render: () => (
                  <Label active color={label.color} onPress={() => {}}>
                    {label.value}
                  </Label>
                ),
                addToQuery: (oldQuery) =>
                  oldQuery.addToLastTerm(
                    negative ? { negativeLabels: [label.value] } : { labels: [label.value] },
                  ),
                deleteFromQuery: (oldQuery) =>
                  oldQuery.deleteFromLastTerm(
                    negative ? { negativeLabels: [label.value] } : { labels: [label.value] },
                  ),
              }),
            ),
          )
          break
        }
        default: {
          setSuggestions(shouldOmitNames ? [] : allVisible())
          break
        }
      }
    }
  }, [isCloud, assetTree, query, visibilities, labels, setSuggestions])

  React.useEffect(() => {
    assetTreeRef.current = assetTree
    const newNodeMap = new Map(assetTree.preorderTraversal().map((asset) => [asset.key, asset]))
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
  }, [hidden, inputBindings, dispatchAssetEvent])

  React.useEffect(
    () =>
      driveStore.subscribe(({ selectedKeys }) => {
        let newCanDownload: boolean
        if (!isCloud) {
          newCanDownload =
            selectedKeys.size !== 0 &&
            Array.from(selectedKeys).every((key) => {
              const node = nodeMapRef.current.get(key)
              return node?.item.type === backendModule.AssetType.project
            })
        } else {
          newCanDownload =
            selectedKeys.size !== 0 &&
            Array.from(selectedKeys).every((key) => {
              const node = nodeMapRef.current.get(key)
              return (
                node?.item.type === backendModule.AssetType.project ||
                node?.item.type === backendModule.AssetType.file ||
                node?.item.type === backendModule.AssetType.datalink
              )
            })
        }
        const currentCanDownload = driveStore.getState().canDownload
        if (currentCanDownload !== newCanDownload) {
          setCanDownload(newCanDownload)
        }
      }),
    [driveStore, isCloud, setCanDownload],
  )

  const overwriteNodes = useEventCallback((newAssets: readonly backendModule.AnyAsset[]) => {
    mostRecentlySelectedIndexRef.current = null
    selectionStartIndexRef.current = null
    const rootPath = 'rootPath' in category ? category.rootPath : backend.rootPath
    // This is required, otherwise we are using an outdated
    // `nameOfProjectToImmediatelyOpen`.
    const nameOfProjectToImmediatelyOpen = nameOfProjectToImmediatelyOpenRef.current
    const rootParentDirectoryId = backendModule.DirectoryId('')
    const rootDirectory = backendModule.createRootDirectoryAsset(rootDirectoryId)
    const rootId = rootDirectory.id
    const children = newAssets.map((asset) =>
      AssetTreeNode.fromAsset(asset, rootId, rootId, 0, `${rootPath}/${asset.title}`, null),
    )
    const newRootNode = new AssetTreeNode(
      rootDirectory,
      rootParentDirectoryId,
      rootParentDirectoryId,
      children,
      -1,
      rootPath,
      null,
      rootId,
      true,
    )
    setAssetTree(newRootNode)
    // The project name here might also be a string with project id, e.g.
    // when opening a project file from explorer on Windows.
    const isInitialProject = (asset: backendModule.AnyAsset) =>
      asset.title === nameOfProjectToImmediatelyOpen || asset.id === nameOfProjectToImmediatelyOpen
    if (nameOfProjectToImmediatelyOpen != null) {
      const projectToLoad = newAssets.filter(backendModule.assetIsProject).find(isInitialProject)
      if (projectToLoad != null) {
        const backendType = backendModule.BackendType.local
        const { id, title, parentId } = projectToLoad
        doOpenProject({ type: backendType, id, title, parentId })
      } else {
        toastAndLog('findProjectError', null, nameOfProjectToImmediatelyOpen)
      }
    }
    setQueuedAssetEvents((oldQueuedAssetEvents) => {
      if (oldQueuedAssetEvents.length !== 0) {
        queueMicrotask(() => {
          for (const event of oldQueuedAssetEvents) {
            dispatchAssetEvent(event)
          }
        })
      }
      return []
    })
    nameOfProjectToImmediatelyOpenRef.current = null
  })
  const overwriteNodesRef = React.useRef(overwriteNodes)
  overwriteNodesRef.current = overwriteNodes

  const rootDirectoryQuery = useBackendQuery(
    backend,
    'listDirectory',
    [
      {
        parentId: rootDirectoryId,
        filterBy: CATEGORY_TO_FILTER_BY[category.type],
        recentProjects: category.type === 'recent',
        labels: null,
      },
      // The root directory has no name. This is also SAFE, as there is a different error
      // message when the directory is the root directory (when `parentId == null`).
      '(root)',
    ],
    { queryKey: [], staleTime: 0, meta: { persist: false } },
  )
  const isLoading = rootDirectoryQuery.isLoading

  React.useEffect(() => {
    if (isLoading) {
      nameOfProjectToImmediatelyOpenRef.current = initialProjectName
    } else {
      // The project name here might also be a string with project id, e.g. when opening
      // a project file from explorer on Windows.
      const isInitialProject = (asset: backendModule.AnyAsset) =>
        asset.title === initialProjectName || asset.id === initialProjectName
      const projectToLoad = assetTree
        .preorderTraversal()
        .map((node) => node.item)
        .filter(backendModule.assetIsProject)
        .find(isInitialProject)
      if (projectToLoad != null) {
        doOpenProject({
          type: backendModule.BackendType.local,
          id: projectToLoad.id,
          title: projectToLoad.title,
          parentId: projectToLoad.parentId,
        })
      } else if (initialProjectName != null) {
        toastAndLog('findProjectError', null, initialProjectName)
      }
    }
    // This effect MUST only run when `initialProjectName` is changed.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [initialProjectName])

  React.useEffect(() => {
    if (rootDirectoryQuery.data) {
      overwriteNodes(rootDirectoryQuery.data)
    }
  }, [rootDirectoryQuery.data, overwriteNodes])

  React.useEffect(() => {
    const savedEnabledColumns = localStorage.get('enabledColumns')
    if (savedEnabledColumns != null) {
      setEnabledColumns(new Set(savedEnabledColumns))
    }
  }, [localStorage])

  React.useEffect(() => {
    localStorage.set('enabledColumns', [...enabledColumns])
  }, [enabledColumns, localStorage])

  React.useEffect(
    () =>
      driveStore.subscribe(({ selectedKeys }) => {
        if (selectedKeys.size !== 1) {
          setAssetPanelProps(null)
          setIsAssetPanelTemporarilyVisible(false)
        }
      }),
    [driveStore, setAssetPanelProps, setIsAssetPanelTemporarilyVisible],
  )

  const directoryListAbortControllersRef = React.useRef(
    new Map<backendModule.DirectoryId, AbortController>(),
  )
  const doToggleDirectoryExpansion = React.useCallback(
    (
      directoryId: backendModule.DirectoryId,
      key: backendModule.DirectoryId,
      title?: string | null,
      override?: boolean,
    ) => {
      const directory = nodeMapRef.current.get(key)
      const isExpanded = directory?.children != null && directory.isExpanded
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
        setAssetTree((oldAssetTree) =>
          oldAssetTree.map((item) => (item.key !== key ? item : item.with({ isExpanded: false }))),
        )
      } else {
        setAssetTree((oldAssetTree) =>
          oldAssetTree.map((item) =>
            item.key !== key ? item
            : item.children != null ? item.with({ isExpanded: true })
            : item.with({
                isExpanded: true,
                children: [
                  AssetTreeNode.fromAsset(
                    backendModule.createSpecialLoadingAsset(directoryId),
                    key,
                    directoryId,
                    item.depth + 1,
                    '',
                    null,
                  ),
                ],
              }),
          ),
        )
        void (async () => {
          const abortController = new AbortController()
          directoryListAbortControllersRef.current.set(directoryId, abortController)
          const displayedTitle = title ?? nodeMapRef.current.get(key)?.item.title ?? '(unknown)'
          const childAssets = await backend
            .listDirectory(
              {
                parentId: directoryId,
                filterBy: CATEGORY_TO_FILTER_BY[category.type],
                recentProjects: category.type === 'recent',
                labels: null,
              },
              displayedTitle,
            )
            .catch((error) => {
              toastAndLog('listFolderBackendError', error, displayedTitle)
              throw error
            })
          if (!abortController.signal.aborted) {
            setAssetTree((oldAssetTree) =>
              oldAssetTree.map((item) => {
                if (item.key !== key) {
                  return item
                } else {
                  const initialChildren = item.children?.filter(
                    (child) => child.item.type !== backendModule.AssetType.specialLoading,
                  )
                  const childAssetsMap = new Map(childAssets.map((asset) => [asset.id, asset]))
                  for (const child of initialChildren ?? []) {
                    const newChild = childAssetsMap.get(child.item.id)
                    if (newChild != null) {
                      child.item = newChild
                      childAssetsMap.delete(child.item.id)
                    }
                  }
                  const childAssetNodes = Array.from(childAssetsMap.values(), (child) =>
                    AssetTreeNode.fromAsset(
                      child,
                      key,
                      directoryId,
                      item.depth + 1,
                      `${item.path}/${child.title}`,
                      null,
                    ),
                  )
                  const specialEmptyAsset: backendModule.SpecialEmptyAsset | null =
                    (
                      (initialChildren != null && initialChildren.length !== 0) ||
                      childAssetNodes.length !== 0
                    ) ?
                      null
                    : backendModule.createSpecialEmptyAsset(directoryId)
                  const children =
                    specialEmptyAsset != null ?
                      [
                        AssetTreeNode.fromAsset(
                          specialEmptyAsset,
                          key,
                          directoryId,
                          item.depth + 1,
                          '',
                          null,
                        ),
                      ]
                    : initialChildren == null || initialChildren.length === 0 ? childAssetNodes
                    : [...initialChildren, ...childAssetNodes].sort(AssetTreeNode.compare)
                  return item.with({ children })
                }
              }),
            )
          }
        })()
      }
    },
    [category, backend, toastAndLog],
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
    [],
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
    const { selectedKeys } = driveStore.getState()
    const prevIndex = mostRecentlySelectedIndexRef.current
    const item = prevIndex == null ? null : visibleItems[prevIndex]
    if (selectedKeys.size === 1 && item != null) {
      switch (event.key) {
        case 'Enter':
        case ' ': {
          if (event.key === ' ' && event.ctrlKey) {
            const keys = selectedKeys
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
                doOpenProject({
                  type: backend.type,
                  id: item.item.id,
                  title: item.item.title,
                  parentId: item.item.parentId,
                })
                break
              }
              case backendModule.AssetType.datalink: {
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
                        await updateSecret([id, { value }, item.item.title])
                      } catch (error) {
                        toastAndLog(null, error)
                      }
                    }}
                  />,
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
          const keys = selectedKeys
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
              event.key === 'ArrowUp' ?
                Math.max(0, index - 1)
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
          setSelectedKeys(new Set(selection.map((newItem) => newItem.key)))
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
        parentKey == null ?
          assetTree.children ?? []
        : nodeMapRef.current.get(parentKey)?.children ?? []
      const projectIndices = siblings
        .map((node) => node.item)
        .filter(backendModule.assetIsProject)
        .map((item) => projectNameTemplate.exec(item.title)?.groups?.projectIndex)
        .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
      return `${prefix}${Math.max(0, ...projectIndices) + 1}`
    },
    [assetTree, nodeMapRef],
  )

  const deleteAsset = React.useCallback((key: backendModule.AssetId) => {
    setAssetTree((oldAssetTree) => oldAssetTree.filter((item) => item.key !== key))
  }, [])

  /** All items must have the same type. */
  const insertAssets = React.useCallback(
    (
      assets: readonly backendModule.AnyAsset[],
      parentKey: backendModule.DirectoryId | null,
      parentId: backendModule.DirectoryId | null,
      getInitialAssetEvents: (id: backendModule.AssetId) => readonly assetEvent.AssetEvent[] | null,
    ) => {
      const actualParentKey = parentKey ?? rootDirectoryId
      const actualParentId = parentId ?? rootDirectoryId
      setAssetTree((oldAssetTree) =>
        oldAssetTree.map((item) =>
          item.key !== actualParentKey ?
            item
          : insertAssetTreeNodeChildren(
              item,
              assets,
              actualParentKey,
              actualParentId,
              getInitialAssetEvents,
            ),
        ),
      )
    },
    [rootDirectoryId],
  )

  const insertArbitraryAssets = React.useCallback(
    (
      assets: backendModule.AnyAsset[],
      parentKey: backendModule.DirectoryId | null,
      parentId: backendModule.DirectoryId | null,
      getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null,
      getInitialAssetEvents: (
        id: backendModule.AssetId,
      ) => readonly assetEvent.AssetEvent[] | null = () => null,
    ) => {
      const actualParentKey = parentKey ?? rootDirectoryId
      const actualParentId = parentId ?? rootDirectoryId
      setAssetTree((oldAssetTree) => {
        return oldAssetTree.map((item) =>
          item.key !== actualParentKey ?
            item
          : insertArbitraryAssetTreeNodeChildren(
              item,
              assets,
              actualParentKey,
              actualParentId,
              getKey,
              getInitialAssetEvents,
            ),
        )
      })
    },
    [rootDirectoryId],
  )

  // This is not a React component, even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const onAssetListEvent = (event: assetListEvent.AssetListEvent) => {
    switch (event.type) {
      case AssetListEventType.newFolder: {
        const parent = nodeMapRef.current.get(event.parentKey)
        const siblings = parent?.children ?? []
        const directoryIndices = siblings
          .map((node) => node.item)
          .filter(backendModule.assetIsDirectory)
          .map((item) => /^New Folder (?<directoryIndex>\d+)$/.exec(item.title))
          .map((match) => match?.groups?.directoryIndex)
          .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        const title = `New Folder ${Math.max(0, ...directoryIndices) + 1}`
        const placeholderItem: backendModule.DirectoryAsset = {
          type: backendModule.AssetType.directory,
          id: backendModule.DirectoryId(uniqueString.uniqueString()),
          title,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryCreateOwnerPermission(
            `${parent?.path ?? ''}/${title}`,
            category,
            user,
            users ?? [],
            userGroups ?? [],
          ),
          projectState: null,
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId, () => [
          { type: AssetEventType.newFolder, placeholderId: placeholderItem.id },
        ])
        break
      }
      case AssetListEventType.newProject: {
        const parent = nodeMapRef.current.get(event.parentKey)
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
          permissions: permissions.tryCreateOwnerPermission(
            `${parent?.path ?? ''}/${projectName}`,
            category,
            user,
            users ?? [],
            userGroups ?? [],
          ),
          projectState: {
            type: backendModule.ProjectState.placeholder,
            volumeId: '',
            openedBy: user.email,
            ...(path != null ? { path } : {}),
          },
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId, () => [
          {
            type: AssetEventType.newProject,
            placeholderId: dummyId,
            templateId: event.templateId,
            datalinkId: event.datalinkId,
            originalId: null,
            versionId: null,
            ...(event.onCreated ? { onCreated: event.onCreated } : {}),
            ...(event.onError ? { onError: event.onError } : {}),
          },
        ])
        break
      }
      case AssetListEventType.uploadFiles: {
        const localBackend = backend instanceof LocalBackend ? backend : null
        const reversedFiles = Array.from(event.files).reverse()
        const parent = nodeMapRef.current.get(event.parentKey)
        const siblingNodes = parent?.children ?? []
        const siblings = siblingNodes.map((node) => node.item)
        const siblingFiles = siblings.filter(backendModule.assetIsFile)
        const siblingProjects = siblings.filter(backendModule.assetIsProject)
        const siblingFileTitles = new Set(siblingFiles.map((asset) => asset.title))
        const siblingProjectTitles = new Set(siblingProjects.map((asset) => asset.title))
        const files = reversedFiles.filter(backendModule.fileIsNotProject)
        const projects = reversedFiles.filter(backendModule.fileIsProject)
        const duplicateFiles = files.filter((file) => siblingFileTitles.has(file.name))
        const duplicateProjects = projects.filter((project) =>
          siblingProjectTitles.has(backendModule.stripProjectExtension(project.name)),
        )
        const ownerPermission = permissions.tryCreateOwnerPermission(
          parent?.path ?? '',
          category,
          user,
          users ?? [],
          userGroups ?? [],
        )
        const fileMap = new Map<backendModule.AssetId, File>()
        const getInitialAssetEvents = (
          id: backendModule.AssetId,
        ): readonly assetEvent.AssetEvent[] | null => {
          const file = fileMap.get(id)
          return file == null ? null : (
              [{ type: AssetEventType.uploadFiles, files: new Map([[id, file]]) }]
            )
        }
        if (duplicateFiles.length === 0 && duplicateProjects.length === 0) {
          const placeholderFiles = files.map((file) => {
            const asset = backendModule.createPlaceholderFileAsset(
              file.name,
              event.parentId,
              ownerPermission,
            )
            fileMap.set(asset.id, file)
            return asset
          })
          const placeholderProjects = projects.map((project) => {
            const basename = backendModule.stripProjectExtension(project.name)
            const asset = backendModule.createPlaceholderProjectAsset(
              basename,
              event.parentId,
              ownerPermission,
              user,
              localBackend?.joinPath(event.parentId, basename) ?? null,
            )
            fileMap.set(asset.id, project)
            return asset
          })
          doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
          insertAssets(placeholderFiles, event.parentKey, event.parentId, getInitialAssetEvents)
          insertAssets(placeholderProjects, event.parentKey, event.parentId, getInitialAssetEvents)
        } else {
          const siblingFilesByName = new Map(siblingFiles.map((file) => [file.title, file]))
          const siblingProjectsByName = new Map(
            siblingProjects.map((project) => [project.title, project]),
          )
          const conflictingFiles = duplicateFiles.map((file) => ({
            // This is SAFE, as `duplicateFiles` only contains files that have siblings
            // with the same name.
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            current: siblingFilesByName.get(file.name)!,
            new: backendModule.createPlaceholderFileAsset(
              file.name,
              event.parentId,
              ownerPermission,
            ),
            file,
          }))
          const conflictingProjects = duplicateProjects.map((project) => {
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
                localBackend?.joinPath(event.parentId, basename) ?? null,
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
              siblingFileNames={siblingFilesByName.keys()}
              siblingProjectNames={siblingProjectsByName.keys()}
              nonConflictingFileCount={files.length - conflictingFiles.length}
              nonConflictingProjectCount={projects.length - conflictingProjects.length}
              doUploadNonConflicting={() => {
                doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
                const newFiles = files
                  .filter((file) => !siblingFileTitles.has(file.name))
                  .map((file) => {
                    const asset = backendModule.createPlaceholderFileAsset(
                      file.name,
                      event.parentId,
                      ownerPermission,
                    )
                    fileMap.set(asset.id, file)
                    return asset
                  })
                const newProjects = projects
                  .filter(
                    (project) =>
                      !siblingProjectTitles.has(backendModule.stripProjectExtension(project.name)),
                  )
                  .map((project) => {
                    const basename = backendModule.stripProjectExtension(project.name)
                    const asset = backendModule.createPlaceholderProjectAsset(
                      basename,
                      event.parentId,
                      ownerPermission,
                      user,
                      localBackend?.joinPath(event.parentId, basename) ?? null,
                    )
                    fileMap.set(asset.id, project)
                    return asset
                  })
                insertAssets(newFiles, event.parentKey, event.parentId, getInitialAssetEvents)
                insertAssets(newProjects, event.parentKey, event.parentId, getInitialAssetEvents)
              }}
            />,
          )
        }
        break
      }
      case AssetListEventType.newDatalink: {
        const parent = nodeMapRef.current.get(event.parentKey)
        const placeholderItem: backendModule.DatalinkAsset = {
          type: backendModule.AssetType.datalink,
          id: backendModule.DatalinkId(uniqueString.uniqueString()),
          title: event.name,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryCreateOwnerPermission(
            `${parent?.path ?? ''}/${event.name}`,
            category,
            user,
            users ?? [],
            userGroups ?? [],
          ),
          projectState: null,
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId, () => [
          {
            type: AssetEventType.newDatalink,
            placeholderId: placeholderItem.id,
            value: event.value,
          },
        ])
        break
      }
      case AssetListEventType.newSecret: {
        const parent = nodeMapRef.current.get(event.parentKey)
        const placeholderItem: backendModule.SecretAsset = {
          type: backendModule.AssetType.secret,
          id: backendModule.SecretId(uniqueString.uniqueString()),
          title: event.name,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryCreateOwnerPermission(
            `${parent?.path ?? ''}/${event.name}`,
            category,
            user,
            users ?? [],
            userGroups ?? [],
          ),
          projectState: null,
          labels: [],
          description: null,
        }
        doToggleDirectoryExpansion(event.parentId, event.parentKey, null, true)
        insertAssets([placeholderItem], event.parentKey, event.parentId, () => [
          { type: AssetEventType.newSecret, placeholderId: placeholderItem.id, value: event.value },
        ])
        break
      }
      case AssetListEventType.insertAssets: {
        insertArbitraryAssets(event.assets, event.parentKey, event.parentId)
        break
      }
      case AssetListEventType.duplicateProject: {
        const parent = nodeMapRef.current.get(event.parentKey)
        const siblings = parent?.children ?? []
        const siblingTitles = new Set(siblings.map((sibling) => sibling.item.title))
        let index = 1
        let title = `${event.original.title} (${index})`
        while (siblingTitles.has(title)) {
          index += 1
          title = `${event.original.title} (${index})`
        }
        const placeholderItem: backendModule.ProjectAsset = {
          type: backendModule.AssetType.project,
          id: backendModule.ProjectId(uniqueString.uniqueString()),
          title,
          modifiedAt: dateTime.toRfc3339(new Date()),
          parentId: event.parentId,
          permissions: permissions.tryCreateOwnerPermission(
            `${parent?.path ?? ''}/${title}`,
            category,
            user,
            users ?? [],
            userGroups ?? [],
          ),
          projectState: {
            type: backendModule.ProjectState.placeholder,
            volumeId: '',
            openedBy: user.email,
          },
          labels: [],
          description: null,
        }
        insertAssets([placeholderItem], event.parentKey, event.parentId, () => [
          {
            type: AssetEventType.newProject,
            placeholderId: placeholderItem.id,
            templateId: null,
            datalinkId: null,
            originalId: event.original.id,
            versionId: event.versionId,
          },
        ])
        break
      }
      case AssetListEventType.willDelete: {
        const { selectedKeys } = driveStore.getState()
        if (selectedKeys.has(event.key)) {
          const newSelectedKeys = new Set(selectedKeys)
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
        const assetEvents: readonly assetEvent.AssetEvent[] = [
          {
            type: AssetEventType.copy,
            ids,
            newParentKey: event.newParentKey,
            newParentId: event.newParentId,
          },
        ]
        const newParent = nodeMapRef.current.get(event.newParentKey)
        const newOwner =
          !isCloud || !newParent ?
            null
          : permissions.newOwnerFromPath(newParent.path, users ?? [], userGroups ?? [])
        insertArbitraryAssets(
          newOwner ?
            event.items.map((item) => permissions.replaceOwnerPermission(item, newOwner))
          : event.items,
          event.newParentKey,
          event.newParentId,
          getKey,
          () => assetEvents,
        )
        break
      }
      case AssetListEventType.move: {
        deleteAsset(event.key)
        const newParent = nodeMapRef.current.get(event.newParentKey)
        const newOwner =
          !isCloud || !newParent ?
            null
          : permissions.newOwnerFromPath(newParent.path, users ?? [], userGroups ?? [])
        insertAssets(
          [newOwner ? permissions.replaceOwnerPermission(event.item, newOwner) : event.item],
          event.newParentKey,
          event.newParentId,
          () => null,
        )
        break
      }
      case AssetListEventType.delete: {
        deleteAsset(event.key)
        break
      }
      case AssetListEventType.emptyTrash: {
        if (category.type !== 'trash') {
          toastAndLog('canOnlyEmptyTrashWhenInTrash')
        } else if (assetTree.children != null) {
          const ids = new Set(
            assetTree.children
              .map((child) => child.item.id)
              .filter((id) => !isSpecialReadonlyDirectoryId(id)),
          )
          // This is required to prevent an infinite loop.
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
  eventListProvider.useAssetListEventListener((event) => {
    if (!isLoading) {
      onAssetListEvent(event)
    } else {
      queuedAssetListEventsRef.current.push(event)
    }
  })

  const doCopy = React.useCallback(() => {
    unsetModal()
    const { selectedKeys } = driveStore.getState()
    setPasteData({ type: PasteType.copy, data: selectedKeys })
  }, [driveStore, unsetModal])

  const doCut = React.useCallback(() => {
    unsetModal()
    if (pasteData != null) {
      dispatchAssetEvent({ type: AssetEventType.cancelCut, ids: pasteData.data })
    }
    const { selectedKeys } = driveStore.getState()
    setPasteData({ type: PasteType.move, data: selectedKeys })
    dispatchAssetEvent({ type: AssetEventType.cut, ids: selectedKeys })
    setSelectedKeys(new Set())
  }, [unsetModal, pasteData, driveStore, dispatchAssetEvent, setSelectedKeys])

  const doPaste = React.useCallback(
    (newParentKey: backendModule.DirectoryId, newParentId: backendModule.DirectoryId) => {
      unsetModal()
      if (pasteData != null) {
        if (pasteData.data.has(newParentKey)) {
          toast.toast.error('Cannot paste a folder into itself.')
        } else {
          doToggleDirectoryExpansion(newParentId, newParentKey, null, true)
          if (pasteData.type === PasteType.copy) {
            const assets = Array.from(pasteData.data, (id) => nodeMapRef.current.get(id)).flatMap(
              (asset) => (asset ? [asset.item] : []),
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
    [pasteData, doToggleDirectoryExpansion, unsetModal, dispatchAssetEvent, dispatchAssetListEvent],
  )

  const hideColumn = React.useCallback((column: columnUtils.Column) => {
    setEnabledColumns((columns) => set.withPresence(columns, column, false))
  }, [])

  const hiddenContextMenu = React.useMemo(
    () => (
      <AssetsTableContextMenu
        hidden
        backend={backend}
        category={category}
        pasteData={pasteData}
        nodeMapRef={nodeMapRef}
        rootDirectoryId={rootDirectoryId}
        event={{ pageX: 0, pageY: 0 }}
        doCopy={doCopy}
        doCut={doCut}
        doPaste={doPaste}
      />
    ),
    [backend, rootDirectoryId, category, pasteData, doCopy, doCut, doPaste],
  )

  const onDropzoneDragOver = (event: React.DragEvent<Element>) => {
    const payload = drag.ASSET_ROWS.lookup(event)
    const filtered = payload?.filter((item) => item.asset.parentId !== rootDirectoryId)
    if (filtered != null && filtered.length > 0) {
      event.preventDefault()
    } else if (event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
    }
  }

  const updateIsDraggingFiles = (event: React.DragEvent<Element>) => {
    if (event.dataTransfer.types.includes('Files')) {
      setIsDraggingFiles(true)
      setDroppedFilesCount(event.dataTransfer.items.length)
    }
  }

  const handleFileDrop = (event: React.DragEvent) => {
    if (event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
      event.stopPropagation()
      dispatchAssetListEvent({
        type: AssetListEventType.uploadFiles,
        parentKey: rootDirectoryId,
        parentId: rootDirectoryId,
        files: Array.from(event.dataTransfer.files),
      })
    }
  }

  const state = React.useMemo<AssetsTableState>(
    // The type MUST be here to trigger excess property errors at typecheck time.
    () => ({
      backend,
      rootDirectoryId,
      visibilities,
      scrollContainerRef: rootRef,
      category,
      hasPasteData: pasteData != null,
      setPasteData,
      sortInfo,
      setSortInfo,
      query,
      setQuery,
      setAssetPanelProps,
      setIsAssetPanelTemporarilyVisible,
      nodeMap: nodeMapRef,
      pasteData: pasteDataRef,
      hideColumn,
      doToggleDirectoryExpansion,
      doCopy,
      doCut,
      doPaste,
    }),
    [
      backend,
      rootDirectoryId,
      visibilities,
      category,
      pasteData,
      sortInfo,
      query,
      doToggleDirectoryExpansion,
      doCopy,
      doCut,
      doPaste,
      hideColumn,
      setAssetPanelProps,
      setIsAssetPanelTemporarilyVisible,
      setQuery,
    ],
  )

  // This is required to prevent the table body from overlapping the table header, because
  // the table header is transparent.
  const updateClipPath = useOnScroll(() => {
    if (bodyRef.current != null && rootRef.current != null) {
      bodyRef.current.style.clipPath = `inset(${rootRef.current.scrollTop}px 0 0 0)`
    }
    if (
      backend.type === backendModule.BackendType.remote &&
      rootRef.current != null &&
      headerRowRef.current != null
    ) {
      const hiddenColumnsCount = columnUtils.CLOUD_ENTERPRISE_COLUMNS.length - enabledColumns.size
      const shrinkBy =
        COLUMNS_SELECTOR_BASE_WIDTH_PX + COLUMNS_SELECTOR_ICON_WIDTH_PX * hiddenColumnsCount
      const rightOffset = rootRef.current.clientWidth + rootRef.current.scrollLeft - shrinkBy
      headerRowRef.current.style.clipPath = `polygon(0 0, ${rightOffset}px 0, ${rightOffset}px 100%, 0 100%)`
    }
  }, [backend.type, enabledColumns.size])

  const updateClipPathObserver = React.useMemo(
    () => new ResizeObserver(updateClipPath),
    [updateClipPath],
  )

  React.useEffect(
    () =>
      inputBindings.attach(
        sanitizedEventTargets.document.body,
        'click',
        {
          selectAdditional: () => {},
          selectAdditionalRange: () => {},
          [inputBindingsModule.DEFAULT_HANDLER]: () => {
            const { selectedKeys } = driveStore.getState()
            if (selectedKeys.size !== 0) {
              setSelectedKeys(set.EMPTY_SET)
              setMostRecentlySelectedIndex(null)
            }
          },
        },
        false,
      ),
    [setSelectedKeys, inputBindings, setMostRecentlySelectedIndex, driveStore],
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
      getRange: () => backendModule.AssetId[],
    ) => {
      event.stopPropagation()
      let result = new Set<backendModule.AssetId>()
      inputBindings.handler({
        selectRange: () => {
          result = new Set(getRange())
        },
        selectAdditionalRange: () => {
          const { selectedKeys } = driveStore.getState()
          result = new Set([...selectedKeys, ...getRange()])
        },
        selectAdditional: () => {
          const { selectedKeys } = driveStore.getState()
          const newSelectedKeys = new Set(selectedKeys)
          let count = 0
          for (const key of keys) {
            if (selectedKeys.has(key)) {
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
    [driveStore, inputBindings],
  )

  const { startAutoScroll, endAutoScroll, onMouseEvent } = autoScrollHooks.useAutoScroll(rootRef)

  const dragSelectionChangeLoopHandle = React.useRef(0)
  const dragSelectionRangeRef = React.useRef<DragSelectionInfo | null>(null)
  const onSelectionDrag = React.useCallback(
    (rectangle: geometry.DetailedRectangle, event: MouseEvent) => {
      startAutoScroll()
      onMouseEvent(event)
      if (mostRecentlySelectedIndexRef.current != null) {
        setKeyboardSelectedIndex(null)
      }
      cancelAnimationFrame(dragSelectionChangeLoopHandle.current)
      const scrollContainer = rootRef.current
      if (scrollContainer != null) {
        const rect = scrollContainer.getBoundingClientRect()
        const overlapsHorizontally = rect.right > rectangle.left && rect.left < rectangle.right
        const selectionTop = Math.max(0, rectangle.top - rect.top - ROW_HEIGHT_PX)
        const selectionBottom = Math.max(
          0,
          Math.min(rect.height, rectangle.bottom - rect.top - ROW_HEIGHT_PX),
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
          setVisuallySelectedKeys(null)
        } else {
          const keys = displayItems.slice(range.start, range.end).map((node) => node.key)
          setVisuallySelectedKeys(calculateNewKeys(event, keys, () => []))
        }
      }
    },
    [startAutoScroll, onMouseEvent, setVisuallySelectedKeys, displayItems, calculateNewKeys],
  )

  const onSelectionDragEnd = React.useCallback(
    (event: MouseEvent) => {
      endAutoScroll()
      onMouseEvent(event)
      const range = dragSelectionRangeRef.current
      if (range != null) {
        const keys = displayItems.slice(range.start, range.end).map((node) => node.key)
        setSelectedKeys(calculateNewKeys(event, keys, () => []))
      }
      setVisuallySelectedKeys(null)
      dragSelectionRangeRef.current = null
    },
    [
      endAutoScroll,
      onMouseEvent,
      setVisuallySelectedKeys,
      displayItems,
      setSelectedKeys,
      calculateNewKeys,
    ],
  )

  const onSelectionDragCancel = React.useCallback(() => {
    setVisuallySelectedKeys(null)
    dragSelectionRangeRef.current = null
  }, [setVisuallySelectedKeys])

  const onRowClick = React.useCallback(
    (innerRowProps: assetRow.AssetRowInnerProps, event: React.MouseEvent) => {
      const { key } = innerRowProps
      event.stopPropagation()
      const newIndex = visibleItems.findIndex((innerItem) => innerItem.key === key)
      const getRange = () => {
        if (mostRecentlySelectedIndexRef.current == null) {
          return [key]
        } else {
          const index1 = mostRecentlySelectedIndexRef.current
          const index2 = newIndex
          const startIndex = Math.min(index1, index2)
          const endIndex = Math.max(index1, index2) + 1
          return visibleItems.slice(startIndex, endIndex).map((innerItem) => innerItem.key)
        }
      }
      setSelectedKeys(calculateNewKeys(event, [key], getRange))
      setMostRecentlySelectedIndex(newIndex)
      if (!event.shiftKey) {
        selectionStartIndexRef.current = null
      }
    },
    [visibleItems, calculateNewKeys, setSelectedKeys, setMostRecentlySelectedIndex],
  )

  const getAsset = React.useCallback(
    (key: backendModule.AssetId) => nodeMapRef.current.get(key)?.item ?? null,
    [nodeMapRef],
  )

  const setAsset = React.useCallback(
    (key: backendModule.AssetId, asset: backendModule.AnyAsset) => {
      setAssetTree((oldAssetTree) =>
        oldAssetTree.map((item) => (item.key === key ? item.with({ item: asset }) : item)),
      )
      updateAssetRef.current[asset.id]?.(asset)
    },
    [],
  )

  React.useImperativeHandle(assetManagementApiRef, () => ({
    getAsset,
    setAsset,
  }))

  const columns = columnUtils.getColumnList(user, backend.type, enabledColumns)

  const headerRow = (
    <tr ref={headerRowRef} className="sticky top-[1px] text-sm font-semibold">
      {columns.map((column) => {
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

  const itemRows =
    isLoading ?
      <tr className="h-row">
        <td colSpan={columns.length} className="bg-transparent">
          <div className="grid w-container justify-around">
            <Spinner size={LOADING_SPINNER_SIZE_PX} state={spinnerState} />
          </div>
        </td>
      </tr>
    : displayItems.map((item, i) => {
        return (
          <AssetRow
            key={item.key}
            updateAssetRef={(instance) => {
              if (instance != null) {
                updateAssetRef.current[item.item.id] = instance
              } else {
                // Hacky way to clear the reference to the asset on unmount.
                // eventually once we pull the assets up in the tree, we can remove this.
                // eslint-disable-next-line @typescript-eslint/no-dynamic-delete
                delete updateAssetRef.current[item.item.id]
              }
            }}
            isOpened={openedProjects.some(({ id }) => item.item.id === id)}
            columns={columns}
            item={item}
            state={state}
            hidden={hidden || visibilities.get(item.key) === Visibility.hidden}
            isKeyboardSelected={
              keyboardSelectedIndex != null && item === visibleItems[keyboardSelectedIndex]
            }
            grabKeyboardFocus={() => {
              setSelectedKeys(new Set([item.key]))
              setMostRecentlySelectedIndex(i, true)
            }}
            onClick={onRowClick}
            select={() => {
              setMostRecentlySelectedIndex(visibleItems.indexOf(item))
              selectionStartIndexRef.current = null
              setSelectedKeys(new Set([item.key]))
            }}
            onDragStart={(event) => {
              startAutoScroll()
              onMouseEvent(event)
              let newSelectedKeys = driveStore.getState().selectedKeys
              if (!newSelectedKeys.has(item.key)) {
                setMostRecentlySelectedIndex(visibleItems.indexOf(item))
                selectionStartIndexRef.current = null
                newSelectedKeys = new Set([item.key])
                setSelectedKeys(newSelectedKeys)
              }
              const nodes = assetTree
                .preorderTraversal()
                .filter((node) => newSelectedKeys.has(node.key))
              const payload: drag.AssetRowsDragPayload = nodes.map((node) => ({
                key: node.key,
                asset: node.item,
              }))
              event.dataTransfer.setData(
                mimeTypes.ASSETS_MIME_TYPE,
                JSON.stringify(nodes.map((node) => node.key)),
              )
              drag.setDragImageToBlank(event)
              drag.ASSET_ROWS.bind(event, payload)
              setModal(
                <DragModal
                  event={event}
                  className="flex flex-col rounded-default bg-selected-frame backdrop-blur-default"
                  onDragEnd={() => {
                    drag.ASSET_ROWS.unbind(payload)
                  }}
                >
                  {nodes.map((node) => (
                    <NameColumn
                      key={node.key}
                      isOpened={false}
                      keyProp={node.key}
                      item={node.with({ depth: 0 })}
                      backendType={backend.type}
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
                </DragModal>,
              )
            }}
            onDragOver={(event) => {
              onMouseEvent(event)
              const payload = drag.LABELS.lookup(event)
              if (payload != null) {
                event.preventDefault()
                event.stopPropagation()
                const { selectedKeys } = driveStore.getState()
                const idsReference = selectedKeys.has(item.key) ? selectedKeys : item.key
                // This optimization is required in order to avoid severe lag on Firefox.
                if (idsReference !== lastSelectedIdsRef.current) {
                  lastSelectedIdsRef.current = idsReference
                  const ids =
                    typeof idsReference === 'string' ? new Set([idsReference]) : idsReference
                  let labelsPresent = 0
                  for (const selectedKey of ids) {
                    const nodeLabels = nodeMapRef.current.get(selectedKey)?.item.labels
                    if (nodeLabels != null) {
                      for (const label of nodeLabels) {
                        if (payload.has(label)) {
                          labelsPresent += 1
                        }
                      }
                    }
                  }
                  const shouldAdd = labelsPresent * 2 < ids.size * payload.size
                  window.setTimeout(() => {
                    dispatchAssetEvent({
                      type:
                        shouldAdd ?
                          AssetEventType.temporarilyAddLabels
                        : AssetEventType.temporarilyRemoveLabels,
                      ids,
                      labelNames: payload,
                    })
                  })
                }
              }
            }}
            onDragEnd={() => {
              endAutoScroll()
              lastSelectedIdsRef.current = null
              const { selectedKeys } = driveStore.getState()
              dispatchAssetEvent({
                type: AssetEventType.temporarilyAddLabels,
                ids: selectedKeys,
                labelNames: set.EMPTY_SET,
              })
            }}
            onDrop={(event) => {
              endAutoScroll()
              const { selectedKeys } = driveStore.getState()
              const ids = new Set(selectedKeys.has(item.key) ? selectedKeys : [item.key])
              const payload = drag.LABELS.lookup(event)
              if (payload != null) {
                event.preventDefault()
                event.stopPropagation()
                let labelsPresent = 0
                for (const selectedKey of ids) {
                  const nodeLabels = nodeMapRef.current.get(selectedKey)?.item.labels
                  if (nodeLabels != null) {
                    for (const label of nodeLabels) {
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
                  labelNames: set.EMPTY_SET,
                })
              }
            }}
          />
        )
      })

  const dropzoneText =
    isDraggingFiles ?
      droppedFilesCount === 1 ?
        getText('assetsDropFileDescription')
      : getText('assetsDropFilesDescription', droppedFilesCount)
    : getText('assetsDropzoneDescription')

  const table = (
    <div
      className="flex grow flex-col"
      onContextMenu={(event) => {
        if (isAssetContextMenuVisible) {
          event.preventDefault()
          event.stopPropagation()
          setModal(
            <AssetsTableContextMenu
              backend={backend}
              category={category}
              pasteData={pasteData}
              nodeMapRef={nodeMapRef}
              event={event}
              rootDirectoryId={rootDirectoryId}
              doCopy={doCopy}
              doCut={doCut}
              doPaste={doPaste}
            />,
          )
        }
      }}
      onDragLeave={(event) => {
        const payload = drag.LABELS.lookup(event)
        if (
          payload != null &&
          event.relatedTarget instanceof Node &&
          !event.currentTarget.contains(event.relatedTarget)
        ) {
          lastSelectedIdsRef.current = null
          const { selectedKeys } = driveStore.getState()
          dispatchAssetEvent({
            type: AssetEventType.temporarilyAddLabels,
            ids: selectedKeys,
            labelNames: set.EMPTY_SET,
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
              {category.type === 'trash' ?
                <aria.Text className="px-cell-x placeholder">
                  {query.query !== '' ?
                    getText('noFilesMatchTheCurrentFilters')
                  : getText('yourTrashIsEmpty')}
                </aria.Text>
              : category.type === 'recent' ?
                <aria.Text className="px-cell-x placeholder">
                  {query.query !== '' ?
                    getText('noFilesMatchTheCurrentFilters')
                  : getText('youHaveNoRecentProjects')}
                </aria.Text>
              : query.query !== '' ?
                <aria.Text className="px-cell-x placeholder">
                  {getText('noFilesMatchTheCurrentFilters')}
                </aria.Text>
              : <aria.Text className="px-cell-x placeholder">{getText('youHaveNoFiles')}</aria.Text>
              }
            </td>
          </tr>
        </tbody>
      </table>
      <div
        data-testid="root-directory-dropzone"
        className={tailwindMerge.twMerge(
          'sticky left-0 grid max-w-container grow place-items-center',
          (category.type === 'recent' || category.type === 'trash') && 'hidden',
        )}
        onDragEnter={onDropzoneDragOver}
        onDragOver={onDropzoneDragOver}
        onDragLeave={(event) => {
          lastSelectedIdsRef.current = null
          if (event.currentTarget === event.target) {
            setIsDraggingFiles(false)
          }
        }}
        onDrop={(event) => {
          const payload = drag.ASSET_ROWS.lookup(event)
          const filtered = payload?.filter((item) => item.asset.parentId !== rootDirectoryId)
          if (filtered != null && filtered.length > 0) {
            event.preventDefault()
            event.stopPropagation()
            unsetModal()
            dispatchAssetEvent({
              type: AssetEventType.move,
              newParentKey: rootDirectoryId,
              newParentId: rootDirectoryId,
              ids: new Set(filtered.map((dragItem) => dragItem.asset.id)),
            })
          }
          handleFileDrop(event)
        }}
        onClick={() => {
          setSelectedKeys(new Set())
        }}
      >
        <aria.FileTrigger
          onSelect={(event) => {
            dispatchAssetListEvent({
              type: AssetListEventType.uploadFiles,
              parentKey: rootDirectoryId,
              parentId: rootDirectoryId,
              files: Array.from(event ?? []),
            })
          }}
        >
          <FocusRing>
            <aria.Button
              ref={mainDropzoneRef}
              className="my-20 flex flex-col items-center gap-3 text-primary/30 transition-colors duration-200 hover:text-primary/50"
              onPress={() => {}}
            >
              <SvgMask src={DropFilesImage} className="size-[186px]" />
              {dropzoneText}
            </aria.Button>
          </FocusRing>
        </aria.FileTrigger>
      </div>
    </div>
  )

  return !isCloud && didLoadingProjectManagerFail ?
      <ErrorDisplay
        error={getText('couldNotConnectToPM')}
        resetErrorBoundary={reconnectToProjectManager}
      />
    : <div className="relative grow">
        <FocusArea direction="vertical">
          {(innerProps) => (
            <div
              {...aria.mergeProps<JSX.IntrinsicElements['div']>()(innerProps, {
                ref: (value) => {
                  rootRef.current = value
                  cleanupRootRef.current()
                  if (value) {
                    updateClipPathObserver.observe(value)
                    cleanupRootRef.current = () => {
                      updateClipPathObserver.unobserve(value)
                    }
                  } else {
                    cleanupRootRef.current = () => {}
                  }
                },
                className: 'flex-1 overflow-auto container-size w-full h-full',
                onKeyDown,
                onScroll: updateClipPath,
                onBlur: (event) => {
                  if (
                    event.relatedTarget instanceof HTMLElement &&
                    !event.currentTarget.contains(event.relatedTarget)
                  ) {
                    setKeyboardSelectedIndex(null)
                  }
                },
                onDragEnter: updateIsDraggingFiles,
                onDragOver: updateIsDraggingFiles,
                onDragLeave: (event) => {
                  if (
                    !(event.relatedTarget instanceof Node) ||
                    !event.currentTarget.contains(event.relatedTarget)
                  ) {
                    lastSelectedIdsRef.current = null
                    setIsDraggingFiles(false)
                  }
                },
              })}
            >
              {!hidden && hiddenContextMenu}
              {!hidden && (
                <SelectionBrush
                  targetRef={rootRef}
                  margin={8}
                  onDrag={onSelectionDrag}
                  onDragEnd={onSelectionDragEnd}
                  onDragCancel={onSelectionDragCancel}
                />
              )}
              <div className="flex h-max min-h-full w-max min-w-full flex-col">
                {isCloud && (
                  <div className="flex-0 sticky top-0 flex h-0 flex-col">
                    <div
                      data-testid="extra-columns"
                      className="sticky right-0 flex self-end px-2 py-3"
                    >
                      <FocusArea direction="horizontal">
                        {(columnsBarProps) => (
                          <div
                            {...aria.mergeProps<JSX.IntrinsicElements['div']>()(columnsBarProps, {
                              className: 'inline-flex gap-icons',
                              onFocus: () => {
                                setKeyboardSelectedIndex(null)
                              },
                            })}
                          >
                            {columnUtils.CLOUD_ENTERPRISE_COLUMNS.filter(
                              (column) => !enabledColumns.has(column),
                            ).map((column) => (
                              <Button
                                key={column}
                                light
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
        {isDraggingFiles && !isMainDropzoneVisible && (
          <div className="pointer-events-none absolute bottom-4 left-1/2 -translate-x-1/2">
            <div
              className="flex items-center justify-center gap-3 rounded-default bg-selected-frame px-8 py-6 text-primary/50 backdrop-blur-3xl transition-all"
              onDragEnter={onDropzoneDragOver}
              onDragOver={onDropzoneDragOver}
              onDrop={(event) => {
                setIsDraggingFiles(false)
                handleFileDrop(event)
              }}
            >
              <SvgMask src={DropFilesImage} className="size-8" />
              {dropzoneText}
            </div>
          </div>
        )}
      </div>
}
