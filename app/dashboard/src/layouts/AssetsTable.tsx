/** @file Table displaying a list of projects. */
import * as React from 'react'

import {
  queryOptions,
  useMutation,
  useQueries,
  useQueryClient,
  useSuspenseQuery,
} from '@tanstack/react-query'
import * as toast from 'react-toastify'
import invariant from 'tiny-invariant'
import * as z from 'zod'

import DropFilesImage from '#/assets/drop_files.svg'

import * as mimeTypes from '#/data/mimeTypes'

import * as autoScrollHooks from '#/hooks/autoScrollHooks'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import * as intersectionHooks from '#/hooks/intersectionHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import useOnScroll from '#/hooks/useOnScroll'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import {
  useDriveStore,
  useSetAssetPanelProps,
  useSetCanCreateAssets,
  useSetCanDownload,
  useSetIsAssetPanelTemporarilyVisible,
  useSetNewestFolderId,
  useSetSelectedKeys,
  useSetSuggestions,
  useSetTargetDirectory,
  useSetVisuallySelectedKeys,
} from '#/providers/DriveProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'
import * as projectsProvider from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import { isLocalCategory, type Category } from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import type * as assetRow from '#/components/dashboard/AssetRow'
import { AssetRow } from '#/components/dashboard/AssetRow'
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
import LocalBackend, * as localBackendModule from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'
import { isSpecialReadonlyDirectoryId } from '#/services/RemoteBackend'

import { ErrorDisplay } from '#/components/ErrorBoundary'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
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
  schema: z.nativeEnum(columnUtils.Column).array().readonly(),
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
  readonly expandedDirectoryIds: readonly backendModule.DirectoryId[]
  readonly scrollContainerRef: React.RefObject<HTMLElement>
  readonly visibilities: ReadonlyMap<backendModule.AssetId, Visibility>
  readonly category: Category
  readonly hasPasteData: boolean
  readonly setPasteData: (pasteData: pasteDataModule.PasteData<Set<backendModule.AssetId>>) => void
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly setSortInfo: (sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
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
    override?: boolean,
  ) => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
  ) => void
  readonly doDelete: (item: backendModule.AnyAsset, forever: boolean) => Promise<void>
  readonly doRestore: (item: backendModule.AnyAsset) => Promise<void>
  readonly doMove: (
    newParentKey: backendModule.DirectoryId,
    item: backendModule.AnyAsset,
  ) => Promise<void>
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
  readonly category: Category
  readonly initialProjectName: string | null
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
  const { initialProjectName } = props

  const openedProjects = projectsProvider.useLaunchedProjects()
  const doOpenProject = projectHooks.useOpenProject()
  const setCanDownload = useSetCanDownload()
  const setSuggestions = useSetSuggestions()

  const { user } = authProvider.useFullUserSession()
  const backend = backendProvider.useBackend(category)
  const { data: labels } = useBackendQuery(backend, 'listTags', [])
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const previousCategoryRef = React.useRef(category)
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const setCanCreateAssets = useSetCanCreateAssets()
  const setTargetDirectoryInStore = useSetTargetDirectory()
  const didLoadingProjectManagerFail = backendProvider.useDidLoadingProjectManagerFail()
  const reconnectToProjectManager = backendProvider.useReconnectToProjectManager()
  const [enabledColumns, setEnabledColumns] = React.useState(columnUtils.DEFAULT_ENABLED_COLUMNS)
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()
  const setAssetPanelProps = useSetAssetPanelProps()

  const hiddenColumns = columnUtils
    .getColumnList(user, backend.type, category)
    .filter((column) => !enabledColumns.has(column))
  const [sortInfo, setSortInfo] =
    React.useState<sorting.SortInfo<columnUtils.SortableColumn> | null>(null)
  const driveStore = useDriveStore()
  const setNewestFolderId = useSetNewestFolderId()
  const setSelectedKeys = useSetSelectedKeys()
  const setVisuallySelectedKeys = useSetVisuallySelectedKeys()
  const updateAssetRef = React.useRef<
    Record<backendModule.AnyAsset['id'], (asset: backendModule.AnyAsset) => void>
  >({})
  const [pasteData, setPasteData] = React.useState<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)

  const { data: users } = useBackendQuery(backend, 'listUsers', [])
  const { data: userGroups } = useBackendQuery(backend, 'listUserGroups', [])
  const organizationQuery = useSuspenseQuery({
    queryKey: [backend.type, 'getOrganization'],
    queryFn: () => backend.getOrganization(),
  })

  const organization = organizationQuery.data

  const nameOfProjectToImmediatelyOpenRef = React.useRef(initialProjectName)
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

  const rootParentDirectoryId = backendModule.DirectoryId('')
  const rootDirectory = React.useMemo(
    () => backendModule.createRootDirectoryAsset(rootDirectoryId),
    [rootDirectoryId],
  )

  const enableAssetsTableBackgroundRefresh = useFeatureFlag('enableAssetsTableBackgroundRefresh')
  const assetsTableBackgroundRefreshInterval = useFeatureFlag(
    'assetsTableBackgroundRefreshInterval',
  )
  /**
   * The expanded directories in the asset tree.
   * We don't include the root directory as it might change when a user switches
   * between items in sidebar and we don't want to reset the expanded state using useEffect.
   */
  const [privateExpandedDirectoryIds, setExpandedDirectoryIds] = React.useState<
    backendModule.DirectoryId[]
  >(() => [])

  const expandedDirectoryIds = React.useMemo(
    () => privateExpandedDirectoryIds.concat(rootDirectoryId),
    [privateExpandedDirectoryIds, rootDirectoryId],
  )

  const expandedDirectoryIdsSet = React.useMemo(
    () => new Set(expandedDirectoryIds),
    [expandedDirectoryIds],
  )

  const createProjectMutation = useMutation(backendMutationOptions(backend, 'createProject'))
  const duplicateProjectMutation = useMutation(backendMutationOptions(backend, 'duplicateProject'))
  const createDirectoryMutation = useMutation(backendMutationOptions(backend, 'createDirectory'))
  const createSecretMutation = useMutation(backendMutationOptions(backend, 'createSecret'))
  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))
  const createDatalinkMutation = useMutation(backendMutationOptions(backend, 'createDatalink'))
  const uploadFileMutation = useMutation(backendMutationOptions(backend, 'uploadFile'))
  const getProjectDetailsMutation = useMutation(
    backendMutationOptions(backend, 'getProjectDetails'),
  )
  const copyAssetMutation = useMutation(backendMutationOptions(backend, 'copyAsset'))
  const deleteAssetMutation = useMutation(backendMutationOptions(backend, 'deleteAsset'))
  const undoDeleteAssetMutation = useMutation(backendMutationOptions(backend, 'undoDeleteAsset'))
  const updateAssetMutation = useMutation(backendMutationOptions(backend, 'updateAsset'))
  const closeProjectMutation = useMutation(backendMutationOptions(backend, 'closeProject'))

  const directories = useQueries({
    // We query only expanded directories, as we don't want to load the data for directories that are not visible.
    queries: React.useMemo(
      () =>
        expandedDirectoryIds.map((directoryId) =>
          queryOptions({
            queryKey: [
              backend.type,
              'listDirectory',
              directoryId,
              {
                parentId: directoryId,
                labels: null,
                filterBy: CATEGORY_TO_FILTER_BY[category.type],
                recentProjects: category.type === 'recent',
              },
            ] as const,
            queryFn: async ({ queryKey: [, , parentId, params] }) => ({
              parentId,
              children: await backend.listDirectory(params, parentId),
            }),

            refetchInterval:
              enableAssetsTableBackgroundRefresh ? assetsTableBackgroundRefreshInterval : false,
            refetchOnMount: 'always',
            refetchIntervalInBackground: false,
            refetchOnWindowFocus: true,

            enabled: !hidden,
            meta: { persist: false },
          }),
        ),
      [
        hidden,
        backend,
        category,
        expandedDirectoryIds,
        assetsTableBackgroundRefreshInterval,
        enableAssetsTableBackgroundRefresh,
      ],
    ),
    combine: (results) => {
      const rootQuery = results.find((directory) => directory.data?.parentId === rootDirectory.id)

      return {
        rootDirectory: {
          isFetching: rootQuery?.isFetching ?? true,
          isLoading: rootQuery?.isLoading ?? true,
          data: rootQuery?.data,
        },
        directories: results.map((res) => ({
          isFetching: res.isFetching,
          isLoading: res.isLoading,
          data: res.data,
        })),
      }
    },
  })

  /**
   * Return type of the query function for the listDirectory query.
   */
  type ListDirectoryQueryDataType = (typeof directories)['rootDirectory']['data']

  const rootDirectoryContent = directories.rootDirectory.data?.children
  const isLoading = directories.rootDirectory.isLoading

  const assetTree = React.useMemo(() => {
    const rootPath = 'rootPath' in category ? category.rootPath : backend.rootPath(user)

    // If the root directory is not loaded, then we cannot render the tree.
    // Return null, and wait for the root directory to load.
    if (rootDirectoryContent == null) {
      // eslint-disable-next-line no-restricted-syntax
      return AssetTreeNode.fromAsset(
        backendModule.createRootDirectoryAsset(rootDirectoryId),
        rootParentDirectoryId,
        rootParentDirectoryId,
        -1,
        rootPath,
        null,
      )
    }

    const rootId = rootDirectory.id

    const children = rootDirectoryContent.map((content) => {
      /**
       * Recursively build assets tree. If a child is a directory, we search for it is content in the loaded
       * data. If it is loaded, we append that data to the asset node and do the same for the children
       */
      const appendChildrenRecursively = (node: assetTreeNode.AnyAssetTreeNode, depth: number) => {
        const { item } = node

        if (backendModule.assetIsDirectory(item)) {
          const childrenAssetsQuery = directories.directories.find(
            (directory) => directory.data?.parentId === item.id,
          )

          const nestedChildren = childrenAssetsQuery?.data?.children.map((child) =>
            AssetTreeNode.fromAsset(
              child,
              item.id,
              item.id,
              depth,
              `${node.path}/${child.title}`,
              null,
              child.id,
            ),
          )

          if (childrenAssetsQuery == null || childrenAssetsQuery.isLoading) {
            node = node.with({
              children: [
                AssetTreeNode.fromAsset(
                  backendModule.createSpecialLoadingAsset(item.id),
                  item.id,
                  item.id,
                  depth,
                  '',
                ),
              ],
            })
          } else if (nestedChildren?.length === 0) {
            node = node.with({
              children: [
                AssetTreeNode.fromAsset(
                  backendModule.createSpecialEmptyAsset(item.id),
                  item.id,
                  item.id,
                  depth,
                  '',
                ),
              ],
            })
          } else if (nestedChildren != null) {
            node = node.with({
              children: nestedChildren.map((child) => appendChildrenRecursively(child, depth + 1)),
            })
          }
        }

        return node
      }

      const node = AssetTreeNode.fromAsset(
        content,
        rootId,
        rootId,
        0,
        `${rootPath}/${content.title}`,
        null,
        content.id,
      )

      return appendChildrenRecursively(node, 1)
    })

    return new AssetTreeNode(
      rootDirectory,
      rootParentDirectoryId,
      rootParentDirectoryId,
      children,
      -1,
      rootPath,
      null,
      rootId,
    )
  }, [
    category,
    backend,
    user,
    rootDirectoryContent,
    rootDirectory,
    rootParentDirectoryId,
    rootDirectoryId,
    directories.directories,
  ])

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

  const displayItems = React.useMemo(() => {
    if (sortInfo == null) {
      return assetTree.preorderTraversal((children) =>
        children.filter((child) => expandedDirectoryIdsSet.has(child.directoryId)),
      )
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
      return assetTree.preorderTraversal((tree) =>
        [...tree].filter((child) => expandedDirectoryIdsSet.has(child.directoryId)).sort(compare),
      )
    }
  }, [assetTree, sortInfo, expandedDirectoryIdsSet])

  const visibleItems = React.useMemo(
    () => displayItems.filter((item) => visibilities.get(item.key) !== Visibility.hidden),
    [displayItems, visibilities],
  )

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

  const queryClient = useQueryClient()

  const isMainDropzoneVisible = intersectionHooks.useIntersectionRatio(
    rootRef,
    mainDropzoneRef,
    MINIMUM_DROPZONE_INTERSECTION_RATIO,
    (ratio) => ratio >= MINIMUM_DROPZONE_INTERSECTION_RATIO,
    true,
  )

  React.useEffect(() => {
    previousCategoryRef.current = category
  })

  const setTargetDirectory = useEventCallback(
    (targetDirectory: AssetTreeNode<backendModule.DirectoryAsset> | null) => {
      const targetDirectorySelfPermission =
        targetDirectory == null ? null : (
          permissions.tryFindSelfPermission(user, targetDirectory.item.permissions)
        )
      const canCreateAssets =
        targetDirectory == null ?
          category.type !== 'cloud' || user.plan == null || user.plan === backendModule.Plan.solo
        : isLocalCategory(category) ||
          (targetDirectorySelfPermission != null &&
            permissions.canPermissionModifyDirectoryContents(
              targetDirectorySelfPermission.permission,
            ))
      setCanCreateAssets(canCreateAssets)
      setTargetDirectoryInStore(targetDirectory)
    },
  )

  React.useEffect(() => {
    setNewestFolderId(null)
  }, [category, setNewestFolderId])

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

  const doToggleDirectoryExpansion = useEventCallback(
    (
      directoryId: backendModule.DirectoryId,
      _key: backendModule.DirectoryId,
      override?: boolean,
    ) => {
      const isExpanded = expandedDirectoryIdsSet.has(directoryId)
      const shouldExpand = override ?? !isExpanded

      if (shouldExpand !== isExpanded) {
        React.startTransition(() => {
          if (shouldExpand) {
            setExpandedDirectoryIds((currentExpandedDirectoryIds) => [
              ...currentExpandedDirectoryIds,
              directoryId,
            ])
          } else {
            setExpandedDirectoryIds((currentExpandedDirectoryIds) =>
              currentExpandedDirectoryIds.filter((id) => id !== directoryId),
            )
          }
        })
      }
    },
  )

  const doCopyOnBackend = useEventCallback(
    async (newParentId: backendModule.DirectoryId | null, asset: backendModule.AnyAsset) => {
      try {
        newParentId ??= rootDirectoryId

        await copyAssetMutation.mutateAsync([
          asset.id,
          newParentId,
          asset.title,
          nodeMapRef.current.get(newParentId)?.item.title ?? '(unknown)',
        ])
      } catch (error) {
        toastAndLog('copyAssetError', error, asset.title)
      }
    },
  )

  const doMove = useEventCallback(
    async (newParentId: backendModule.DirectoryId | null, asset: backendModule.AnyAsset) => {
      try {
        await updateAssetMutation.mutateAsync([
          asset.id,
          { parentDirectoryId: newParentId ?? rootDirectoryId, description: null },
          asset.title,
        ])
      } catch (error) {
        toastAndLog('moveAssetError', error, asset.title)
      }
    },
  )

  const doDelete = useEventCallback(
    async (asset: backendModule.AnyAsset, forever: boolean = false) => {
      if (asset.type === backendModule.AssetType.directory) {
        dispatchAssetListEvent({
          type: AssetListEventType.closeFolder,
          id: asset.id,
          // This is SAFE, as this asset is already known to be a directory.
          // eslint-disable-next-line no-restricted-syntax
          key: asset.id,
        })
      }
      try {
        dispatchAssetListEvent({ type: AssetListEventType.willDelete, key: asset.id })
        if (
          asset.type === backendModule.AssetType.project &&
          backend.type === backendModule.BackendType.local
        ) {
          try {
            await closeProjectMutation.mutateAsync([asset.id, asset.title])
          } catch {
            // Ignored. The project was already closed.
          }
        }
        await deleteAssetMutation.mutateAsync([asset.id, { force: forever }, asset.title])
      } catch (error) {
        toastAndLog('deleteAssetError', error, asset.title)
      }
    },
  )

  const doDeleteById = useEventCallback(
    async (assetId: backendModule.AssetId, forever: boolean = false) => {
      const asset = nodeMapRef.current.get(assetId)?.item

      if (asset != null) {
        // eslint-disable-next-line no-restricted-syntax
        return doDelete(asset, forever)
      }
    },
  )

  const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
  const [keyboardSelectedIndex, setKeyboardSelectedIndex] = React.useState<number | null>(null)
  const mostRecentlySelectedIndexRef = React.useRef<number | null>(null)
  const selectionStartIndexRef = React.useRef<number | null>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)

  const setMostRecentlySelectedIndex = useEventCallback(
    (index: number | null, isKeyboard: boolean = false) => {
      React.startTransition(() => {
        mostRecentlySelectedIndexRef.current = index
        setKeyboardSelectedIndex(isKeyboard ? index : null)
      })
    },
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
                        await updateSecretMutation.mutateAsync([id, { value }, item.item.title])
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
              doToggleDirectoryExpansion(item.item.id, item.key, false)
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
            doToggleDirectoryExpansion(item.item.id, item.key, true)
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
        setSelectedKeys(set.EMPTY_SET)
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
          setSelectedKeys(set.EMPTY_SET)
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

  const getNewProjectName = useEventCallback(
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
  )

  const deleteAsset = useEventCallback((assetId: backendModule.AssetId) => {
    const asset = nodeMapRef.current.get(assetId)?.item

    if (asset) {
      const listDirectoryQuery = queryClient.getQueryCache().find<ListDirectoryQueryDataType>({
        queryKey: ['listDirectory', backend.type, asset.parentId],
        exact: false,
      })

      if (listDirectoryQuery?.state.data) {
        listDirectoryQuery.setData({
          ...listDirectoryQuery.state.data,
          children: listDirectoryQuery.state.data.children.filter((child) => child.id !== assetId),
        })
      }
    }
  })

  /** All items must have the same type. */
  const insertAssets = useEventCallback(
    (assets: readonly backendModule.AnyAsset[], parentId: backendModule.DirectoryId | null) => {
      const actualParentId = parentId ?? rootDirectoryId

      const listDirectoryQuery = queryClient.getQueryCache().find<ListDirectoryQueryDataType>({
        queryKey: ['listDirectory', backend.type, actualParentId],
        exact: false,
      })

      if (listDirectoryQuery?.state.data) {
        listDirectoryQuery.setData({
          ...listDirectoryQuery.state.data,
          children: [...listDirectoryQuery.state.data.children, ...assets],
        })
      }
    },
  )

  // This is not a React component, even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const onAssetListEvent = useEventCallback((event: assetListEvent.AssetListEvent) => {
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

        console.log('placeholderItem', { placeholderItem, parent, siblings })

        doToggleDirectoryExpansion(event.parentId, event.parentKey, true)
        insertAssets([placeholderItem], event.parentId)

        void createDirectoryMutation
          .mutateAsync([{ parentId: placeholderItem.parentId, title: placeholderItem.title }])
          .then(({ id }) => {
            setNewestFolderId(id)
            setSelectedKeys(new Set([id]))
          })

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
        doToggleDirectoryExpansion(event.parentId, event.parentKey, true)

        insertAssets([placeholderItem], event.parentId)

        void createProjectMutation
          .mutateAsync([
            {
              parentDirectoryId: placeholderItem.parentId,
              projectName: placeholderItem.title,
              ...(event.templateId == null ? {} : { projectTemplateName: event.templateId }),
              ...(event.datalinkId == null ? {} : { datalinkId: event.datalinkId }),
            },
          ])
          .catch((error) => {
            event.onError?.()

            deleteAsset(placeholderItem.id)
            toastAndLog('createProjectError', error)

            throw error
          })
          .then((createdProject) => {
            event.onCreated?.(createdProject)
            doOpenProject({
              id: createdProject.projectId,
              type: backend.type,
              parentId: placeholderItem.parentId,
              title: placeholderItem.title,
            })
          })

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
        const uploadedFileIds: backendModule.AssetId[] = []
        const addIdToSelection = (id: backendModule.AssetId) => {
          uploadedFileIds.push(id)
          const newIds = new Set(uploadedFileIds)
          setSelectedKeys(newIds)
        }

        const doUploadFile = async (asset: backendModule.AnyAsset, method: 'new' | 'update') => {
          const file = fileMap.get(asset.id)

          if (file != null) {
            const fileId = method === 'new' ? null : asset.id

            switch (true) {
              case backendModule.assetIsProject(asset): {
                const { extension } = backendModule.extractProjectExtension(file.name)
                const title = backendModule.stripProjectExtension(asset.title)

                const assetNode = nodeMapRef.current.get(asset.id)

                if (backend.type === backendModule.BackendType.local && localBackend != null) {
                  const directory = localBackendModule.extractTypeAndId(
                    assetNode?.directoryId ?? asset.parentId,
                  ).id
                  let id: string
                  if (
                    'backendApi' in window &&
                    // This non-standard property is defined in Electron.
                    'path' in file
                  ) {
                    const projectInfo = await window.backendApi.importProjectFromPath(
                      file.path,
                      directory,
                      title,
                    )
                    id = projectInfo.id
                  } else {
                    const searchParams = new URLSearchParams({ directory, name: title }).toString()
                    // Ideally this would use `file.stream()`, to minimize RAM
                    // requirements. for uploading large projects. Unfortunately,
                    // this requires HTTP/2, which is HTTPS-only, so it will not
                    // work on `http://localhost`.
                    const body =
                      window.location.protocol === 'https:' ?
                        file.stream()
                      : await file.arrayBuffer()
                    const path = `./api/upload-project?${searchParams}`
                    const response = await fetch(path, { method: 'POST', body })
                    id = await response.text()
                  }
                  const projectId = localBackendModule.newProjectId(projectManager.UUID(id))
                  addIdToSelection(projectId)

                  await getProjectDetailsMutation
                    .mutateAsync([projectId, asset.parentId, file.name])
                    .catch((error) => {
                      deleteAsset(projectId)
                      toastAndLog('uploadProjectError', error)
                    })
                } else {
                  uploadFileMutation
                    .mutateAsync([
                      {
                        fileId,
                        fileName: `${title}.${extension}`,
                        parentDirectoryId: asset.parentId,
                      },
                      file,
                    ])
                    .then(({ id }) => {
                      addIdToSelection(id)
                    })
                    .catch((error) => {
                      deleteAsset(asset.id)
                      toastAndLog('uploadProjectError', error)
                    })
                }

                break
              }
              case backendModule.assetIsFile(asset): {
                void uploadFileMutation
                  .mutateAsync([
                    { fileId, fileName: asset.title, parentDirectoryId: asset.parentId },
                    file,
                  ])
                  .then(({ id }) => {
                    addIdToSelection(id)
                  })

                break
              }
              default:
                break
            }
          }
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

          const assets = [...placeholderFiles, ...placeholderProjects]

          doToggleDirectoryExpansion(event.parentId, event.parentKey, true)

          insertAssets(assets, event.parentId)

          void Promise.all(assets.map((asset) => doUploadFile(asset, 'new')))
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
              doUpdateConflicting={(resolvedConflicts) => {
                for (const conflict of resolvedConflicts) {
                  const isUpdating = conflict.current.title === conflict.new.title

                  const asset = isUpdating ? conflict.current : conflict.new

                  fileMap.set(
                    asset.id,
                    new File([conflict.file], asset.title, {
                      type: conflict.file.type,
                      lastModified: conflict.file.lastModified,
                    }),
                  )

                  insertAssets([asset], event.parentId)
                  void doUploadFile(asset, isUpdating ? 'update' : 'new')
                }
              }}
              doUploadNonConflicting={() => {
                doToggleDirectoryExpansion(event.parentId, event.parentKey, true)

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

                const assets = [...newFiles, ...newProjects]

                insertAssets(assets, event.parentId)

                for (const asset of assets) {
                  void doUploadFile(asset, 'new')
                }
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
        doToggleDirectoryExpansion(event.parentId, event.parentKey, true)
        insertAssets([placeholderItem], event.parentId)

        createDatalinkMutation.mutate([
          {
            parentDirectoryId: placeholderItem.parentId,
            datalinkId: null,
            name: placeholderItem.title,
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

        doToggleDirectoryExpansion(event.parentId, event.parentKey, true)
        insertAssets([placeholderItem], event.parentId)

        createSecretMutation.mutate([
          {
            parentDirectoryId: placeholderItem.parentId,
            name: placeholderItem.title,
            value: event.value,
          },
        ])

        break
      }
      case AssetListEventType.insertAssets: {
        insertAssets(event.assets, event.parentId)
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

        insertAssets([placeholderItem], event.parentId)

        void duplicateProjectMutation
          .mutateAsync([event.original.id, event.versionId, placeholderItem.title])
          .catch((error) => {
            deleteAsset(placeholderItem.id)
            toastAndLog('createProjectError', error)

            throw error
          })
          .then((project) => {
            doOpenProject({
              type: backend.type,
              parentId: event.parentId,
              title: placeholderItem.title,
              id: project.projectId,
            })
          })

        break
      }
      case AssetListEventType.willDelete: {
        const { selectedKeys } = driveStore.getState()
        if (selectedKeys.has(event.key)) {
          const newSelectedKeys = new Set(selectedKeys)
          newSelectedKeys.delete(event.key)
          setSelectedKeys(newSelectedKeys)
        }

        deleteAsset(event.key)

        break
      }
      case AssetListEventType.copy: {
        insertAssets(event.items, event.newParentId)

        for (const item of event.items) {
          void doCopyOnBackend(event.newParentId, item)
        }
        break
      }
      case AssetListEventType.move: {
        deleteAsset(event.key)
        insertAssets(event.items, event.newParentId)

        for (const item of event.items) {
          void doMove(event.newParentId, item)
        }

        break
      }
      case AssetListEventType.delete: {
        deleteAsset(event.key)
        const asset = nodeMapRef.current.get(event.key)?.item
        if (asset) {
          void doDelete(asset, false)
        }
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
        doToggleDirectoryExpansion(event.id, event.key, false)
        break
      }
    }
  })
  eventListProvider.useAssetListEventListener((event) => {
    if (!isLoading) {
      onAssetListEvent(event)
    } else {
      queuedAssetListEventsRef.current.push(event)
    }
  })

  const doCopy = useEventCallback(() => {
    unsetModal()
    const { selectedKeys } = driveStore.getState()
    setPasteData({ type: PasteType.copy, data: selectedKeys })
  })

  const doCut = useEventCallback(() => {
    unsetModal()
    if (pasteData != null) {
      dispatchAssetEvent({ type: AssetEventType.cancelCut, ids: pasteData.data })
    }
    const { selectedKeys } = driveStore.getState()
    setPasteData({ type: PasteType.move, data: selectedKeys })
    dispatchAssetEvent({ type: AssetEventType.cut, ids: selectedKeys })
    setSelectedKeys(set.EMPTY_SET)
  })

  const doPaste = useEventCallback(
    (newParentKey: backendModule.DirectoryId, newParentId: backendModule.DirectoryId) => {
      unsetModal()
      if (pasteData != null) {
        if (pasteData.data.has(newParentKey)) {
          toast.toast.error('Cannot paste a folder into itself.')
        } else {
          doToggleDirectoryExpansion(newParentId, newParentKey, true)
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
  )

  const doRestore = useEventCallback(async (asset: backendModule.AnyAsset) => {
    try {
      await undoDeleteAssetMutation.mutateAsync([asset.id, asset.title])
    } catch (error) {
      toastAndLog('restoreAssetError', error, asset.title)
    }
  })

  const hideColumn = useEventCallback((column: columnUtils.Column) => {
    setEnabledColumns((columns) => set.withPresence(columns, column, false))
  })

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
        doDelete={doDeleteById}
      />
    ),
    [backend, category, pasteData, rootDirectoryId, doCopy, doCut, doPaste, doDeleteById],
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
    setIsDraggingFiles(false)
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
      expandedDirectoryIds,
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
      nodeMap: nodeMapRef,
      pasteData: pasteDataRef,
      hideColumn,
      doToggleDirectoryExpansion,
      doCopy,
      doCut,
      doPaste,
      doDelete,
      doRestore,
      doMove,
    }),
    [
      backend,
      expandedDirectoryIds,
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
      doDelete,
      doRestore,
      doMove,
      hideColumn,
      setQuery,
    ],
  )

  React.useEffect(() => {
    // In some browsers, at least in Chrome 126,
    // in some situations, when an element has a
    // 'container-size' style, and the parent element is hidden,
    // the browser can't calculate the element's size
    // and thus the element doesn't appear when we unhide the parent.
    // The only way to fix that is to force browser to recalculate styles
    // So the trick is to change a property, trigger style recalc(`getBoundlingClientRect()`)
    // and remove the property.
    // since everything is happening synchronously, user won't see a broken layout during recalculation
    if (!hidden && rootRef.current) {
      for (let i = 0; i < rootRef.current.children.length; i++) {
        const element = rootRef.current.children[i]

        if (element instanceof HTMLElement) {
          element.style.width = '0px'
          element.getBoundingClientRect()
          element.style.width = ''
        }
      }
    }
  }, [hidden])

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
      const shrinkBy =
        COLUMNS_SELECTOR_BASE_WIDTH_PX + COLUMNS_SELECTOR_ICON_WIDTH_PX * hiddenColumns.length
      const rightOffset = rootRef.current.clientWidth + rootRef.current.scrollLeft - shrinkBy
      headerRowRef.current.style.clipPath = `polygon(0 0, ${rightOffset}px 0, ${rightOffset}px 100%, 0 100%)`
    }
  }, [backend.type, hiddenColumns.length])

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
          [inputBindingsModule.DEFAULT_HANDLER]: (event) => {
            /** When the document is clicked, deselect the keys, but only if the clicked element
             * is not inside a `Dialog`. To detect whether an element is a `Dialog`,
             * we check whether it is inside the `portal-root` where all the `Dialog`s are mounted.
             * If this check is omitted, when the user clicks inside a Datalink dialog,
             * the keys are deselected, causing the Datalink to be added to the root directory,
             * rather than the one that was selected when the dialog was opened.
             */
            const portalRoot =
              event.target instanceof HTMLElement || event.target instanceof SVGElement ?
                event.target.closest('.enso-portal-root')
              : null
            if (!portalRoot && driveStore.getState().selectedKeys.size !== 0) {
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
          onAssetListEvent(event)
        }
      }
      setSpinnerState(spinner.SpinnerState.initial)
    }
  }, [isLoading, onAssetListEvent])

  const calculateNewKeys = useEventCallback(
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
  )

  const { startAutoScroll, endAutoScroll, onMouseEvent } = autoScrollHooks.useAutoScroll(rootRef)

  const dragSelectionChangeLoopHandle = React.useRef(0)
  const dragSelectionRangeRef = React.useRef<DragSelectionInfo | null>(null)
  const onSelectionDrag = useEventCallback(
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
  )

  const onSelectionDragEnd = useEventCallback((event: MouseEvent) => {
    endAutoScroll()
    onMouseEvent(event)
    const range = dragSelectionRangeRef.current
    if (range != null) {
      const keys = displayItems.slice(range.start, range.end).map((node) => node.key)
      setSelectedKeys(calculateNewKeys(event, keys, () => []))
    }
    setVisuallySelectedKeys(null)
    dragSelectionRangeRef.current = null
  })

  const onSelectionDragCancel = useEventCallback(() => {
    setVisuallySelectedKeys(null)
    dragSelectionRangeRef.current = null
  })

  const grabRowKeyboardFocus = useEventCallback((item: assetTreeNode.AnyAssetTreeNode) => {
    setSelectedKeys(new Set([item.key]))
  })

  const onRowClick = useEventCallback(
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
  )

  const selectRow = useEventCallback((item: assetTreeNode.AnyAssetTreeNode) => {
    setMostRecentlySelectedIndex(visibleItems.indexOf(item))
    selectionStartIndexRef.current = null
    setSelectedKeys(new Set([item.key]))
  })

  const onRowDragStart = useEventCallback(
    (event: React.DragEvent<HTMLTableRowElement>, item: assetTreeNode.AnyAssetTreeNode) => {
      startAutoScroll()
      onMouseEvent(event)
      let newSelectedKeys = driveStore.getState().selectedKeys
      if (!newSelectedKeys.has(item.key)) {
        setMostRecentlySelectedIndex(visibleItems.indexOf(item))
        selectionStartIndexRef.current = null
        newSelectedKeys = new Set([item.key])
        setSelectedKeys(newSelectedKeys)
      }
      const nodes = assetTree.preorderTraversal().filter((node) => newSelectedKeys.has(node.key))
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
    },
  )

  const onRowDragOver = useEventCallback(
    (event: React.DragEvent<HTMLTableRowElement>, item: assetTreeNode.AnyAssetTreeNode) => {
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
          const ids = typeof idsReference === 'string' ? new Set([idsReference]) : idsReference
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
    },
  )

  const onRowDragEnd = useEventCallback(() => {
    setIsDraggingFiles(false)
    endAutoScroll()
    lastSelectedIdsRef.current = null
    const { selectedKeys } = driveStore.getState()
    dispatchAssetEvent({
      type: AssetEventType.temporarilyAddLabels,
      ids: selectedKeys,
      labelNames: set.EMPTY_SET,
    })
  })

  const onRowDrop = useEventCallback(
    (event: React.DragEvent<HTMLTableRowElement>, item: assetTreeNode.AnyAssetTreeNode) => {
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
    },
  )

  const getAsset = useEventCallback(
    (key: backendModule.AssetId) => nodeMapRef.current.get(key)?.item ?? null,
  )

  const setAsset = useEventCallback(
    (assetId: backendModule.AssetId, asset: backendModule.AnyAsset) => {
      const listDirectoryQuery = queryClient.getQueryCache().find<ListDirectoryQueryDataType>({
        queryKey: ['listDirectory', backend.type, asset.parentId],
        exact: false,
      })

      if (listDirectoryQuery?.state.data) {
        listDirectoryQuery.setData({
          ...listDirectoryQuery.state.data,
          children: listDirectoryQuery.state.data.children.map((child) =>
            child.id === assetId ? asset : child,
          ),
        })
      }
    },
  )

  React.useImperativeHandle(assetManagementApiRef, () => ({
    getAsset,
    setAsset,
  }))

  const columns = React.useMemo(
    () =>
      columnUtils
        .getColumnList(user, backend.type, category)
        .filter((column) => enabledColumns.has(column)),
    [backend.type, category, enabledColumns, user],
  )

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
    : displayItems.map((item) => {
        return (
          <AssetRow
            key={item.key + item.path}
            updateAssetRef={updateAssetRef}
            isOpened={openedProjects.some(({ id }) => item.item.id === id)}
            columns={columns}
            item={item}
            state={state}
            hidden={hidden || visibilities.get(item.key) === Visibility.hidden}
            isKeyboardSelected={
              keyboardSelectedIndex != null && item === visibleItems[keyboardSelectedIndex]
            }
            grabKeyboardFocus={grabRowKeyboardFocus}
            onClick={onRowClick}
            select={selectRow}
            onDragStart={onRowDragStart}
            onDragOver={onRowDragOver}
            onDragEnd={onRowDragEnd}
            onDrop={onRowDrop}
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
              doDelete={doDeleteById}
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
        onDragLeave={() => {
          lastSelectedIdsRef.current = null
        }}
        onDragEnd={() => {
          setIsDraggingFiles(false)
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
          setSelectedKeys(set.EMPTY_SET)
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
                  }
                },
                onDragEnd: () => {
                  setIsDraggingFiles(false)
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
                          {hiddenColumns.map((column) => (
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
                <div className="flex h-full w-min min-w-full grow flex-col">{table}</div>
              </div>
            </div>
          )}
        </FocusArea>
        {isDraggingFiles && !isMainDropzoneVisible && (
          <div className="pointer-events-none absolute bottom-4 left-1/2 -translate-x-1/2">
            <div
              className="pointer-events-auto flex items-center justify-center gap-3 rounded-default bg-selected-frame px-8 py-6 text-primary/50 backdrop-blur-3xl transition-all"
              onDragEnter={onDropzoneDragOver}
              onDragOver={onDropzoneDragOver}
              onDragEnd={() => {
                setIsDraggingFiles(false)
              }}
              onDrop={(event) => {
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
