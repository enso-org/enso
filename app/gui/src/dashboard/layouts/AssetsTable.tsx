/** @file Table displaying a list of projects. */
import {
  Children,
  cloneElement,
  isValidElement,
  memo,
  startTransition,
  useEffect,
  useImperativeHandle,
  useMemo,
  useRef,
  useState,
  type Dispatch,
  type DragEvent,
  type KeyboardEvent,
  type MutableRefObject,
  type MouseEvent as ReactMouseEvent,
  type ReactNode,
  type Ref,
  type RefObject,
  type SetStateAction,
} from 'react'

import { useMutation, useQueryClient } from '@tanstack/react-query'
import { toast } from 'react-toastify'
import * as z from 'zod'

import DropFilesImage from '#/assets/drop_files.svg'
import { FileTrigger, mergeProps, usePress } from '#/components/aria'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetRowInnerProps } from '#/components/dashboard/AssetRow'
import { AssetRow } from '#/components/dashboard/AssetRow'
import { INITIAL_ROW_STATE } from '#/components/dashboard/AssetRow/assetRowUtils'
import type { SortableColumn } from '#/components/dashboard/column/columnUtils'
import {
  Column,
  COLUMN_CSS_CLASS,
  COLUMN_ICONS,
  COLUMN_SHOW_TEXT_ID,
  DEFAULT_ENABLED_COLUMNS,
  getColumnList,
} from '#/components/dashboard/column/columnUtils'
import NameColumn from '#/components/dashboard/column/NameColumn'
import { COLUMN_HEADING } from '#/components/dashboard/columnHeading'
import Label from '#/components/dashboard/Label'
import { ErrorDisplay } from '#/components/ErrorBoundary'
import { IsolateLayout } from '#/components/IsolateLayout'
import { SelectionBrush, type OnDragParams } from '#/components/SelectionBrush'
import { IndefiniteSpinner } from '#/components/Spinner'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { useAutoScroll } from '#/hooks/autoScrollHooks'
import {
  addAssetsLabelsMutationOptions,
  copyAssetsMutationOptions,
  moveAssetsMutationOptions,
  removeAssetsLabelsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { useCutAndPaste } from '#/hooks/cutAndPasteHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useIntersectionRatio } from '#/hooks/intersectionHooks'
import { useOpenProject } from '#/hooks/projectHooks'
import { useStore } from '#/hooks/storeHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import {
  assetPanelStore,
  useResetAssetPanelProps,
  useSetAssetPanelProps,
  useSetIsAssetPanelTemporarilyVisible,
} from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import { useSetSuggestions } from '#/layouts/AssetSearchBar'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import {
  canTransferBetweenCategories,
  isLocalCategory,
  type Category,
} from '#/layouts/CategorySwitcher/Category'
import { useAssetsTableItems } from '#/layouts/Drive/assetsTableItemsHooks'
import { useAssetTree, type DirectoryQuery } from '#/layouts/Drive/assetTreeHooks'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import DragModal from '#/modals/DragModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import {
  useBackend,
  useDidLoadingProjectManagerFail,
  useReconnectToProjectManager,
} from '#/providers/BackendProvider'
import {
  useDriveStore,
  useNodeMap,
  useSetCanCreateAssets,
  useSetCanDownload,
  useSetLabelsDragPayload,
  useSetNewestFolderId,
  useSetNodeMap,
  useSetPasteData,
  useSetSelectedAssets,
  useSetTargetDirectory,
  useSetVisuallySelectedKeys,
  useToggleDirectoryExpansion,
  type SelectedAssetInfo,
} from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useNavigator2D } from '#/providers/Navigator2DProvider'
import { useLaunchedProjects } from '#/providers/ProjectsProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import type { AssetId } from '#/services/Backend'
import {
  assetIsProject,
  AssetType,
  BackendType,
  getAssetPermissionName,
  IS_OPENING_OR_OPENED,
  Plan,
  type AnyAsset,
  type DirectoryAsset,
  type DirectoryId,
} from '#/services/Backend'
import type { AssetQueryKey } from '#/utilities/AssetQuery'
import AssetQuery from '#/utilities/AssetQuery'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import type { AssetRowsDragPayload } from '#/utilities/drag'
import { ASSET_ROWS, LABELS, setDragImageToBlank } from '#/utilities/drag'
import { fileExtension } from '#/utilities/fileInfo'
import { noop } from '#/utilities/functions'
import { DEFAULT_HANDLER } from '#/utilities/inputBindings'
import LocalStorage from '#/utilities/LocalStorage'
import {
  canPermissionModifyDirectoryContents,
  PermissionAction,
  tryFindSelfPermission,
} from '#/utilities/permissions'
import { document } from '#/utilities/sanitizedEventTargets'
import { withPresence } from '#/utilities/set'
import type { SortInfo } from '#/utilities/sorting'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import invariant from 'tiny-invariant'
import {
  SUGGESTIONS_FOR_HAS,
  SUGGESTIONS_FOR_NEGATIVE_TYPE,
  SUGGESTIONS_FOR_NO,
  SUGGESTIONS_FOR_TYPE,
} from './Drive/suggestionsConstants'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly enabledColumns: readonly Column[]
  }
}

LocalStorage.registerKey('enabledColumns', {
  schema: z.nativeEnum(Column).array().readonly(),
})

/**
 * If the ratio of intersection between the main dropzone that should be visible, and the
 * scrollable container, is below this value, then the backup dropzone will be shown.
 */
const MINIMUM_DROPZONE_INTERSECTION_RATIO = 0.5
/**
 * The height of each row in the table body. MUST be identical to the value as set by the
 * Tailwind styling.
 */
const ROW_HEIGHT_PX = 36
/** The size of the loading spinner. */
const LOADING_SPINNER_SIZE_PX = 36

/** Information related to a drag selection. */
interface DragSelectionInfo {
  readonly initialIndex: number
  readonly start: number
  readonly end: number
}

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  readonly backend: Backend
  readonly rootDirectoryId: DirectoryId
  readonly scrollContainerRef: RefObject<HTMLElement>
  readonly category: Category
  readonly sortInfo: SortInfo<SortableColumn> | null
  readonly setSortInfo: (sortInfo: SortInfo<SortableColumn> | null) => void
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
  readonly nodeMap: Readonly<MutableRefObject<ReadonlyMap<AssetId, AnyAssetTreeNode>>>
  readonly hideColumn: (column: Column) => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (newParentKey: DirectoryId, newParentId: DirectoryId) => void
  readonly getAssetNodeById: (id: AssetId) => AnyAssetTreeNode | null
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
  readonly isEditingName: boolean
}

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
  readonly hidden: boolean
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
  readonly category: Category
  readonly initialProjectName: string | null
  readonly assetManagementApiRef: Ref<AssetManagementApi>
}

/** The API for managing assets in the table. */
export interface AssetManagementApi {
  readonly getAsset: (id: AssetId) => AnyAsset | null
  readonly setAsset: (id: AssetId, asset: AnyAsset) => void
}

/** The table of project assets. */
function AssetsTable(props: AssetsTableProps) {
  const { hidden, query, setQuery, category, assetManagementApiRef } = props
  const { initialProjectName } = props

  const openedProjects = useLaunchedProjects()
  const doOpenProject = useOpenProject()
  const setCanDownload = useSetCanDownload()
  const setSuggestions = useSetSuggestions()

  const queryClient = useQueryClient()
  const { user } = useFullUserSession()
  const backend = useBackend(category)
  const { data: labels } = useBackendQuery(backend, 'listTags', [])
  const { setModal, unsetModal } = useSetModal()
  const { localStorage } = useLocalStorage()
  const { getText } = useText()
  const inputBindings = useInputBindings()
  const navigator2D = useNavigator2D()
  const toastAndLog = useToastAndLog()
  const setCanCreateAssets = useSetCanCreateAssets()
  const setTargetDirectoryInStore = useSetTargetDirectory()
  const didLoadingProjectManagerFail = useDidLoadingProjectManagerFail()
  const reconnectToProjectManager = useReconnectToProjectManager()
  const [enabledColumns, setEnabledColumns] = useState(DEFAULT_ENABLED_COLUMNS)
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()
  const setAssetPanelProps = useSetAssetPanelProps()
  const resetAssetPanelProps = useResetAssetPanelProps()
  const setLabelsDragPayload = useSetLabelsDragPayload()

  const columns = useMemo(
    () =>
      getColumnList(user, backend.type, category).filter((column) => enabledColumns.has(column)),
    [backend.type, category, enabledColumns, user],
  )

  const hiddenColumns = useMemo(
    () =>
      getColumnList(user, backend.type, category).filter((column) => !enabledColumns.has(column)),
    [backend.type, category, enabledColumns, user],
  )

  const [sortInfo, setSortInfo] = useState<SortInfo<SortableColumn> | null>(null)
  const driveStore = useDriveStore()
  const setNewestFolderId = useSetNewestFolderId()
  const setSelectedAssets = useSetSelectedAssets()
  const setVisuallySelectedKeys = useSetVisuallySelectedKeys()
  const setPasteData = useSetPasteData()

  const nameOfProjectToImmediatelyOpenRef = useRef(initialProjectName)

  const toggleDirectoryExpansion = useToggleDirectoryExpansion()

  const uploadFiles = useUploadFiles(backend, category)
  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))
  const cutAndPaste = useCutAndPaste(backend, category)
  const copyAssetsMutation = useMutation(copyAssetsMutationOptions(backend))
  const moveAssetsMutation = useMutation(moveAssetsMutationOptions(backend))
  const addAssetsLabelsMutation = useMutation(addAssetsLabelsMutationOptions(backend))
  const removeAssetsLabelsMutation = useMutation(removeAssetsLabelsMutationOptions(backend))

  const { rootDirectoryId, rootDirectory, expandedDirectoryIds } = useDirectoryIds({ category })
  const { isLoading, isError, assetTree } = useAssetTree({
    hidden,
    category,
    rootDirectory,
    expandedDirectoryIds,
  })
  const { displayItems, visibleItems, visibilities } = useAssetsTableItems({
    assetTree,
    query,
    sortInfo,
    expandedDirectoryIds,
  })

  const [isDraggingFiles, setIsDraggingFiles] = useState(false)
  const [droppedFilesCount, setDroppedFilesCount] = useState(0)
  const isCloud = backend.type === BackendType.remote
  const rootRef = useRef<HTMLDivElement | null>(null)
  const mainDropzoneRef = useRef<HTMLButtonElement | null>(null)
  const lastSelectedIdsRef = useRef<AssetId | ReadonlySet<AssetId> | null>(null)
  const headerRowRef = useRef<HTMLTableRowElement>(null)
  const assetTreeRef = useRef<AnyAssetTreeNode>(assetTree)
  const getPasteData = useEventCallback(() => driveStore.getState().pasteData)
  const nodeMapRef = useNodeMap()
  const setNodeMap = useSetNodeMap()
  const isAssetContextMenuVisible =
    category.type !== 'cloud' || user.plan == null || user.plan === Plan.solo

  const isMainDropzoneVisible = useIntersectionRatio(
    rootRef,
    mainDropzoneRef,
    MINIMUM_DROPZONE_INTERSECTION_RATIO,
    (ratio) => ratio >= MINIMUM_DROPZONE_INTERSECTION_RATIO,
    true,
  )

  const setTargetDirectory = useEventCallback(
    (targetDirectory: AssetTreeNode<DirectoryAsset> | null) => {
      const targetDirectorySelfPermission =
        targetDirectory == null ? null : (
          tryFindSelfPermission(user, targetDirectory.item.permissions)
        )
      const canCreateAssets =
        targetDirectory == null ?
          category.type !== 'cloud' || user.plan == null || user.plan === Plan.solo
        : isLocalCategory(category) ||
          (targetDirectorySelfPermission != null &&
            canPermissionModifyDirectoryContents(targetDirectorySelfPermission.permission))
      setCanCreateAssets(canCreateAssets)
      setTargetDirectoryInStore(targetDirectory)
    },
  )

  useEffect(() => {
    setNewestFolderId(null)
  }, [category, setNewestFolderId])

  useEffect(
    () =>
      driveStore.subscribe(({ selectedKeys }, { selectedKeys: oldSelectedKeys }) => {
        if (selectedKeys !== oldSelectedKeys) {
          if (selectedKeys.size === 0) {
            setTargetDirectory(null)
          } else if (selectedKeys.size === 1) {
            const [soleKey] = selectedKeys
            const item = soleKey == null ? null : nodeMapRef.current.get(soleKey)

            if (item != null && item.isType(AssetType.directory)) {
              setTargetDirectory(item)
            } else {
              setTargetDirectory(null)
            }

            if (
              item != null &&
              item.item.id !== assetPanelStore.getState().assetPanelProps.item?.id
            ) {
              setAssetPanelProps({ backend, item: item.item, path: item.path })
              setIsAssetPanelTemporarilyVisible(false)
            }
          } else {
            let commonDirectoryId: AssetId | null = null
            let otherCandidateDirectoryId: AssetId | null = null
            for (const key of selectedKeys) {
              const node = nodeMapRef.current.get(key)
              if (node != null) {
                if (commonDirectoryId == null) {
                  commonDirectoryId = node.item.parentId
                  otherCandidateDirectoryId =
                    node.item.type === AssetType.directory ? node.item.id : null
                } else if (
                  node.item.id === commonDirectoryId ||
                  node.item.parentId === commonDirectoryId
                ) {
                  otherCandidateDirectoryId = null
                } else if (
                  otherCandidateDirectoryId != null &&
                  (node.item.id === otherCandidateDirectoryId ||
                    node.item.parentId === otherCandidateDirectoryId)
                ) {
                  commonDirectoryId = otherCandidateDirectoryId
                  otherCandidateDirectoryId = null
                } else {
                  // No match; there is no common parent directory for the entire selection.
                  commonDirectoryId = null
                  break
                }
              }
            }
            const node =
              commonDirectoryId == null ? null : nodeMapRef.current.get(commonDirectoryId)
            if (node != null && node.isType(AssetType.directory)) {
              setTargetDirectory(node)
            } else {
              setTargetDirectory(null)
            }
          }
        }
      }),
    [
      backend,
      driveStore,
      nodeMapRef,
      setAssetPanelProps,
      setIsAssetPanelTemporarilyVisible,
      setTargetDirectory,
    ],
  )

  useEffect(() => {
    const nodeToSuggestion = (
      node: AnyAssetTreeNode,
      key: AssetQueryKey = 'names',
    ): assetSearchBar.Suggestion => ({
      key: node.item.id,
      render: () => `${key === 'names' ? '' : '-:'}${node.item.title}`,
      addToQuery: (oldQuery) => oldQuery.addToLastTerm({ [key]: [node.item.title] }),
      deleteFromQuery: (oldQuery) => oldQuery.deleteFromLastTerm({ [key]: [node.item.title] }),
    })
    const allVisibleNodes = () =>
      assetTree
        .preorderTraversal((children) =>
          children.filter((child) => visibilities.get(child.item.id) !== Visibility.hidden),
        )
        .filter(
          (node) =>
            visibilities.get(node.item.id) === Visibility.visible &&
            node.item.type !== AssetType.specialEmpty &&
            node.item.type !== AssetType.specialLoading,
        )

    const allVisible = (negative = false) => {
      return allVisibleNodes().map((node) =>
        nodeToSuggestion(node, negative ? 'negativeNames' : 'names'),
      )
    }

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
            .filter((node) => node.item.type === AssetType.file)
            .map((node) => fileExtension(node.item.title))
          setSuggestions(
            Array.from(
              new Set(extensions),
              (extension): assetSearchBar.Suggestion => ({
                key: extension,
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
                key: modified,
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
                .filter((permission) => permission.permission === PermissionAction.own)
                .map(getAssetPermissionName),
            )
          setSuggestions(
            Array.from(
              new Set(owners),
              (owner): assetSearchBar.Suggestion => ({
                key: owner,
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
                key: label.value,
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

  useEffect(() => {
    assetTreeRef.current = assetTree
    const newNodeMap = new Map(assetTree.preorderTraversal().map((asset) => [asset.item.id, asset]))
    newNodeMap.set(assetTree.item.id, assetTree)
    setNodeMap(newNodeMap)
  }, [assetTree, setNodeMap])

  useEffect(() => {
    if (!hidden) {
      return inputBindings.attach(document.body, 'keydown', {
        cancelCut: () => {
          const pasteData = getPasteData()
          if (pasteData == null) {
            return false
          } else {
            setPasteData(null)
            return
          }
        },
      })
    }
  }, [getPasteData, hidden, inputBindings, setPasteData])

  useEffect(
    () =>
      driveStore.subscribe(({ selectedKeys }) => {
        const predicate =
          isCloud ?
            (type: AssetType | undefined) =>
              type === AssetType.project || type === AssetType.file || type === AssetType.datalink
          : (type: AssetType | undefined) => type === AssetType.project
        const newCanDownload =
          selectedKeys.size !== 0 &&
          Array.from(selectedKeys).every((key) => {
            const node = nodeMapRef.current.get(key)
            return predicate(node?.item.type)
          })
        const currentCanDownload = driveStore.getState().canDownload
        if (currentCanDownload !== newCanDownload) {
          setCanDownload(newCanDownload)
        }
      }),
    [driveStore, isCloud, nodeMapRef, setCanDownload],
  )

  const initialProjectNameDeps = useSyncRef({ assetTree, doOpenProject, isLoading, toastAndLog })
  useEffect(() => {
    const deps = initialProjectNameDeps.current
    if (deps.isLoading) {
      nameOfProjectToImmediatelyOpenRef.current = initialProjectName
    } else {
      // The project name here might also be a string with project id, e.g. when opening
      // a project file from explorer on Windows.
      const isInitialProject = (asset: AnyAsset) =>
        asset.title === initialProjectName || asset.id === initialProjectName
      const projectToLoad = deps.assetTree
        .preorderTraversal()
        .map((node) => node.item)
        .filter(assetIsProject)
        .find(isInitialProject)
      if (projectToLoad != null) {
        deps.doOpenProject({
          type: BackendType.local,
          id: projectToLoad.id,
          title: projectToLoad.title,
          parentId: projectToLoad.parentId,
        })
      } else if (initialProjectName != null) {
        deps.toastAndLog('findProjectError', null, initialProjectName)
      }
    }
  }, [initialProjectName, initialProjectNameDeps])

  useEffect(() => {
    const savedEnabledColumns = localStorage.get('enabledColumns')
    if (savedEnabledColumns != null) {
      setEnabledColumns(new Set(savedEnabledColumns))
    }
  }, [localStorage])

  useEffect(() => {
    localStorage.set('enabledColumns', [...enabledColumns])
  }, [enabledColumns, localStorage])

  useEffect(
    () =>
      driveStore.subscribe(({ selectedKeys }) => {
        if (selectedKeys.size !== 1) {
          resetAssetPanelProps()
          setIsAssetPanelTemporarilyVisible(false)
        }
      }),
    [driveStore, resetAssetPanelProps, setIsAssetPanelTemporarilyVisible],
  )

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] = useState<number | null>(null)
  const mostRecentlySelectedIndexRef = useRef<number | null>(null)
  const selectionStartIndexRef = useRef<number | null>(null)
  const bodyRef = useRef<HTMLTableSectionElement>(null)

  const setMostRecentlySelectedIndex = useEventCallback(
    (index: number | null, isKeyboard: boolean = false) => {
      startTransition(() => {
        mostRecentlySelectedIndexRef.current = index
        setKeyboardSelectedIndex(isKeyboard ? index : null)
      })
    },
  )

  useEffect(() => {
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

  const onKeyDown = useEventCallback((event: KeyboardEvent) => {
    const { selectedAssets } = driveStore.getState()
    const prevIndex = mostRecentlySelectedIndexRef.current
    const item = prevIndex == null ? null : visibleItems[prevIndex]
    if (selectedAssets.length === 1 && item != null) {
      switch (event.key) {
        case 'Enter':
        case ' ': {
          if (event.key === ' ' && event.ctrlKey) {
            setSelectedAssets(
              selectedAssets.some((asset) => asset.id === item.item.id) ?
                selectedAssets.filter((asset) => asset.id !== item.item.id)
              : [...selectedAssets, item.item],
            )
          } else {
            switch (item.type) {
              case AssetType.directory: {
                event.preventDefault()
                event.stopPropagation()
                toggleDirectoryExpansion(item.item.id)
                break
              }
              case AssetType.project: {
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
              case AssetType.datalink: {
                event.preventDefault()
                event.stopPropagation()
                setIsAssetPanelTemporarilyVisible(true)
                break
              }
              case AssetType.secret: {
                event.preventDefault()
                event.stopPropagation()
                const id = item.item.id
                setModal(
                  <UpsertSecretModal
                    id={item.item.id}
                    name={item.item.title}
                    doCreate={async (title, value) => {
                      try {
                        await updateSecretMutation.mutateAsync([
                          id,
                          { title, value },
                          item.item.title,
                        ])
                      } catch (error) {
                        toastAndLog(null, error)
                      }
                    }}
                  />,
                )
                break
              }
              case AssetType.file:
              case AssetType.specialLoading:
              case AssetType.specialEmpty:
              case AssetType.specialError:
              default: {
                break
              }
            }
          }
          break
        }
        case 'ArrowLeft': {
          if (item.type === AssetType.directory) {
            if (item.children != null) {
              // The folder is expanded; collapse it.
              event.preventDefault()
              event.stopPropagation()
              toggleDirectoryExpansion(item.item.id, false)
            } else if (prevIndex != null) {
              // Focus parent if there is one.
              let index = prevIndex - 1
              let possibleParent = visibleItems[index]
              while (possibleParent != null && index >= 0) {
                if (possibleParent.depth < item.depth) {
                  event.preventDefault()
                  event.stopPropagation()
                  setSelectedAssets([possibleParent.item])
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
          if (item.type === AssetType.directory && item.children == null) {
            // The folder is collapsed; expand it.
            event.preventDefault()
            event.stopPropagation()
            toggleDirectoryExpansion(item.item.id, true)
          }
          break
        }
      }
    }
    switch (event.key) {
      case ' ': {
        if (event.ctrlKey && item != null) {
          setSelectedAssets(
            selectedAssets.some((asset) => asset.id === item.item.id) ?
              selectedAssets.filter((asset) => asset.id !== item.item.id)
            : [...selectedAssets, item.item],
          )
        }
        break
      }
      case 'Escape': {
        setSelectedAssets([])
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
            (itemType === AssetType.specialEmpty || itemType === AssetType.specialLoading)
          )
          if (itemType === AssetType.specialEmpty || itemType === AssetType.specialLoading) {
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
          setSelectedAssets(selection.map((newItem) => newItem.item))
        } else if (event.ctrlKey) {
          event.preventDefault()
          event.stopPropagation()
          selectionStartIndexRef.current = null
        } else if (index !== prevIndex) {
          event.preventDefault()
          event.stopPropagation()
          const newItem = visibleItems[index]
          if (newItem != null) {
            setSelectedAssets([newItem.item])
          }
          selectionStartIndexRef.current = null
        } else {
          // The arrow key will escape this container. In that case, do not stop propagation
          // and let `navigator2D` navigate to a different container.
          setSelectedAssets([])
          selectionStartIndexRef.current = null
        }
        break
      }
    }
  })

  useEffect(() => {
    const onClick = () => {
      setKeyboardSelectedIndex(null)
    }

    document.addEventListener('click', onClick, { capture: true })
    return () => {
      document.removeEventListener('click', onClick, { capture: true })
    }
  }, [setMostRecentlySelectedIndex])

  const doCopy = useEventCallback(() => {
    unsetModal()
    const { selectedKeys } = driveStore.getState()

    setPasteData({
      type: 'copy',
      data: { backendType: backend.type, category, ids: selectedKeys },
    })
  })

  const doCut = useEventCallback(() => {
    unsetModal()
    const { selectedKeys } = driveStore.getState()
    setPasteData({
      type: 'move',
      data: { backendType: backend.type, category, ids: selectedKeys },
    })
    setSelectedAssets([])
  })

  const doPaste = useEventCallback((newParentKey: DirectoryId, newParentId: DirectoryId) => {
    unsetModal()

    const { pasteData } = driveStore.getState()

    if (
      pasteData?.data.backendType === backend.type &&
      canTransferBetweenCategories(pasteData.data.category, category, user)
    ) {
      if (pasteData.data.ids.has(newParentKey)) {
        toast.error('Cannot paste a folder into itself.')
      } else {
        toggleDirectoryExpansion(newParentId, true)
        if (pasteData.type === 'copy') {
          copyAssetsMutation.mutate([[...pasteData.data.ids], newParentId])
        } else {
          cutAndPaste(newParentKey, newParentId, pasteData.data, nodeMapRef.current)
        }
        setPasteData(null)
      }
    }
  })

  const hiddenContextMenu = (
    <AssetsTableContextMenu
      hidden
      backend={backend}
      category={category}
      nodeMapRef={nodeMapRef}
      rootDirectoryId={rootDirectoryId}
      event={{ pageX: 0, pageY: 0 }}
      doCopy={doCopy}
      doCut={doCut}
      doPaste={doPaste}
    />
  )

  const onDropzoneDragOver = (event: DragEvent<Element>) => {
    const payload = ASSET_ROWS.lookup(event)
    const filtered = payload?.filter((item) => item.asset.parentId !== rootDirectoryId)
    if (filtered != null && filtered.length > 0) {
      event.preventDefault()
    } else if (event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
    }
  }

  const updateIsDraggingFiles = (event: DragEvent<Element>) => {
    if (event.dataTransfer.types.includes('Files')) {
      setIsDraggingFiles(true)
      setDroppedFilesCount(event.dataTransfer.items.length)
    }
  }

  const handleFileDrop = (event: DragEvent) => {
    setIsDraggingFiles(false)
    if (event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
      event.stopPropagation()
      void uploadFiles(Array.from(event.dataTransfer.files), rootDirectoryId, rootDirectoryId)
    }
  }

  const getAssetNodeById = useEventCallback(
    (id: AssetId) => assetTree.preorderTraversal().find((node) => node.item.id === id) ?? null,
  )

  const hideColumn = useEventCallback((column: Column) => {
    setEnabledColumns((currentColumns) => withPresence(currentColumns, column, false))
  })

  const state: AssetsTableState = useMemo(
    () => ({
      backend,
      rootDirectoryId,
      scrollContainerRef: rootRef,
      category,
      sortInfo,
      setSortInfo,
      query,
      setQuery,
      nodeMap: nodeMapRef,
      hideColumn,
      doCopy,
      doCut,
      doPaste,
      getAssetNodeById,
    }),
    [
      backend,
      category,
      doCopy,
      doCut,
      doPaste,
      getAssetNodeById,
      hideColumn,
      nodeMapRef,
      query,
      rootDirectoryId,
      setQuery,
      sortInfo,
    ],
  )

  useEffect(() => {
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

  const calculateNewSelection = useEventCallback(
    (
      event: MouseEvent | ReactMouseEvent,
      assets: readonly SelectedAssetInfo[],
      getRange: () => readonly SelectedAssetInfo[],
    ) => {
      event.stopPropagation()
      let result: readonly SelectedAssetInfo[] = []
      inputBindings.handler({
        selectRange: () => {
          result = getRange()
        },
        selectAdditionalRange: () => {
          const { selectedAssets } = driveStore.getState()
          const newAssetsMap = new Map(
            [...selectedAssets, ...getRange()].map((asset) => [asset.id, asset]),
          )
          result = [...newAssetsMap.values()]
        },
        selectAdditional: () => {
          const { selectedKeys, selectedAssets } = driveStore.getState()
          let count = 0
          for (const asset of assets) {
            if (selectedKeys.has(asset.id)) {
              count += 1
            }
          }
          const add = count * 2 < assets.length
          if (add) {
            const newAssetsMap = new Map(
              [...selectedAssets, ...assets].map((asset) => [asset.id, asset]),
            )
            result = [...newAssetsMap.values()]
          } else {
            const newIds = new Set(assets.map((asset) => asset.id))
            result = selectedAssets.filter((asset) => !newIds.has(asset.id))
          }
        },
        [DEFAULT_HANDLER]: () => {
          result = assets
        },
      })(event, false)
      return result
    },
  )

  const { startAutoScroll, endAutoScroll, onMouseEvent } = useAutoScroll(rootRef)

  const dragSelectionRangeRef = useRef<DragSelectionInfo | null>(null)

  const preventSelection = useEventCallback((event: PointerEvent) => {
    const { target } = event

    if (target instanceof HTMLElement) {
      const row = target.closest('tr')
      return Boolean(row?.dataset.selected === 'true')
    }

    return false
  })

  const onSelectionDrag = useEventCallback(({ event, rectangle }: OnDragParams) => {
    startAutoScroll()

    onMouseEvent(event)

    if (mostRecentlySelectedIndexRef.current != null) {
      setKeyboardSelectedIndex(null)
    }
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
        const assets = displayItems.slice(range.start, range.end).map((node) => node.item)
        setVisuallySelectedKeys(
          new Set(calculateNewSelection(event, assets, () => []).map((asset) => asset.id)),
        )
      }
    }
  })

  const onSelectionDragEnd = useEventCallback((event: MouseEvent) => {
    event.stopImmediatePropagation()
    endAutoScroll()
    onMouseEvent(event)
    const range = dragSelectionRangeRef.current
    if (range != null) {
      const assets = displayItems.slice(range.start, range.end).map((node) => node.item)
      setSelectedAssets(calculateNewSelection(event, assets, () => []))
    }
    setVisuallySelectedKeys(null)
    dragSelectionRangeRef.current = null
  })

  const onSelectionDragCancel = useEventCallback(() => {
    setVisuallySelectedKeys(null)
    dragSelectionRangeRef.current = null
  })

  const grabRowKeyboardFocus = useEventCallback((item: AnyAsset) => {
    setSelectedAssets([item])
  })

  const onRowClick = useEventCallback(({ asset }: AssetRowInnerProps, event: ReactMouseEvent) => {
    event.stopPropagation()
    const newIndex = visibleItems.findIndex((innerItem) => innerItem.item.id === asset.id)
    const getRange = () => {
      if (mostRecentlySelectedIndexRef.current == null) {
        return [asset]
      } else {
        const index1 = mostRecentlySelectedIndexRef.current
        const index2 = newIndex
        const startIndex = Math.min(index1, index2)
        const endIndex = Math.max(index1, index2) + 1
        return visibleItems.slice(startIndex, endIndex).map(({ item }) => item)
      }
    }
    setSelectedAssets(calculateNewSelection(event, [asset], getRange))
    setMostRecentlySelectedIndex(newIndex)
    if (!event.shiftKey) {
      selectionStartIndexRef.current = null
    }
  })

  const selectRow = useEventCallback((item: AnyAsset) => {
    setMostRecentlySelectedIndex(
      visibleItems.findIndex((visibleItem) => visibleItem.item.id === item.id),
    )
    selectionStartIndexRef.current = null
    setSelectedAssets([item])
  })

  const onRowDragStart = useEventCallback(
    (event: DragEvent<HTMLTableRowElement>, item: AnyAsset) => {
      startAutoScroll()

      onMouseEvent(event)

      let newSelectedKeys = driveStore.getState().selectedKeys

      if (!newSelectedKeys.has(item.id)) {
        setMostRecentlySelectedIndex(
          visibleItems.findIndex((visibleItem) => visibleItem.item.id === item.id),
        )
        selectionStartIndexRef.current = null
        newSelectedKeys = new Set([item.id])
        setSelectedAssets([item])
      }
      const nodes = assetTree
        .preorderTraversal()
        .filter((node) => newSelectedKeys.has(node.item.id))
      const payload: AssetRowsDragPayload = nodes.map((node) => ({
        key: node.item.id,
        asset: node.item,
      }))
      event.dataTransfer.setData(
        ASSETS_MIME_TYPE,
        JSON.stringify(nodes.map((node) => node.item.id)),
      )
      setDragImageToBlank(event)
      ASSET_ROWS.bind(event, payload)
      setModal(
        <DragModal
          event={event}
          className="flex flex-col rounded-default bg-selected-frame backdrop-blur-default"
          onDragEnd={() => {
            ASSET_ROWS.unbind(payload)
          }}
        >
          {nodes.map((node) => (
            <NameColumn
              key={node.item.id}
              isPlaceholder={node.isPlaceholder()}
              isExpanded={false}
              item={node.item}
              depth={0}
              isOpened={false}
              backendType={backend.type}
              state={state}
              // Default states.
              isSoleSelected={false}
              selected={false}
              rowState={INITIAL_ROW_STATE}
              // The drag placeholder cannot be interacted with.
              setSelected={noop}
              setRowState={noop}
              isEditable={false}
            />
          ))}
        </DragModal>,
      )
    },
  )

  const onRowDragEnd = useEventCallback(() => {
    setIsDraggingFiles(false)
    endAutoScroll()
    lastSelectedIdsRef.current = null
  })

  const onRowDrop = useEventCallback((event: DragEvent<HTMLTableRowElement>, item: AnyAsset) => {
    endAutoScroll()
    const { selectedKeys } = driveStore.getState()
    const items =
      selectedKeys.has(item.id) ?
        [...selectedKeys].flatMap((id) => {
          const otherItem = nodeMapRef.current.get(id)
          return otherItem ? [otherItem.item] : []
        })
      : [item]
    const payload = LABELS.lookup(event)
    if (payload != null) {
      event.preventDefault()
      event.stopPropagation()
      let labelsPresent = 0
      for (const selectedItem of items) {
        for (const label of selectedItem.labels ?? []) {
          if (payload.has(label)) {
            labelsPresent += 1
          }
        }
      }
      const shouldAdd = labelsPresent * 2 < items.length * payload.size
      if (shouldAdd) {
        addAssetsLabelsMutation.mutate([items, [...payload]])
      } else {
        removeAssetsLabelsMutation.mutate([items, [...payload]])
      }
      setLabelsDragPayload(null)
    }
  })

  const getAsset = useEventCallback((key: AssetId) => nodeMapRef.current.get(key)?.item ?? null)

  const setAsset = useEventCallback((assetId: AssetId, asset: AnyAsset) => {
    const listDirectoryQuery = queryClient.getQueryCache().find<DirectoryQuery>({
      queryKey: [backend.type, 'listDirectory', asset.parentId],
      exact: false,
    })

    if (listDirectoryQuery?.state.data) {
      listDirectoryQuery.setData(
        listDirectoryQuery.state.data.map((child) => (child.id === assetId ? asset : child)),
      )
    }
  })

  useImperativeHandle(assetManagementApiRef, () => ({
    getAsset,
    setAsset,
  }))

  const headerRow = (
    <tr ref={headerRowRef} className="rounded-none text-sm font-semibold">
      {[...columns].map((column) => {
        // The spread on the line above is required for React Compiler to compile this component.
        // This is a React component, even though it does not contain JSX.
        const Heading = COLUMN_HEADING[column]

        return (
          <th key={column} className={COLUMN_CSS_CLASS[column]}>
            <Heading
              sortInfo={state.sortInfo}
              hideColumn={state.hideColumn}
              setSortInfo={state.setSortInfo}
              category={state.category}
            />
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
            <IndefiniteSpinner size={LOADING_SPINNER_SIZE_PX} />
          </div>
        </td>
      </tr>
    : displayItems.map((item) => {
        const isOpenedByYou = openedProjects.some(({ id }) => item.item.id === id)
        const isOpenedOnTheBackend =
          item.item.projectState?.type != null ?
            IS_OPENING_OR_OPENED[item.item.projectState.type]
          : false
        return (
          <AssetRow
            key={item.item.id + item.path}
            isPlaceholder={item.isPlaceholder()}
            isExpanded={
              item.item.type === AssetType.directory ?
                expandedDirectoryIds.includes(item.item.id)
              : false
            }
            onCutAndPaste={cutAndPaste}
            isOpened={isOpenedByYou || isOpenedOnTheBackend}
            visibility={visibilities.get(item.item.id)}
            columns={columns}
            id={item.item.id}
            type={item.item.type}
            parentId={item.item.parentId}
            path={item.path}
            depth={item.depth}
            state={state}
            hidden={visibilities.get(item.item.id) === Visibility.hidden}
            isKeyboardSelected={
              keyboardSelectedIndex != null && item === visibleItems[keyboardSelectedIndex]
            }
            grabKeyboardFocus={grabRowKeyboardFocus}
            onClick={onRowClick}
            select={selectRow}
            onDragStart={onRowDragStart}
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
      className="flex flex-none flex-col"
      onContextMenu={(event) => {
        if (isAssetContextMenuVisible) {
          event.preventDefault()
          event.stopPropagation()
          setModal(
            <AssetsTableContextMenu
              backend={backend}
              category={category}
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
        const payload = LABELS.lookup(event)
        if (
          payload != null &&
          event.relatedTarget instanceof Node &&
          !event.currentTarget.contains(event.relatedTarget)
        ) {
          lastSelectedIdsRef.current = null
          setLabelsDragPayload(null)
        }
      }}
    >
      <table className="isolate table-fixed border-collapse rounded-rows">
        <thead className="sticky top-0 isolate z-1 bg-dashboard before:absolute before:-inset-1 before:bottom-0 before:bg-dashboard">
          {headerRow}
        </thead>
        <tbody ref={bodyRef} className="isolate">
          {itemRows}
          <tr className="hidden h-row first:table-row">
            <td colSpan={columns.length} className="bg-transparent">
              <Text
                className={twJoin('px-cell-x placeholder', isError && 'text-danger')}
                disableLineHeightCompensation
              >
                {isError ?
                  getText('thisFolderFailedToFetch')
                : category.type === 'trash' ?
                  query.query !== '' ?
                    getText('noFilesMatchTheCurrentFilters')
                  : getText('yourTrashIsEmpty')
                : category.type === 'recent' ?
                  query.query !== '' ?
                    getText('noFilesMatchTheCurrentFilters')
                  : getText('youHaveNoRecentProjects')
                : query.query !== '' ?
                  getText('noFilesMatchTheCurrentFilters')
                : getText('youHaveNoFiles')}
              </Text>
            </td>
          </tr>
        </tbody>
      </table>
      <AssetsTableAssetsUnselector asChild>
        <div
          data-testid="root-directory-dropzone"
          className={twMerge(
            'sticky left-0 grid max-w-container grow place-items-center py-20',
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
            const payload = ASSET_ROWS.lookup(event)
            const filtered = payload?.filter((item) => item.asset.parentId !== rootDirectoryId)
            if (filtered != null && filtered.length > 0) {
              event.preventDefault()
              event.stopPropagation()
              unsetModal()

              moveAssetsMutation.mutate([
                filtered.map((dragItem) => dragItem.asset.id),
                rootDirectoryId,
              ])
            }
            handleFileDrop(event)
          }}
          onClick={() => {
            setSelectedAssets([])
          }}
        >
          <FileTrigger
            onSelect={(event) => {
              void uploadFiles(Array.from(event ?? []), rootDirectoryId, rootDirectoryId)
            }}
          >
            <Button
              size="custom"
              variant="custom"
              ref={mainDropzoneRef}
              icon={DropFilesImage}
              className="rounded-2xl"
              contentClassName="h-[186px] flex flex-col items-center gap-3 text-primary/30 transition-colors duration-200 hover:text-primary/50"
            >
              {dropzoneText}
            </Button>
          </FileTrigger>
        </div>
      </AssetsTableAssetsUnselector>
    </div>
  )

  if (!isCloud && didLoadingProjectManagerFail) {
    return (
      <ErrorDisplay
        error={getText('couldNotConnectToPM')}
        resetErrorBoundary={reconnectToProjectManager}
      />
    )
  }

  return (
    <div className="relative grow contain-strict">
      <div
        data-testid="extra-columns"
        className="absolute right-3 top-0.5 isolate z-1 flex self-end p-2"
      >
        <FocusArea direction="horizontal">
          {(columnsBarProps) => (
            <div
              {...mergeProps<JSX.IntrinsicElements['div']>()(columnsBarProps, {
                className: 'inline-flex gap-icons',
                onFocus: () => {
                  setKeyboardSelectedIndex(null)
                },
              })}
            >
              {hiddenColumns.map((column) => (
                <HiddenColumn
                  key={column}
                  column={column}
                  enabledColumns={enabledColumns}
                  onColumnClick={setEnabledColumns}
                />
              ))}
            </div>
          )}
        </FocusArea>
      </div>

      <FocusArea direction="vertical">
        {(innerProps) => (
          <IsolateLayout className="isolate h-full w-full">
            <div
              {...mergeProps<JSX.IntrinsicElements['div']>()(innerProps, {
                className:
                  'flex-1 overflow-auto container-size w-full h-full scroll-p-24 scroll-smooth',
                onKeyDown,
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
                ref: rootRef,
              })}
            >
              {!hidden && hiddenContextMenu}
              {!hidden && (
                <SelectionBrush
                  targetRef={rootRef}
                  onDrag={onSelectionDrag}
                  onDragEnd={onSelectionDragEnd}
                  onDragCancel={onSelectionDragCancel}
                  preventDrag={preventSelection}
                />
              )}
              <div className="flex h-max min-h-full w-max min-w-full flex-col">
                <div className="flex h-full w-min min-w-full grow flex-col px-1">
                  {table}
                  <AssetsTableAssetsUnselector />
                </div>
              </div>
            </div>
          </IsolateLayout>
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
  )
}

/** Props for the {@link HiddenColumn} component. */
interface HiddenColumnProps {
  readonly column: Column
  readonly enabledColumns: ReadonlySet<Column>
  readonly onColumnClick: (columns: ReadonlySet<Column>) => void
}

/** Display a button to show/hide a column. */
const HiddenColumn = memo(function HiddenColumn(props: HiddenColumnProps) {
  const { column, enabledColumns, onColumnClick } = props

  const { getText } = useText()

  const onPress = useEventCallback(() => {
    const newExtraColumns = new Set(enabledColumns)
    if (enabledColumns.has(column)) {
      newExtraColumns.delete(column)
    } else {
      newExtraColumns.add(column)
    }
    onColumnClick(newExtraColumns)
  })

  return (
    <Button
      size="medium"
      variant="icon"
      key={column}
      icon={COLUMN_ICONS[column]}
      aria-label={getText(COLUMN_SHOW_TEXT_ID[column])}
      onPress={onPress}
      className="opacity-50"
    />
  )
})

/** Props for the {@link AssetsTableAssetsUnselector} component. */
export interface AssetsTableAssetsUnselectorProps {
  readonly className?: string
  readonly children?: ReactNode
  readonly asChild?: boolean
}

/** A component that unselects all assets when clicked. */
export function AssetsTableAssetsUnselector(props: AssetsTableAssetsUnselectorProps) {
  const { className, asChild = false, children } = props

  const driveStore = useDriveStore()
  const hasSelectedKeys = useStore(driveStore, (state) => state.selectedKeys.size > 0, {
    unsafeEnableTransition: true,
  })
  const setSelectedAssets = useSetSelectedAssets()

  const { pressProps } = usePress({
    isDisabled: !hasSelectedKeys,
    onPress: () => {
      setSelectedAssets([])
    },
  })

  if (asChild) {
    const childenArray = Children.toArray(children)
    const onlyChild = childenArray.length === 1 ? childenArray[0] : null

    invariant(onlyChild != null, 'Children must be a single element when `asChild` is true')
    invariant(isValidElement(onlyChild), 'Children must be a JSX element when `asChild` is true')

    return cloneElement(
      onlyChild,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-explicit-any, no-restricted-syntax
      mergeProps<any>()(pressProps as any, onlyChild.props as any),
    )
  }

  return (
    <div
      {...pressProps}
      className={twMerge('h-full w-full flex-1', className)}
      data-testid="assets-table-assets-unselector"
    >
      {children}
    </div>
  )
}

export default memo(AssetsTable)
