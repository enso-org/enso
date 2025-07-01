/** @file Table displaying a list of projects. */
import DropFilesImage from '#/assets/drop_files.svg'
import {
  Cell,
  ColumnResizer,
  FileTrigger,
  isTextDropItem,
  mergeProps,
  ResizableTableContainer,
  Row,
  Table,
  TableBody,
  Column as TableColumn,
  TableHeader,
  useDragAndDrop,
} from '#/components/aria'
import { Button } from '#/components/Button'
import { ErrorDisplay } from '#/components/ErrorBoundary'
import { IsolateLayout } from '#/components/IsolateLayout'
import { SelectionBrush, type OnDragParams } from '#/components/SelectionBrush'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { VisualTooltip } from '#/components/VisualTooltip'
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { useAutoScroll } from '#/hooks/autoScrollHooks'
import {
  backendQueryOptions,
  listDirectoryQueryOptions,
  useListDirectoryRefetchInterval,
} from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { usePaste } from '#/hooks/cutAndPasteHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useIntersectionRatio } from '#/hooks/intersectionHooks'
import { useMount } from '#/hooks/mountHooks'
import { useOpenProjectLocally } from '#/hooks/projectHooks'
import { useStore } from '#/hooks/storeHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import { useSetSuggestions } from '#/layouts/AssetSearchBar'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import { type Category } from '#/layouts/CategorySwitcher/Category'
import {
  assetsTableColumnWidthsStore,
  useAssetsTableColumnWidths,
  useSetAssetsTableColumnWidths,
} from '#/layouts/Drive/assetsTableColumnWidths'
import { useAssetsTableItems, useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import { AssetRow } from '#/pages/dashboard/components/AssetRow'
import type { SortableColumn } from '#/pages/dashboard/components/column/columnUtils'
import {
  Column,
  COLUMN_CSS_CLASS,
  COLUMN_ICONS,
  COLUMN_SHOW_TEXT_ID,
  DEFAULT_ENABLED_COLUMNS,
  getColumnList,
} from '#/pages/dashboard/components/column/columnUtils'
import { COLUMN_HEADING } from '#/pages/dashboard/components/columnHeading'
import Label from '#/pages/dashboard/components/Label'
import {
  useDriveStore,
  useSetCanDownload,
  useSetEditingNameAssetId,
  useSetPasteData,
  useSetSelectedAssets,
  useSetVisuallySelectedKeys,
  type SelectedAssetInfo,
} from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import type Backend from '#/services/Backend'
import type { AssetId, DirectoryId } from '#/services/Backend'
import {
  assetIsProject,
  AssetType,
  BackendType,
  getAssetPermissionName,
  isDirectoryId,
  type AnyAsset,
} from '#/services/Backend'
import { shallowEqual } from '#/utilities/array'
import type { AssetQueryKey } from '#/utilities/AssetQuery'
import AssetQuery from '#/utilities/AssetQuery'
import { type AssetRowsDragPayload } from '#/utilities/drag'
import { fileExtension } from '#/utilities/fileInfo'
import { DEFAULT_HANDLER } from '#/utilities/inputBindings'
import LocalStorage from '#/utilities/LocalStorage'
import { mapEntries, unsafeEntries } from '#/utilities/object'
import { PermissionAction } from '#/utilities/permissions'
import { useShallowMemo } from '#/utilities/react'
import { withPresence } from '#/utilities/set'
import type { SortInfo } from '#/utilities/sorting'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'
import {
  useBackends,
  useFullUserSession,
  useLocalStorage,
  useRightPanelData,
  useText,
} from '$/providers/react'
import { useDidLoadingProjectManagerFail } from '$/providers/react/backends'
import { useQuery, useSuspenseQuery } from '@tanstack/react-query'
import {
  Children,
  cloneElement,
  isValidElement,
  memo,
  startTransition,
  useEffect,
  useMemo,
  useReducer,
  useRef,
  useState,
  type Dispatch,
  type HTMLAttributes,
  type DragEvent as ReactDragEvent,
  type ReactNode,
  type RefObject,
  type SetStateAction,
} from 'react'
import invariant from 'tiny-invariant'
import * as z from 'zod'
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

/** Information related to a drag selection. */
interface DragSelectionInfo {
  readonly initialIndex: number
  readonly start: number
  readonly end: number
}

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  readonly backend: Backend
  readonly currentDirectoryId: DirectoryId
  readonly scrollContainerRef: RefObject<HTMLElement>
  readonly category: Category
  readonly sortInfo: SortInfo<SortableColumn> | null
  readonly setSortInfo: (sortInfo: SortInfo<SortableColumn> | null) => void
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
  readonly hideColumn: (column: Column) => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly getAssetNodeById: (id: AssetId) => AnyAsset | null
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
  readonly isEditingName: boolean
}

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
  readonly category: Category
  readonly initialProjectName: string | null
}

/** The table of project assets. */
function AssetsTable(props: AssetsTableProps) {
  const { query, setQuery, category } = props
  const { initialProjectName } = props

  const openProjectLocally = useOpenProjectLocally()
  const setCanDownload = useSetCanDownload()
  const setSuggestions = useSetSuggestions()

  const { user } = useFullUserSession()
  const { backendForType, reconnectToProjectManager } = useBackends()
  const didLoadingProjectManagerFail = useDidLoadingProjectManagerFail()
  const backend = backendForType(category.backend)
  const { data: labels } = useQuery(backendQueryOptions(backend, 'listTags', []))
  const localStorage = useLocalStorage()
  const { getText } = useText()
  const inputBindings = useInputBindings()
  const toastAndLog = useToastAndLog()
  const [enabledColumns, setEnabledColumns] = useState(DEFAULT_ENABLED_COLUMNS)
  const rightPanel = useRightPanelData()

  const allColumns = useShallowMemo(
    () => getColumnList(user.plan, backend.type, category.type),
    [backend.type, category.type, user.plan],
  )

  const columns = useMemo(
    () => allColumns.filter((column) => enabledColumns.has(column)),
    [allColumns, enabledColumns],
  )

  const hiddenColumns = useMemo(
    () => allColumns.filter((column) => !enabledColumns.has(column)),
    [allColumns, enabledColumns],
  )

  const [sortInfo, setSortInfo] = useState<SortInfo<SortableColumn> | null>(null)
  const driveStore = useDriveStore()
  const setEditingNameAssetId = useSetEditingNameAssetId()
  const setSelectedAssets = useSetSelectedAssets()
  const setVisuallySelectedKeys = useSetVisuallySelectedKeys()
  const setPasteData = useSetPasteData()

  const [renderId, rerender] = useReducer((x: number) => x + 1, 0)
  useEffect(
    () =>
      driveStore.subscribe((state, oldState) => {
        if (state.selectedAssets.length === 0 && oldState.selectedAssets.length > 0) {
          rerender()
        }
      }),
    [driveStore],
  )

  const uploadFiles = useUploadFiles(backend, category)
  const paste = usePaste(category)

  const isSingleSelectedDirectoryItem = useStore(
    driveStore,
    (state) => {
      const selectedIds = state.selectedIds

      if (selectedIds.size !== 1) {
        return false
      }

      const firstId = Array.from(selectedIds).values().next().value

      if (firstId == null) {
        return false
      }

      const isDirectory = isDirectoryId(firstId)

      return isDirectory
    },
    { unsafeEnableTransition: true },
  )

  const { queryDirectoryId, currentDirectoryId, setCurrentDirectoryId } = useDirectoryIds({
    category,
  })
  const listDirectoryRefetchInterval = useListDirectoryRefetchInterval()
  const { data: assets = [] } = useSuspenseQuery({
    ...listDirectoryQueryOptions({
      backend,
      parentId: queryDirectoryId,
      category,
      refetchInterval: listDirectoryRefetchInterval,
    }),
    retry: () => {
      setCurrentDirectoryId(null)
      return false
    },
  })

  const { visibleItems } = useAssetsTableItems({
    parentId: currentDirectoryId,
    assets,
    sortInfo,
    query,
  })

  const [isDraggingFiles, setIsDraggingFiles] = useState(false)
  const [droppedFilesCount, setDroppedFilesCount] = useState(0)
  const isCloud = backend.type === BackendType.remote
  const rootRef = useRef<HTMLDivElement | null>(null)
  const mainDropzoneRef = useRef<HTMLButtonElement | null>(null)
  const getPasteData = useEventCallback(() => driveStore.getState().pasteData)

  const isMainDropzoneVisible = useIntersectionRatio(
    rootRef,
    mainDropzoneRef,
    MINIMUM_DROPZONE_INTERSECTION_RATIO,
    (ratio) => ratio >= MINIMUM_DROPZONE_INTERSECTION_RATIO,
    true,
  )

  useEffect(() => {
    setEditingNameAssetId(null)
  }, [category, setEditingNameAssetId])

  // temporary solution to update the asset panel when the selected asset changes
  useEffect(() => {
    const selectedIds = driveStore.getState().selectedIds

    if (selectedIds.size === 1) {
      const [soleId] = selectedIds
      const asset = soleId == null ? null : assets.find((otherAsset) => otherAsset.id === soleId)

      rightPanel.setContext('drive', {
        item: asset ?? undefined,
        category,
      })
    } else {
      rightPanel.setContext('drive', { category })
    }
  }, [assets, driveStore, rightPanel, category])

  useEffect(
    () =>
      driveStore.subscribe(({ selectedIds }, { selectedIds: oldSelectedIds }) => {
        if (selectedIds !== oldSelectedIds) {
          if (selectedIds.size === 1) {
            const [soleId] = selectedIds
            const asset =
              soleId == null ? null : assets.find((otherAsset) => otherAsset.id === soleId)

            rightPanel.setContext('drive', {
              item: asset ?? undefined,
              category,
            })
            rightPanel.setTemporaryTab(undefined)
          } else {
            rightPanel.setContext('drive', { category })
          }
        }
      }),
    [category, driveStore, assets, rightPanel],
  )

  useEffect(() => {
    const nodeToSuggestion = (
      node: AnyAsset,
      key: AssetQueryKey = 'names',
    ): assetSearchBar.Suggestion => ({
      key: node.id,
      render: () => `${key === 'names' ? '' : '-:'}${node.title}`,
      addToQuery: (oldQuery) => oldQuery.addToLastTerm({ [key]: [node.title] }),
      deleteFromQuery: (oldQuery) => oldQuery.deleteFromLastTerm({ [key]: [node.title] }),
    })

    const allVisible = (negative = false) => {
      return assets.map((node) => nodeToSuggestion(node, negative ? 'negativeNames' : 'names'))
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
          const extensions = assets
            .filter((node) => node.type === AssetType.file)
            .map((node) => fileExtension(node.title))
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
          const modifieds = assets.map((node) => {
            const date = new Date(node.modifiedAt)
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
          const owners = assets.flatMap((asset) =>
            (asset.permissions ?? [])
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
  }, [isCloud, query, labels, setSuggestions, assets])

  useEffect(
    () =>
      inputBindings.attach(document.body, 'keydown', {
        cancelCut: () => {
          const pasteData = getPasteData()
          if (pasteData == null) {
            return false
          } else {
            setPasteData(null)
            return
          }
        },
      }),
    [getPasteData, inputBindings, setPasteData],
  )

  useEffect(
    () =>
      driveStore.subscribe(({ selectedIds }) => {
        const predicate =
          isCloud ?
            (type: AssetType | undefined) =>
              type === AssetType.project || type === AssetType.file || type === AssetType.datalink
          : (type: AssetType | undefined) => type === AssetType.project
        const map = new Map(assets.map((item) => [item.id, item]))
        const newCanDownload =
          selectedIds.size !== 0 &&
          Array.from(selectedIds).every((id) => predicate(map.get(id)?.type))
        const currentCanDownload = driveStore.getState().canDownload
        if (currentCanDownload !== newCanDownload) {
          setCanDownload(newCanDownload)
        }
      }),
    [driveStore, isCloud, assets, setCanDownload],
  )

  const initialProjectNameDeps = useSyncRef({
    items: assets,
    openProjectLocally,
    toastAndLog,
  })

  useEffect(() => {
    const deps = initialProjectNameDeps.current
    // The project name here might also be a string with project id, e.g. when opening
    // a project file from explorer on Windows.
    const isInitialProject = (asset: AnyAsset) =>
      asset.title === initialProjectName || asset.id === initialProjectName
    const projectToLoad = deps.items.filter(assetIsProject).find(isInitialProject)
    if (projectToLoad != null) {
      void deps.openProjectLocally(projectToLoad, BackendType.local)
    } else if (initialProjectName != null && initialProjectName !== '') {
      deps.toastAndLog('findProjectError', null, initialProjectName)
    }
  }, [initialProjectName, initialProjectNameDeps])

  useEffect(() => {
    const savedEnabledColumns = localStorage.get('enabledColumns')
    if (
      savedEnabledColumns != null &&
      !shallowEqual(savedEnabledColumns, [...DEFAULT_ENABLED_COLUMNS])
    ) {
      setEnabledColumns(new Set(savedEnabledColumns))
    }
  }, [localStorage])

  useEffect(() => {
    localStorage.set('enabledColumns', [...enabledColumns])
  }, [enabledColumns, localStorage])

  useEffect(
    () =>
      driveStore.subscribe(({ selectedIds }) => {
        if (selectedIds.size !== 1) {
          rightPanel.setContext('drive', { category })
          rightPanel.setTemporaryTab(undefined)
        }
      }),
    [driveStore, rightPanel, category],
  )

  const bodyRef = useRef<HTMLTableSectionElement>(null)

  const doCopy = useEventCallback(() => {
    unsetModal()
    const { selectedIds } = driveStore.getState()

    setPasteData({
      type: 'copy',
      data: {
        backendType: backend.type,
        category,
        assets: Array.from(selectedIds)
          .map((id) => assets.find((asset) => asset.id === id))
          .filter((asset) => asset != null),
      },
    })
  })

  const doCut = useEventCallback(() => {
    unsetModal()
    const { selectedIds } = driveStore.getState()
    setPasteData({
      type: 'move',
      data: {
        backendType: backend.type,
        category,
        assets: Array.from(selectedIds)
          .map((id) => assets.find((asset) => asset.id === id))
          .filter((asset) => asset != null),
      },
    })
    setSelectedAssets([])
  })

  const hiddenContextMenu =
    isSingleSelectedDirectoryItem ? null : (
      <AssetsTableContextMenu
        rootRef={rootRef}
        hidden
        backend={backend}
        category={category}
        currentDirectoryId={currentDirectoryId}
        event={{ pageX: 0, pageY: 0 }}
        doCopy={doCopy}
        doCut={doCut}
      />
    )

  const onDropzoneDragOver = (event: ReactDragEvent) => {
    // Unconditionally handle drag event even if drop target is invalid
    // otherwise the drag modal stays around.
    if (
      event.dataTransfer.types.includes(ASSETS_MIME_TYPE) ||
      event.dataTransfer.types.includes('Files')
    ) {
      event.preventDefault()
      return
    }
  }

  const updateIsDraggingFiles = (event: ReactDragEvent) => {
    if (event.dataTransfer.types.includes('Files')) {
      setIsDraggingFiles(true)
      setDroppedFilesCount(event.dataTransfer.items.length)
    }
  }

  const handleFileDrop = (event: ReactDragEvent) => {
    setIsDraggingFiles(false)
    if (event.dataTransfer.types.includes('Files') === true) {
      event.preventDefault()
      event.stopPropagation()
      void uploadFiles(Array.from(event.dataTransfer.files), currentDirectoryId)
    }
  }

  const getAssetNodeById = useEventCallback(
    (id: AssetId) => assets.find((node) => node.id === id) ?? null,
  )

  const hideColumn = useEventCallback((column: Column) => {
    setEnabledColumns((currentColumns) => withPresence(currentColumns, column, false))
  })

  const state = useMemo<AssetsTableState>(
    () => ({
      backend,
      currentDirectoryId,
      scrollContainerRef: rootRef,
      category,
      sortInfo,
      setSortInfo,
      query,
      setQuery,
      hideColumn,
      doCopy,
      doCut,
      getAssetNodeById,
    }),
    [
      backend,
      category,
      currentDirectoryId,
      doCopy,
      doCut,
      getAssetNodeById,
      hideColumn,
      query,
      setQuery,
      sortInfo,
    ],
  )

  const calculateNewSelection = useEventCallback(
    (
      event: MouseEvent,
      otherAssets: readonly SelectedAssetInfo[],
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
          const { selectedIds, selectedAssets } = driveStore.getState()
          let count = 0
          for (const asset of otherAssets) {
            if (selectedIds.has(asset.id)) {
              count += 1
            }
          }
          const add = count * 2 < otherAssets.length
          if (add) {
            const newAssetsMap = new Map(
              [...selectedAssets, ...otherAssets].map((asset) => [asset.id, asset]),
            )
            result = [...newAssetsMap.values()]
          } else {
            const newIds = new Set(otherAssets.map((asset) => asset.id))
            result = selectedAssets.filter((asset) => !newIds.has(asset.id))
          }
        },
        [DEFAULT_HANDLER]: () => {
          result = otherAssets
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
      if (target.classList.contains('ColumnResizer')) {
        return true
      }
      const row = target.closest('tr')
      return row?.dataset.selected === 'true'
    }

    return false
  })

  const onSelectionDrag = useEventCallback(({ event, rectangle }: OnDragParams) => {
    startAutoScroll()

    onMouseEvent(event)

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
      startTransition(() => {
        if (range == null) {
          setVisuallySelectedKeys(null)
        } else {
          const otherAssets = visibleItems.slice(range.start, range.end).map((node) => node)
          setVisuallySelectedKeys(
            new Set(calculateNewSelection(event, otherAssets, () => []).map((asset) => asset.id)),
          )
        }
      })
    }
  })

  const onSelectionDragEnd = useEventCallback((event: MouseEvent) => {
    event.stopImmediatePropagation()
    endAutoScroll()
    onMouseEvent(event)
    const range = dragSelectionRangeRef.current
    if (range != null) {
      const otherAssets = visibleItems.slice(range.start, range.end).map((node) => node)
      setSelectedAssets(calculateNewSelection(event, otherAssets, () => []))
    }
    setVisuallySelectedKeys(null)
    dragSelectionRangeRef.current = null
  })

  const onSelectionDragCancel = useEventCallback(() => {
    setVisuallySelectedKeys(null)
    dragSelectionRangeRef.current = null
  })

  const selectRow = useEventCallback((asset: AnyAsset) => {
    setSelectedAssets([asset])
  })

  const onRowDrop = useEventCallback(
    (event: DragEvent | ReactDragEvent, item: AnyAsset | null = null) => {
      if (category.type === 'trash' || category.type === 'recent') {
        return
      }
      endAutoScroll()
      const directoryId = item?.type === AssetType.directory ? item.id : currentDirectoryId
      const payload =
        event.dataTransfer ?
          // eslint-disable-next-line no-restricted-syntax
          (JSON.parse(event.dataTransfer.getData(ASSETS_MIME_TYPE)) as AssetRowsDragPayload)
        : null
      const items = payload?.items ?? []

      if (payload != null && items.every((innerItem) => innerItem.key !== directoryId)) {
        event.preventDefault()
        event.stopPropagation()
        unsetModal()

        void paste({
          fromCategory: payload.category,
          toCategory: category,
          newParentId: directoryId,
          pasteData: {
            backendType: backend.type,
            assets: items
              .filter(({ asset }) => asset.parentId !== directoryId)
              .map(({ asset }) => asset),
            category,
          },
          method: 'move',
        })
        return
      }
      if (event.dataTransfer?.types.includes('Files') === true) {
        event.preventDefault()
        event.stopPropagation()
        void uploadFiles(Array.from(event.dataTransfer.files), directoryId)
      }
    },
  )

  const dropzoneText =
    isDraggingFiles ?
      droppedFilesCount === 1 ?
        getText('assetsDropFileDescription')
      : getText('assetsDropFilesDescription', droppedFilesCount)
    : getText('assetsDropzoneDescription')

  const specialEmptyText =
    query.query !== '' ? getText('noFilesMatchTheCurrentFilters')
    : currentDirectoryId !== category.homeDirectoryId ? getText('thisFolderIsEmpty')
    : null

  const widths = useAssetsTableColumnWidths()
  const widthsRef = useRef(widths)

  useMount(() => {
    widthsRef.current = structuredClone(widths)
  })

  const setWidths = useSetAssetsTableColumnWidths()
  const onResize: Parameters<typeof ResizableTableContainer>[0]['onResize'] = (newWidths) => {
    for (const [column, width] of unsafeEntries(widths)) {
      const columnRef = columnRefs.current[column]
      if (!columnRef) {
        continue
      }
      const newWidth = newWidths.get(column) ?? width
      columnRef.style.width = typeof newWidth === 'string' ? newWidth : `${newWidth}px`
    }
  }
  const onResizeEnd: Parameters<typeof ResizableTableContainer>[0]['onResize'] = (newWidths) => {
    setWidths(
      mapEntries(
        assetsTableColumnWidthsStore.getState().widths,
        (key, value) => newWidths.get(key) ?? value,
      ),
    )
  }
  const columnRefs = useRef<Partial<Record<Column, HTMLTableCellElement | undefined>>>({})

  // useDebugEffect(
  //   () => {},
  //   [visibleItems, columns, category.id, currentDirectoryId, renderId],
  //   'table render',
  //   ['visibleItems', 'columns', 'category.id', 'currentDirectoryId', 'renderId'],
  // )

  const getAsset = useGetAsset()
  const { dragAndDropHooks } = useDragAndDrop({
    getItems: (keys) => [
      {
        [ASSETS_MIME_TYPE]: JSON.stringify({
          category,
          items: [...keys].flatMap((id) => {
            // eslint-disable-next-line no-restricted-syntax
            const asset = getAsset(id as AssetId)
            return asset ? [{ asset, key: asset.id }] : []
          }),
        } satisfies AssetRowsDragPayload),
      },
    ],
    /**
     *
     */
    onRootDrop(event) {
      void (async () => {
        const items = await Promise.all(
          event.items.filter(isTextDropItem).map(
            async (item): Promise<AssetRowsDragPayload> =>
              // eslint-disable-next-line @typescript-eslint/no-unsafe-return
              JSON.parse(await item.getText(ASSETS_MIME_TYPE)),
          ),
        )
        paste(items)
        setItems(items)
      })()
    },
  })

  const table = (
    <div
      className="flex flex-none flex-col"
      onKeyDownCapture={(event) => {
        // Allow `ArrowLeft` and `ArrowRight` to work in inputs, rather than using react-aria's
        // navigation between components.
        if (event.target instanceof HTMLInputElement) {
          switch (event.key) {
            case 'ArrowLeft':
            case 'ArrowRight': {
              event.stopPropagation()
              break
            }
          }
        }
      }}
    >
      <ResizableTableContainer onResize={onResize} onResizeEnd={onResizeEnd}>
        <Table
          /* The key is required to reset selection state when the category or folder changes. */
          key={`${category.id}/${currentDirectoryId}/${renderId}`}
          aria-label={getText('drivePageName')}
          data-testid="assets-table"
          selectionMode="multiple"
          selectionBehavior="replace"
          onSelectionChange={(selection) => {
            const set = new Set(selection)
            setSelectedAssets(assets.filter((asset) => set.has(asset.id)))
          }}
          dragAndDropHooks={dragAndDropHooks}
          className="AssetsTable isolate border-collapse rounded-rows"
        >
          <TableHeader
            columns={columns.map((column) => ({ id: column }))}
            className="sticky top-0 isolate z-1 bg-dashboard"
          >
            {({ id: column }) => {
              // The spread on the line above is required for React Compiler to compile this component.
              // This is a React component, even though it does not contain JSX.
              const Heading = COLUMN_HEADING[column]

              return (
                <TableColumn
                  ref={(el) => {
                    if (el) {
                      // eslint-disable-next-line no-restricted-syntax
                      columnRefs.current[column] = el as HTMLTableCellElement
                    } else {
                      columnRefs.current[column] = undefined
                    }
                  }}
                  key={column}
                  width={widths[column]}
                  className={twJoin(
                    'before:absolute before:-inset-1 before:bottom-0 before:bg-dashboard',
                    COLUMN_CSS_CLASS[column],
                  )}
                  isRowHeader={column === Column.name}
                >
                  <div className="group flex gap-2">
                    <Heading
                      sortInfo={sortInfo}
                      hideColumn={hideColumn}
                      setSortInfo={setSortInfo}
                      category={category}
                    />
                    <VisualTooltip
                      tooltip={getText('resizeThisColumn')}
                      tooltipPlacement="top"
                      className="m-1 w-[0.375rem] min-w-[0.375rem]"
                    >
                      <ColumnResizer className="ColumnResizer relative z-1 h-full w-full cursor-ew-resize rounded-full bg-primary/20 opacity-0 transition-all group-hover:opacity-100 [&[data-resizing]]:opacity-100" />
                    </VisualTooltip>
                  </div>
                </TableColumn>
              )
            }}
          </TableHeader>

          {assets.length !== 0 ?
            <TableBody
              ref={bodyRef}
              items={visibleItems}
              dependencies={[visibleItems, columns]}
              className="isolate"
            >
              {(item) => (
                <AssetRow
                  key={`${item.virtualParentsPath}/${item.id}`}
                  item={item}
                  columns={columns}
                  state={state}
                  select={selectRow}
                  tableRootRef={rootRef}
                />
              )}
            </TableBody>
          : <TableBody ref={bodyRef} dependencies={[visibleItems, columns]} className="isolate">
              <Row className="hidden h-row first:table-row">
                <Cell colSpan={columns.length} className="h-table-row bg-transparent">
                  <Text className="px-cell-x placeholder" disableLineHeightCompensation>
                    {category.type === 'trash' ?
                      (specialEmptyText ?? getText('yourTrashIsEmpty'))
                    : category.type === 'recent' ?
                      (specialEmptyText ?? getText('youHaveNoRecentProjects'))
                    : (specialEmptyText ?? getText('youHaveNoFiles'))}
                  </Text>
                </Cell>
              </Row>
            </TableBody>
          }
        </Table>
      </ResizableTableContainer>

      <AssetsTableAssetsUnselector asChild>
        <div
          data-testid="root-directory-dropzone"
          className={twMerge(
            'sticky left-1 grid max-w-[calc(100cqw_-_0.5rem)] grow place-items-center pb-40 pt-20',
            (category.type === 'recent' || category.type === 'trash') && 'hidden',
          )}
          onDragEnter={onDropzoneDragOver}
          onDragOver={onDropzoneDragOver}
          onDragEnd={() => {
            setIsDraggingFiles(false)
          }}
          onDrop={(event) => {
            event.preventDefault()
            event.stopPropagation()
            onRowDrop(event)
          }}
        >
          <FileTrigger
            onSelect={(event) => {
              void uploadFiles(Array.from(event ?? []), currentDirectoryId)
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
      {hiddenContextMenu}

      {hiddenColumns.length !== 0 && (
        <div
          data-testid="extra-columns"
          className="absolute right-3 top-0.5 z-1 flex self-end bg-dashboard p-2"
        >
          <div className="inline-flex gap-icons">
            {hiddenColumns.map((column) => (
              <HiddenColumn
                key={column}
                column={column}
                enabledColumns={enabledColumns}
                onColumnClick={setEnabledColumns}
              />
            ))}
          </div>
        </div>
      )}

      <IsolateLayout className="isolate h-full w-full" useRAF>
        <div
          tabIndex={-1}
          className="h-full w-full flex-1 scroll-p-24 overflow-auto scroll-smooth container-size"
          onDragEnter={updateIsDraggingFiles}
          onDragOver={updateIsDraggingFiles}
          onDragEnd={() => {
            setIsDraggingFiles(false)
          }}
          ref={rootRef}
          onClick={(event) => {
            if (!event.currentTarget.contains(document.activeElement)) {
              event.currentTarget.focus()
            }
          }}
        >
          <SelectionBrush
            targetRef={rootRef}
            onDrag={onSelectionDrag}
            onDragEnd={onSelectionDragEnd}
            onDragCancel={onSelectionDragCancel}
            preventDrag={preventSelection}
          />
          <div
            className="flex h-max min-h-full flex-col"
            onContextMenu={(event) => {
              event.preventDefault()
              event.stopPropagation()
              setModal(
                <AssetsTableContextMenu
                  rootRef={rootRef}
                  backend={backend}
                  category={category}
                  event={event}
                  doCopy={doCopy}
                  doCut={doCut}
                  currentDirectoryId={currentDirectoryId}
                />,
              )
            }}
          >
            <div
              className="flex h-full grow flex-col px-1"
              onDrop={(event) => {
                onRowDrop(event, null)
              }}
            >
              {table}
              <AssetsTableAssetsUnselector />
            </div>
          </div>
        </div>
      </IsolateLayout>

      {isDraggingFiles && !isMainDropzoneVisible && category.canUploadHere && (
        <div className="pointer-events-none absolute bottom-4 left-1/2 -translate-x-1/2">
          <div
            className="pointer-events-auto flex items-center justify-center gap-3 rounded-default bg-selected-frame px-8 py-6 text-primary/50 backdrop-blur-3xl transition-all"
            onDragEnter={onDropzoneDragOver}
            onDragOver={onDropzoneDragOver}
            onDragEnd={() => {
              setIsDraggingFiles(false)
            }}
            onDrop={handleFileDrop}
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
  const hasSelectedKeys = useStore(driveStore, (state) => state.selectedIds.size > 0, {
    unsafeEnableTransition: true,
  })
  const setSelectedAssets = useSetSelectedAssets()

  const onPointerDown = useEventCallback(() => {
    if (!hasSelectedKeys) {
      return
    }

    setSelectedAssets([])
  })

  if (asChild) {
    const childenArray = Children.toArray(children)
    const onlyChild = childenArray.length === 1 ? childenArray[0] : null

    invariant(
      onlyChild != null && isValidElement(onlyChild),
      'Children must be a single JSX element when `asChild` is true',
    )

    return cloneElement(
      onlyChild,
      // eslint-disable-next-line no-restricted-syntax
      mergeProps<HTMLAttributes<HTMLElement>>()(onlyChild.props as HTMLAttributes<HTMLElement>, {
        onPointerDown,
      }),
    )
  }

  return (
    <div
      className={twMerge('h-full w-full flex-1', className)}
      data-testid="assets-table-assets-unselector"
      onPointerDown={onPointerDown}
    >
      {children}
    </div>
  )
}

export default memo(AssetsTable)
