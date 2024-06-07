/** @file Table displaying a list of projects. */
import * as React from 'react'

import * as toast from 'react-toastify'
import * as tailwindMerge from 'tailwind-merge'

import DropFilesImage from 'enso-assets/drop_files.svg'

import * as store from '#/store'

import * as backendHooks from '#/hooks/backendHooks'
import * as scrollHooks from '#/hooks/scrollHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetPanel from '#/layouts/AssetPanel'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import type * as assetRow from '#/components/dashboard/AssetRow'
import AssetRow from '#/components/dashboard/AssetRow'
import AssetRows from '#/components/dashboard/AssetRows'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import * as columnHeading from '#/components/dashboard/columnHeading'
import SelectionBrush from '#/components/SelectionBrush'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as array from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import * as dateTime from '#/utilities/dateTime'
import * as drag from '#/utilities/drag'
import type * as geometry from '#/utilities/geometry'
import * as inputBindingsModule from '#/utilities/inputBindings'
import LocalStorage from '#/utilities/LocalStorage'
import type * as pasteDataModule from '#/utilities/pasteData'
import PasteType from '#/utilities/PasteType'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import * as set from '#/utilities/set'
import type * as sorting from '#/utilities/sorting'

// FIXME: Reimplement `-startup.project` using React Query - it should query the root directory
// and find a project with a matching name. In the future it should fire a search request through
// all projects.

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
/** The number of pixels the header bar should shrink when the column selector is visible,
 * assuming 0 icons are visible in the column selector. */
const COLUMNS_SELECTOR_BASE_WIDTH_PX = 4
/** The number of pixels the header bar should shrink per collapsed column. */
const COLUMNS_SELECTOR_ICON_WIDTH_PX = 28

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
  [Category.cloud]: backendModule.FilterBy.active,
  [Category.local]: backendModule.FilterBy.active,
  [Category.recent]: null,
  [Category.trash]: backendModule.FilterBy.trashed,
}

// ===================
// === AssetsTable ===
// ===================

// FIXME: Remove obsolete keys
/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  readonly backend: Backend
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly scrollContainerRef: React.RefObject<HTMLElement>
  readonly category: Category
  readonly hasPasteData: boolean
  readonly setPasteData: (pasteData: pasteDataModule.PasteData<Set<backendModule.AssetId>>) => void
  readonly sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null
  readonly setSortInfo: (sortInfo: sorting.SortInfo<columnUtils.SortableColumn> | null) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setProjectStartupInfo: (projectStartupInfo: backendModule.ProjectStartupInfo) => void
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly hideColumn: (column: columnUtils.Column) => void
  readonly doOpenEditor: (project: backendModule.ProjectAsset, switchPage: boolean) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
  readonly doPaste: (newParentId: backendModule.DirectoryId) => void
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
  readonly isEditingName: boolean
  readonly temporarilyAddedLabels: ReadonlySet<backendModule.LabelName>
  readonly temporarilyRemovedLabels: ReadonlySet<backendModule.LabelName>
}

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
  readonly hidden: boolean
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setProjectStartupInfo: (projectStartupInfo: backendModule.ProjectStartupInfo) => void
  readonly setCanDownload: (canDownload: boolean) => void
  readonly category: Category
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly doOpenEditor: (
    backend: Backend,
    project: backendModule.ProjectAsset,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
  const { hidden, query, setQuery, setProjectStartupInfo, category, projectStartupInfo } = props
  const { doOpenEditor: doOpenEditorRaw, doCloseEditor: doCloseEditorRaw } = props
  const { setAssetPanelProps, setIsAssetPanelTemporarilyVisible } = props

  const { session } = sessionProvider.useSession()
  const { user } = authProvider.useNonPartialUserSession()
  const backend = backendProvider.useBackend(category)
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toggleIsAssetOpen = store.useStore(storeState => storeState.toggleIsAssetOpen)
  const setIsAssetOpen = store.useStore(storeState => storeState.setIsAssetOpen)
  const setSelectedAssetIds = store.useStore(storeState => storeState.setSelectedAssetIds)
  const setDragSelectedAssetIds = store.useStore(storeState => storeState.setDragSelectedAssetIds)
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [enabledColumns, setEnabledColumns] = localStorageProvider.useLocalStorageValue(
    'enabledColumns',
    () => columnUtils.DEFAULT_ENABLED_COLUMNS
  )
  const [sortInfo, setSortInfo] =
    React.useState<sorting.SortInfo<columnUtils.SortableColumn> | null>(null)
  const selectedKeysRef = React.useRef<readonly backendModule.AssetId[]>([])
  React.useEffect(
    () =>
      store.useStore.subscribe(state => {
        selectedKeysRef.current = state.getSelectedAssetIds(backend.type)
      }),
    [backend.type]
  )
  const [pasteData, setPasteData] = React.useState<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const rootDirectoryId = React.useMemo(
    () => backend.rootDirectoryId(user) ?? backendModule.DirectoryId(''),
    [backend, user]
  )
  const rootDirectoryAsset = React.useMemo<
    backendHooks.WithPlaceholder<backendModule.DirectoryAsset>
  >(
    () => ({
      type: backendModule.AssetType.directory,
      title: '(root)',
      id: rootDirectoryId,
      description: null,
      labels: [],
      permissions: [],
      modifiedAt: dateTime.toRfc3339(new Date(0)),
      parentId: backendModule.DirectoryId(''),
      projectState: null,
      isPlaceholder: false,
    }),
    [rootDirectoryId]
  )
  const filter = React.useMemo(() => query.isMatch.bind(query), [query])
  const [isDropzoneVisible, setIsDropzoneVisible] = React.useState(false)
  const [droppedFilesCount, setDroppedFilesCount] = React.useState(0)
  const isCloud = backend.type === backendModule.BackendType.remote
  const rootRef = React.useRef<HTMLDivElement | null>(null)
  const headerRowRef = React.useRef<HTMLTableRowElement>(null)
  const pasteDataRef = React.useRef<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const columns = columnUtils.getColumnList(backend.type, enabledColumns)
  const dropzoneText = isDropzoneVisible
    ? droppedFilesCount === 1
      ? getText('assetsDropFileDescription')
      : getText('assetsDropFilesDescription', droppedFilesCount)
    : getText('assetsDropzoneDescription')

  const uploadFilesMutation = backendHooks.useBackendUploadFilesMutation(backend)
  const updateAssetMutation = backendHooks.useBackendMutation(backend, 'updateAsset')
  const copyAssetMutation = backendHooks.useBackendMutation(backend, 'copyAsset')
  const openProjectMutation = backendHooks.useBackendMutation(backend, 'openProject')

  React.useEffect(() => {
    setIsAssetOpen(backend.type, rootDirectoryId, true)
  }, [backend.type, rootDirectoryId, setIsAssetOpen])

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
  }, [hidden, /* should never change */ inputBindings])

  // FIXME: re-add `setIsAssetPanelTemporarilyVisible(false)`

  const mostRecentlySelectedIndexRef = React.useRef<number | null>(null)
  const selectionStartIndexRef = React.useRef<number | null>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)

  // This is not a React component, even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const onKeyDown = (event: React.KeyboardEvent) => {
    const prevIndex = mostRecentlySelectedIndexRef.current
    const item = prevIndex == null ? null : visibleItems[prevIndex]
    if (selectedKeysRef.current.length === 1 && item != null) {
      switch (event.key) {
        case 'Enter':
        case ' ': {
          if (event.key === ' ' && event.ctrlKey) {
            const keys = selectedKeysRef.current
            setSelectedAssetIds(set.withPresence(keys, item.id, !keys.has(item.id)))
          } else {
            switch (item.type) {
              case backendModule.AssetType.directory: {
                event.preventDefault()
                event.stopPropagation()
                toggleIsAssetOpen(backend.type, item.id)
                break
              }
              case backendModule.AssetType.project: {
                event.preventDefault()
                event.stopPropagation()
                openProjectMutation.mutate([
                  item.id,
                  { executeAsync: false, cognitoCredentials: session },
                ])
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
                setModal(
                  <UpsertSecretModal backend={backend} asset={item} parentDirectoryId={null} />
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
          if (item.type === backendModule.AssetType.directory && item.children != null) {
            // The folder is expanded; collapse it.
            event.preventDefault()
            event.stopPropagation()
            setIsAssetOpen(backend.type, item.id, false)
          } else {
            // Focus parent if there is one.
            const index2 = allRows.findIndex(row => row.id === item.parentId)
            if (index2 !== -1) {
              event.preventDefault()
              event.stopPropagation()
              setSelectedAssetIds(backend.type, [item.parentId])
              setMostRecentlySelectedIndex(index2, true)
            }
          }
          break
        }
        case 'ArrowRight': {
          if (item.type === backendModule.AssetType.directory && item.children == null) {
            // The folder is collapsed; expand it.
            event.preventDefault()
            event.stopPropagation()
            setIsAssetOpen(backend.type, item.id, true)
          }
          break
        }
      }
    }
    switch (event.key) {
      case ' ': {
        if (event.ctrlKey && item != null) {
          const keys = selectedKeysRef.current
          setSelectedAssetIds(set.withPresence(keys, item.id, !keys.has(item.id)))
        }
        break
      }
      case 'Escape': {
        setSelectedAssetIds(backend.type, [])
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
          setSelectedAssetIds(
            backend.type,
            selection.map(newItem => newItem.id)
          )
        } else if (event.ctrlKey) {
          event.preventDefault()
          event.stopPropagation()
          selectionStartIndexRef.current = null
        } else if (index !== prevIndex) {
          event.preventDefault()
          event.stopPropagation()
          const newItem = visibleItems[index]
          if (newItem != null) {
            setSelectedAssetIds(backend.type, [newItem.id])
          }
          selectionStartIndexRef.current = null
        } else {
          // The arrow key will escape this container. In that case, do not stop propagation
          // and let `navigator2D` navigate to a different container.
          setSelectedAssetIds(backend.type, [])
          selectionStartIndexRef.current = null
        }
        break
      }
    }
  }

  const doOpenEditor = React.useCallback(
    (project: backendModule.ProjectAsset, switchPage: boolean) => {
      doOpenEditorRaw(backend, project, switchPage)
    },
    [backend, doOpenEditorRaw]
  )

  const doCloseEditor = React.useCallback(
    (project: backendModule.ProjectAsset) => {
      if (project.id === projectStartupInfo?.projectAsset.id) {
        doCloseEditorRaw(project)
      }
    },
    [projectStartupInfo, doCloseEditorRaw]
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
    setSelectedAssetIds(backend.type, [])
  }, [backend.type, pasteData, setSelectedAssetIds, /* should never change */ unsetModal])

  const doPaste = React.useCallback(
    (newParentId: backendModule.DirectoryId) => {
      unsetModal()
      if (pasteData != null) {
        if (pasteData.data.has(newParentKey)) {
          toast.toast.error('Cannot paste a folder into itself.')
        } else {
          setIsAssetOpen(backend.type, newParentId, true)
          const assets = Array.from(pasteData.data, id => nodeMapRef.current.get(id)).flatMap(
            asset => (asset ? [asset.item] : [])
          )
          if (pasteData.type === PasteType.copy) {
            for (const asset of assets) {
              copyAssetMutation.mutate([asset.id, newParentId, asset.title, '(unknown)'])
            }
          } else {
            for (const asset of assets) {
              updateAssetMutation.mutate([
                asset.id,
                { parentDirectoryId: newParentId, description: null },
                asset.title,
              ])
            }
          }
          setPasteData(null)
        }
      }
    },
    [pasteData, /* should never change */ unsetModal]
  )

  const hideColumn = React.useCallback(
    (column: columnUtils.Column) => {
      setEnabledColumns(currentColumns =>
        currentColumns.filter(otherColumn => otherColumn !== column)
      )
    },
    [setEnabledColumns]
  )

  const hiddenContextMenu = React.useMemo(
    () => (
      <AssetsTableContextMenu
        hidden
        backend={backend}
        category={category}
        pasteData={pasteData}
        rootDirectoryId={rootDirectoryId}
        event={{ pageX: 0, pageY: 0 }}
        doCopy={doCopy}
        doCut={doCut}
        doPaste={doPaste}
      />
    ),
    [backend, rootDirectoryId, category, pasteData, doCopy, doCut, doPaste]
  )

  const onDropzoneDragOver = (event: React.DragEvent<Element>) => {
    const payload = drag.ASSET_ROWS.lookup(event)
    const filtered = payload?.filter(item => item.parentId !== rootDirectoryId)
    if (filtered != null && filtered.length > 0) {
      event.preventDefault()
    } else if (event.dataTransfer.types.includes('Files')) {
      setIsDropzoneVisible(true)
      setDroppedFilesCount(event.dataTransfer.items.length)
      event.preventDefault()
    }
  }

  const state = React.useMemo<AssetsTableState>(
    // The type MUST be here to trigger excess property errors at typecheck time.
    () => ({
      backend,
      rootDirectoryId,
      scrollContainerRef: rootRef,
      category,
      hasPasteData: pasteData != null,
      setPasteData,
      sortInfo,
      setSortInfo,
      query,
      setQuery,
      setProjectStartupInfo,
      setAssetPanelProps,
      setIsAssetPanelTemporarilyVisible,
      hideColumn,
      doOpenEditor,
      doCloseEditor,
      doCopy,
      doCut,
      doPaste,
    }),
    [
      backend,
      rootDirectoryId,
      category,
      pasteData,
      sortInfo,
      query,
      doOpenEditor,
      doCloseEditor,
      doCopy,
      doCut,
      doPaste,
      /* should never change */ hideColumn,
      /* should never change */ setAssetPanelProps,
      /* should never change */ setIsAssetPanelTemporarilyVisible,
      /* should never change */ setProjectStartupInfo,
      /* should never change */ setQuery,
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
      const hiddenColumnsCount = columnUtils.CLOUD_COLUMNS.length - enabledColumns.length
      const shrinkBy =
        COLUMNS_SELECTOR_BASE_WIDTH_PX + COLUMNS_SELECTOR_ICON_WIDTH_PX * hiddenColumnsCount
      const rightOffset = rootRef.current.clientWidth + rootRef.current.scrollLeft - shrinkBy
      headerRowRef.current.style.clipPath = `polygon(0 0, ${rightOffset}px 0, ${rightOffset}px 100%, 0 100%)`
    }
  }, [enabledColumns.length])

  React.useEffect(
    () =>
      inputBindings.attach(
        sanitizedEventTargets.document.body,
        'click',
        {
          selectAdditional: () => {},
          selectAdditionalRange: () => {},
          [inputBindingsModule.DEFAULT_HANDLER]: () => {
            if (selectedKeysRef.current.length !== 0) {
              setSelectedAssetIds(backend.type, [])
              setMostRecentlySelectedIndex(null)
            }
          },
        },
        false
      ),
    [backend.type, setSelectedAssetIds, /* should never change */ inputBindings]
  )

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
        selectAdditional: bindingEvent => {
          const newSelectedKeys = new Set(selectedKeysRef.current)
          for (const key of keys) {
            set.setPresence(newSelectedKeys, key, !bindingEvent.shiftKey)
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
          setDragSelectedAssetIds(backend.type, [])
        } else {
          const keys = displayItems.slice(range.start, range.end).map(node => node.key)
          setDragSelectedAssetIds(
            backend.type,
            calculateNewKeys(event, keys, () => [])
          )
        }
      }
    },
    [backend.type, displayItems, calculateNewKeys]
  )

  const onSelectionDragEnd = React.useCallback(
    (event: MouseEvent) => {
      const range = dragSelectionRangeRef.current
      if (range != null) {
        const keys = displayItems.slice(range.start, range.end).map(node => node.key)
        setSelectedAssetIds(calculateNewKeys(event, keys, () => []))
      }
      setVisuallySelectedKeysOverride(null)
      dragSelectionRangeRef.current = null
    },
    [displayItems, calculateNewKeys, /* should never change */ setSelectedAssetIds]
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
      setSelectedAssetIds(calculateNewKeys(event, [key], getRange))
      setMostRecentlySelectedIndex(newIndex)
      if (!event.shiftKey) {
        selectionStartIndexRef.current = null
      }
    },
    [visibleItems, calculateNewKeys, /* should never change */ setSelectedAssetIds]
  )

  const table = (
    <div
      className="flex grow flex-col"
      onContextMenu={event => {
        event.preventDefault()
        event.stopPropagation()
        setModal(
          <AssetsTableContextMenu
            backend={backend}
            category={category}
            pasteData={pasteData}
            event={event}
            rootDirectoryId={rootDirectoryId}
            doPaste={doPaste}
          />
        )
      }}
      onDragEnter={onDropzoneDragOver}
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
      <FocusArea direction="vertical">
        {innerProps => (
          <table {...innerProps} className="table-fixed border-collapse rounded-rows">
            <thead>
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
            </thead>
            <tbody ref={bodyRef}>
              <AssetRows
                hideRoot
                columns={columns}
                item={rootDirectoryAsset}
                state={state}
                filter={filter}
                filterBy={CATEGORY_TO_FILTER_BY[category]}
                onClick={onRowClick}
              />
              <tr className="hidden h-row first:table-row">
                <td colSpan={columns.length} className="bg-transparent">
                  {category === Category.trash ? (
                    <aria.Text className="px-cell-x placeholder">
                      {query.query !== ''
                        ? getText('noFilesMatchTheCurrentFilters')
                        : getText('yourTrashIsEmpty')}
                    </aria.Text>
                  ) : category === Category.recent ? (
                    <aria.Text className="px-cell-x placeholder">
                      {query.query !== ''
                        ? getText('noFilesMatchTheCurrentFilters')
                        : getText('youHaveNoRecentProjects')}
                    </aria.Text>
                  ) : query.query !== '' ? (
                    <aria.Text className="px-cell-x placeholder">
                      {getText('noFilesMatchTheCurrentFilters')}
                    </aria.Text>
                  ) : (
                    <aria.Text className="px-cell-x placeholder">
                      {getText('youHaveNoFiles')}
                    </aria.Text>
                  )}
                </td>
              </tr>
            </tbody>
          </table>
        )}
      </FocusArea>
      <div
        className={tailwindMerge.twMerge(
          'sticky left grid max-w-container grow place-items-center',
          category !== Category.cloud && category !== Category.local && 'hidden'
        )}
        onDragEnter={onDropzoneDragOver}
        onDragOver={onDropzoneDragOver}
        onDrop={event => {
          const payload = drag.ASSET_ROWS.lookup(event)
          const filtered = payload?.filter(item => item.parentId !== rootDirectoryId)
          if (filtered != null && filtered.length > 0) {
            event.preventDefault()
            event.stopPropagation()
            unsetModal()
            for (const item of filtered) {
              updateAssetMutation.mutate([
                item.id,
                { parentDirectoryId: rootDirectoryId, description: null },
              ])
            }
          }
        }}
        onClick={() => {
          setSelectedAssetIds(backend.type, [])
        }}
      >
        <aria.FileTrigger
          onSelect={files => {
            if (files != null) {
              uploadFilesMutation.mutate([files, rootDirectoryId])
            }
          }}
        >
          <FocusRing>
            <aria.Button
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

  return (
    <div className="relative grow">
      <FocusArea direction="vertical">
        {innerProps => (
          <div
            {...aria.mergeProps<JSX.IntrinsicElements['div']>()(innerProps, {
              ref: rootRef,
              className: 'flex-1 overflow-auto container-size w-full h-full',
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
                          })}
                        >
                          {columnUtils.CLOUD_COLUMNS.filter(
                            column => !enabledColumns.includes(column)
                          ).map(column => (
                            <Button
                              key={column}
                              light
                              image={columnUtils.COLUMN_ICONS[column]}
                              alt={getText(columnUtils.COLUMN_SHOW_TEXT_ID[column])}
                              onPress={() => {
                                setEnabledColumns(currentColumns =>
                                  currentColumns.includes(column)
                                    ? currentColumns.filter(otherColumn => otherColumn !== column)
                                    : [...currentColumns, column]
                                )
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
      <div className="pointer-events-none absolute inset-0">
        <div
          data-testid="root-directory-dropzone"
          onDragEnter={onDropzoneDragOver}
          onDragOver={onDropzoneDragOver}
          onDragLeave={event => {
            if (event.currentTarget === event.target) {
              setIsDropzoneVisible(false)
            }
          }}
          onDrop={event => {
            setIsDropzoneVisible(false)
            if (event.dataTransfer.types.includes('Files')) {
              event.preventDefault()
              event.stopPropagation()
              uploadFilesMutation.mutate([event.dataTransfer.files, rootDirectoryId])
            }
          }}
          className={tailwindMerge.twMerge(
            'pointer-events-none sticky left-0 top-0 flex h-full w-full flex-col items-center justify-center gap-3 rounded-default bg-selected-frame text-primary/50 opacity-0 backdrop-blur-3xl transition-all',
            isDropzoneVisible && 'pointer-events-auto opacity-100'
          )}
        >
          <SvgMask src={DropFilesImage} className="size-[186px]" />
          {dropzoneText}
        </div>
      </div>
    </div>
  )
}
