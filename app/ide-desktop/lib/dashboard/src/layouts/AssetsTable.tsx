/** @file Table displaying a list of projects. */
import * as React from 'react'

import * as toast from 'react-toastify'
import * as tailwindMerge from 'tailwind-merge'

import DropFilesImage from 'enso-assets/drop_files.svg'

import * as mimeTypes from '#/data/mimeTypes'

import * as backendHooks from '#/hooks/backendHooks'
import * as scrollHooks from '#/hooks/scrollHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetPanel from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import type * as assetRow from '#/components/dashboard/AssetRow'
import AssetRow from '#/components/dashboard/AssetRow'
import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import AssetRows from '#/components/dashboard/AssetRows'
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
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as array from '#/utilities/array'
import type * as assetQuery from '#/utilities/AssetQuery'
import AssetQuery from '#/utilities/AssetQuery'
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
import type * as sorting from '#/utilities/sorting'
import Visibility from '#/utilities/Visibility'

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
    render: () => 'type:datalink',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['datalink'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['datalink'] }),
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

const CATEGORY_TO_FILTER_BY: Readonly<Record<Category, backendModule.FilterBy | null>> = {
  [Category.cloud]: backendModule.FilterBy.active,
  [Category.local]: backendModule.FilterBy.active,
  [Category.recent]: null,
  [Category.trash]: backendModule.FilterBy.trashed,
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  readonly backend: Backend
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly selectedKeys: React.MutableRefObject<ReadonlySet<backendModule.AssetId>>
  readonly scrollContainerRef: React.RefObject<HTMLElement>
  readonly visibilities: ReadonlyMap<backendModule.AssetId, Visibility>
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
  readonly nodeMap: Readonly<
    React.MutableRefObject<ReadonlyMap<backendModule.AssetId, backendModule.AnyAsset>>
  >
  readonly hideColumn: (column: columnUtils.Column) => void
  readonly doToggleDirectoryExpansion: (
    directoryId: backendModule.DirectoryId,
    title?: string | null,
    override?: boolean
  ) => void
  readonly doOpenEditor: (
    project: backendModule.ProjectAsset,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
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
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setProjectStartupInfo: (projectStartupInfo: backendModule.ProjectStartupInfo) => void
  readonly setCanDownload: (canDownload: boolean) => void
  readonly category: Category
  readonly setSuggestions: (suggestions: assetSearchBar.Suggestion[]) => void
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly targetDirectoryNodeRef: React.MutableRefObject<backendModule.DirectoryAsset | null>
  readonly doOpenEditor: (
    backend: Backend,
    project: backendModule.ProjectAsset,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
  const { hidden, query, setQuery, setProjectStartupInfo, setCanDownload, category } = props
  const { setSuggestions, projectStartupInfo } = props
  const { doOpenEditor: doOpenEditorRaw, doCloseEditor: doCloseEditorRaw } = props
  const { setAssetPanelProps, targetDirectoryNodeRef, setIsAssetPanelTemporarilyVisible } = props

  const { user } = authProvider.useNonPartialUserSession()
  const backend = backendProvider.useBackend(category)
  const labels = backendHooks.useBackendListTags(backend)
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [enabledColumns, setEnabledColumns] = localStorageProvider.useLocalStorageValue(
    'enabledColumns',
    () => columnUtils.DEFAULT_ENABLED_COLUMNS
  )
  const [sortInfo, setSortInfo] =
    React.useState<sorting.SortInfo<columnUtils.SortableColumn> | null>(null)
  const [selectedKeys, setSelectedKeysRaw] = React.useState<ReadonlySet<backendModule.AssetId>>(
    () => new Set()
  )
  const selectedKeysRef = React.useRef(selectedKeys)
  const [pasteData, setPasteData] = React.useState<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const rootDirectoryId = React.useMemo(
    () => backend.rootDirectoryId(user) ?? backendModule.DirectoryId(''),
    [backend, user]
  )
  const filterBy = CATEGORY_TO_FILTER_BY[category]
  const rootDirectoryEntries = backendHooks.useBackendListDirectory(
    backend,
    rootDirectoryId,
    '(root)',
    ...(filterBy == null ? [] : [filterBy])
  )
  const isLoading = rootDirectoryEntries == null
  const [isDropzoneVisible, setIsDropzoneVisible] = React.useState(false)
  const [droppedFilesCount, setDroppedFilesCount] = React.useState(0)
  const isCloud = backend.type === backendModule.BackendType.remote
  const rootRef = React.useRef<HTMLDivElement | null>(null)
  const headerRowRef = React.useRef<HTMLTableRowElement>(null)
  const pasteDataRef = React.useRef<pasteDataModule.PasteData<
    ReadonlySet<backendModule.AssetId>
  > | null>(null)
  const filter = React.useMemo(() => query.filter.bind(query), [query])

  const updateAssetMutation = backendHooks.useBackendMutation(backend, 'updateAsset')
  const copyAssetMutation = backendHooks.useBackendMutation(backend, 'copyAsset')
  const updateSecretMutation = backendHooks.useBackendMutation(backend, 'updateSecret')

  React.useEffect(() => {
    if (selectedKeys.size === 0) {
      targetDirectoryNodeRef.current = null
    } else if (selectedKeys.size === 1) {
      const [soleKey] = selectedKeys
      const node = soleKey == null ? null : nodeMapRef.current.get(soleKey)
      if (node != null && node.type === backendModule.AssetType.directory) {
        targetDirectoryNodeRef.current = node
      }
    } else {
      let commonDirectoryId: backendModule.AssetId | null = null
      let otherCandidateDirectoryId: backendModule.AssetId | null = null
      for (const key of selectedKeys) {
        const node = nodeMapRef.current.get(key)
        if (node != null) {
          if (commonDirectoryId == null) {
            commonDirectoryId = node.parentId
            otherCandidateDirectoryId =
              node.type === backendModule.AssetType.directory ? node.id : null
          } else if (node.id === commonDirectoryId || node.parentId === commonDirectoryId) {
            otherCandidateDirectoryId = null
          } else if (
            otherCandidateDirectoryId != null &&
            (node.id === otherCandidateDirectoryId || node.parentId === otherCandidateDirectoryId)
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
      const node = commonDirectoryId == null ? null : nodeMapRef.current.get(commonDirectoryId)
      if (node != null && node.type === backendModule.AssetType.directory) {
        targetDirectoryNodeRef.current = node
      }
    }
  }, [targetDirectoryNodeRef, selectedKeys])

  React.useEffect(() => {
    const nodeToSuggestion = (
      node: backendModule.AnyAsset,
      key: assetQuery.AssetQueryKey = 'names'
    ): assetSearchBar.Suggestion => ({
      render: () => `${key === 'names' ? '' : '-:'}${node.title}`,
      addToQuery: oldQuery => oldQuery.addToLastTerm({ [key]: [node.title] }),
      deleteFromQuery: oldQuery => oldQuery.deleteFromLastTerm({ [key]: [node.title] }),
    })
    // TODO: switch to React Query to fetch all known nodes
    const allVisibleNodes = () =>
      rootDirectoryEntries
        .preorderTraversal(children =>
          children.filter(child => visibilities.get(child.key) !== Visibility.hidden)
        )
        .filter(
          node =>
            visibilities.get(node.key) === Visibility.visible &&
            node.type !== backendModule.AssetType.specialEmpty &&
            node.type !== backendModule.AssetType.specialLoading
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
            .filter(node => node.type === backendModule.AssetType.file)
            .map(node => fileInfo.fileExtension(node.title))
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
          const modifieds = rootDirectoryEntries.preorderTraversal().map(node => {
            const date = new Date(node.modifiedAt)
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
          const owners = rootDirectoryEntries
            .preorderTraversal()
            .flatMap(node =>
              (node.permissions ?? [])
                .filter(permission => permission.permission === permissions.PermissionAction.own)
                .map(backendModule.getAssetPermissionName)
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
            (labels ?? []).map(
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
  }, [
    isCloud,
    rootDirectoryEntries,
    query,
    visibilities,
    labels,
    /* should never change */ setSuggestions,
  ])

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

  const setSelectedKeys = React.useCallback(
    (newSelectedKeys: ReadonlySet<backendModule.AssetId>) => {
      selectedKeysRef.current = newSelectedKeys
      setSelectedKeysRaw(newSelectedKeys)
      if (!isCloud) {
        setCanDownload(
          newSelectedKeys.size !== 0 &&
            Array.from(newSelectedKeys).every(key => {
              const node = nodeMapRef.current.get(key)
              return node?.type === backendModule.AssetType.project
            })
        )
      } else {
        setCanDownload(
          newSelectedKeys.size !== 0 &&
            Array.from(newSelectedKeys).every(key => {
              const node = nodeMapRef.current.get(key)
              return (
                node?.type === backendModule.AssetType.project ||
                node?.type === backendModule.AssetType.file ||
                node?.type === backendModule.AssetType.datalink
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
            setSelectedKeys(set.withPresence(keys, item.id, !keys.has(item.id)))
          } else {
            switch (item.type) {
              case backendModule.AssetType.directory: {
                event.preventDefault()
                event.stopPropagation()
                doToggleDirectoryExpansion(item.id, item.id)
                break
              }
              case backendModule.AssetType.project: {
                event.preventDefault()
                event.stopPropagation()
                openProjectMutation
                dispatchAssetEvent({
                  type: AssetEventType.openProject,
                  id: item.id,
                  runInBackground: false,
                  shouldAutomaticallySwitchPage: true,
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
                const id = item.id
                setModal(
                  <UpsertSecretModal
                    id={item.id}
                    name={item.title}
                    doCreate={async (_name, value) => {
                      try {
                        await updateSecretMutation.mutateAsync([id, { value }, item.title])
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
          if (item.type === backendModule.AssetType.directory && item.children != null) {
            // The folder is expanded; collapse it.
            event.preventDefault()
            event.stopPropagation()
            doToggleDirectoryExpansion(item.id, null, false)
          } else {
            // Focus parent if there is one.
            const index2 = allRows.findIndex(row => row.id === item.parentId)
            if (index2 !== -1) {
              event.preventDefault()
              event.stopPropagation()
              setSelectedKeys(new Set([item.parentId]))
              setMostRecentlySelectedIndex(index2, true)
              //
            }
          }
          break
        }
        case 'ArrowRight': {
          if (item.type === backendModule.AssetType.directory && item.children == null) {
            // The folder is collapsed; expand it.
            event.preventDefault()
            event.stopPropagation()
            doToggleDirectoryExpansion(item.id, null, true)
          }
          break
        }
      }
    }
    switch (event.key) {
      case ' ': {
        if (event.ctrlKey && item != null) {
          const keys = selectedKeysRef.current
          setSelectedKeys(set.withPresence(keys, item.id, !keys.has(item.id)))
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
          setSelectedKeys(new Set(selection.map(newItem => newitem.id)))
        } else if (event.ctrlKey) {
          event.preventDefault()
          event.stopPropagation()
          selectionStartIndexRef.current = null
        } else if (index !== prevIndex) {
          event.preventDefault()
          event.stopPropagation()
          const newItem = visibleItems[index]
          if (newItem != null) {
            setSelectedKeys(new Set([newitem.id]))
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

  const doOpenEditor = React.useCallback(
    (
      project: backendModule.ProjectAsset,
      setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
      switchPage: boolean
    ) => {
      doOpenEditorRaw(backend, project, setProject, switchPage)
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
    setSelectedKeys(new Set())
  }, [pasteData, setSelectedKeys, /* should never change */ unsetModal])

  const doPaste = React.useCallback(
    (newParentKey: backendModule.DirectoryId, newParentId: backendModule.DirectoryId) => {
      unsetModal()
      if (pasteData != null) {
        if (pasteData.data.has(newParentKey)) {
          toast.toast.error('Cannot paste a folder into itself.')
        } else {
          doToggleDirectoryExpansion(newParentId, null, true)
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
    [pasteData, doToggleDirectoryExpansion, /* should never change */ unsetModal]
  )

  const hideColumn = React.useCallback(
    (column: columnUtils.Column) => {
      setEnabledColumns(columns => columns.filter(otherColumn => otherColumn !== column))
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
        selectedKeys={selectedKeys}
        clearSelectedKeys={clearSelectedKeys}
        nodeMapRef={nodeMapRef}
        rootDirectoryId={rootDirectoryId}
        event={{ pageX: 0, pageY: 0 }}
        doCopy={doCopy}
        doCut={doCut}
        doPaste={doPaste}
      />
    ),
    [
      backend,
      rootDirectoryId,
      category,
      selectedKeys,
      pasteData,
      doCopy,
      doCut,
      doPaste,
      /* should never change */ clearSelectedKeys,
    ]
  )

  const onDropzoneDragOver = (event: React.DragEvent<Element>) => {
    const payload = drag.ASSET_ROWS.lookup(event)
    const filtered = payload?.filter(item => item.asset.parentId !== rootDirectoryId)
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
      visibilities,
      selectedKeys: selectedKeysRef,
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
      nodeMap: nodeMapRef,
      hideColumn,
      doToggleDirectoryExpansion,
      doOpenEditor,
      doCloseEditor,
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
    <AssetRows
      hideRoot
      columns={columns}
      item={item}
      state={state}
      hidden={hidden || visibilities.get(item.id) === Visibility.hidden}
      selected={isSelected}
      setSelected={selected => {
        setSelectedKeys(set.withPresence(selectedKeysRef.current, item.id, selected))
      }}
      isSoleSelected={isSoleSelected}
      isKeyboardSelected={
        keyboardSelectedIndex != null && item === visibleItems[keyboardSelectedIndex]
      }
      grabKeyboardFocus={() => {
        setSelectedKeys(new Set([item.id]))
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
          setSelectedKeys(new Set([item.id]))
        }
      }}
      onDragStart={event => {
        let newSelectedKeys = selectedKeysRef.current
        if (!newSelectedKeys.has(item.id)) {
          setMostRecentlySelectedIndex(visibleItems.indexOf(item))
          selectionStartIndexRef.current = null
          newSelectedKeys = new Set([item.id])
          setSelectedKeys(newSelectedKeys)
        }
        const nodes = rootDirectoryEntries
          .preorderTraversal()
          .filter(node => newSelectedKeys.has(node.id))
        const payload: drag.AssetRowsDragPayload = nodes.map(node => ({
          key: node.key,
          asset: node,
        }))
        event.dataTransfer.setData(
          mimeTypes.ASSETS_MIME_TYPE,
          JSON.stringify(nodes.map(node => node.id))
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
                key={node.id}
                keyProp={node.id}
                item={node}
                state={state}
                // Default states.
                depth={0}
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
            selectedKeysRef.current.has(item.id) ? selectedKeysRef.current : [item.id]
          )
          // Expand ids to include ids of children as well.
          for (const node of rootDirectoryEntries.preorderTraversal()) {
            if (ids.has(node.id) && node.children != null) {
              for (const child of node.children) {
                ids.add(child.id)
              }
            }
          }
          let labelsPresent = 0
          for (const selectedKey of ids) {
            const nodeLabels = nodeMapRef.current.get(selectedKey)?.labels
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
        const ids = new Set(
          selectedKeysRef.current.has(item.id) ? selectedKeysRef.current : [item.id]
        )
        // Expand ids to include ids of descendants as well.
        for (const node of rootDirectoryEntries.preorderTraversal()) {
          if (ids.has(node.id) && node.children != null) {
            for (const child of node.children) {
              ids.add(child.id)
            }
          }
        }
        const payload = drag.LABELS.lookup(event)
        if (payload != null) {
          event.preventDefault()
          event.stopPropagation()
          let labelsPresent = 0
          for (const selectedKey of ids) {
            const nodeLabels = nodeMapRef.current.get(selectedKey)?.labels
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
            labelNames: set.EMPTY,
          })
        }
      }}
    />
  )

  const dropzoneText = isDropzoneVisible
    ? droppedFilesCount === 1
      ? getText('assetsDropFileDescription')
      : getText('assetsDropFilesDescription', droppedFilesCount)
    : getText('assetsDropzoneDescription')

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
            selectedKeys={selectedKeys}
            clearSelectedKeys={clearSelectedKeys}
            nodeMapRef={nodeMapRef}
            event={event}
            rootDirectoryId={rootDirectoryId}
            doCopy={doCopy}
            doCut={doCut}
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
      <table className="table-fixed border-collapse rounded-rows">
        <thead>{headerRow}</thead>
        <tbody ref={bodyRef}>
          {itemRows}
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
                <aria.Text className="px-cell-x placeholder">{getText('youHaveNoFiles')}</aria.Text>
              )}
            </td>
          </tr>
        </tbody>
      </table>
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
                item.title,
              ])
            }
          }
        }}
        onClick={() => {
          setSelectedKeys(new Set())
        }}
      >
        <aria.FileTrigger
          onSelect={event => {
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
                            onFocus: () => {
                              setKeyboardSelectedIndex(null)
                            },
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
              dispatchAssetListEvent({
                type: AssetListEventType.uploadFiles,
                parentKey: rootDirectoryId,
                parentId: rootDirectoryId,
                files: Array.from(event.dataTransfer.files),
              })
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
