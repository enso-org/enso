/** @file Table displaying a list of projects. */
import DropFilesImage from '#/assets/drop_files.svg'
import { FileTrigger, mergeProps } from '#/components/aria'
import { Button } from '#/components/Button'
import { ErrorDisplay } from '#/components/ErrorBoundary'
import { IsolateLayout } from '#/components/IsolateLayout'
import { SelectionBrush, type OnDragParams } from '#/components/SelectionBrush'
import { Text } from '#/components/Text'
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { useAutoScroll } from '#/hooks/autoScrollHooks'
import {
  backendMutationOptions,
  backendQueryOptions,
  listDirectoryQueryOptions,
  useListDirectoryRefetchInterval,
} from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { usePaste } from '#/hooks/cutAndPasteHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useCloseProject, useOpenProjectLocally } from '#/hooks/projectHooks'
import { useStore } from '#/hooks/storeHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import { useSetSuggestions } from '#/layouts/AssetSearchBar'
import AssetsTableContextMenu from '#/layouts/AssetsTableContextMenu'
import { type Category } from '#/layouts/CategorySwitcher/Category'
import { useAssetsTableItems } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import DragModal from '#/modals/DragModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import type { AssetRowInnerProps } from '#/pages/dashboard/components/AssetRow'
import { AssetRow } from '#/pages/dashboard/components/AssetRow'
import { INITIAL_ROW_STATE } from '#/pages/dashboard/components/AssetRow/assetRowUtils'
import { NameColumn } from '#/pages/dashboard/components/column'
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
  setDriveLocation,
  useDriveStore,
  useSetCanDownload,
  useSetNewestFolderId,
  useSetPasteData,
  useSetSelectedAssets,
  useSetVisuallySelectedKeys,
  type SelectedAssetInfo,
} from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { useLaunchedProjects } from '#/providers/ProjectsProvider'
import type Backend from '#/services/Backend'
import type { AssetId, DirectoryId, ProjectId } from '#/services/Backend'
import {
  assetIsProject,
  AssetType,
  BackendType,
  getAssetPermissionName,
  IS_OPENING_OR_OPENED,
  isAssetCredential,
  isDirectoryId,
  type AnyAsset,
} from '#/services/Backend'
import type { AssetQueryKey } from '#/utilities/AssetQuery'
import AssetQuery from '#/utilities/AssetQuery'
import { ASSET_ROWS, setDragImageToBlank, type AssetRowsDragPayload } from '#/utilities/drag'
import { isElementTextInput, isTextInputEvent } from '#/utilities/event'
import { fileExtension } from '#/utilities/fileInfo'
import { noop, noopPromise } from '#/utilities/functions'
import { DEFAULT_HANDLER } from '#/utilities/inputBindings'
import LocalStorage from '#/utilities/LocalStorage'
import { PermissionAction } from '#/utilities/permissions'
import { withPresence } from '#/utilities/set'
import type { SortInfo } from '#/utilities/sorting'
import { twMerge } from '#/utilities/tailwindMerge'
import { useMutationCallback } from '#/utilities/tanstackQuery'
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
  useRef,
  useState,
  type Dispatch,
  type DragEvent,
  type KeyboardEvent,
  type MouseEvent as ReactMouseEvent,
  type ReactNode,
  type RefObject,
  type SetStateAction,
} from 'react'
import { toast } from 'react-toastify'
import invariant from 'tiny-invariant'
import * as z from 'zod'
import type { AssetsDataTransferPayload } from './Drive/Categories/transferBetweenCategoriesHooks'
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
  readonly doPaste: (newParentKey: DirectoryId, newParentId: DirectoryId) => void
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
  readonly initialProjectName: string | null
}

/** The table of project assets. */
function AssetsTable(props: AssetsTableProps) {
  const { query, setQuery } = props
  const { initialProjectName } = props

  const { category, associatedBackend: backend } = useCategoriesAPI()
  const openedProjects = useLaunchedProjects()
  const openProjectLocally = useOpenProjectLocally()
  const setCanDownload = useSetCanDownload()
  const setSuggestions = useSetSuggestions()

  const { user } = useFullUserSession()
  const { reconnectToProjectManager } = useBackends()
  const didLoadingProjectManagerFail = useDidLoadingProjectManagerFail()
  const { data: labels } = useQuery(backendQueryOptions(backend, 'listTags', []))
  const localStorage = useLocalStorage()
  const { getText } = useText()
  const inputBindings = useInputBindings()
  const toastAndLog = useToastAndLog()
  const [enabledColumns, setEnabledColumns] = useState(DEFAULT_ENABLED_COLUMNS)
  const rightPanel = useRightPanelData()

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

  const uploadFiles = useUploadFiles(backend, category)
  const updateSecretMutation = useMutationCallback(backendMutationOptions(backend, 'updateSecret'))
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

  const { queryDirectoryId, currentDirectoryId } = useDirectoryIds({
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
      setDriveLocation(null, category.id)
      return false
    },
  })

  const { visibleItems } = useAssetsTableItems({
    parentId: currentDirectoryId,
    assets,
    sortInfo,
    query,
  })

  const isCloud = backend.type === BackendType.remote
  const rootRef = useRef<HTMLDivElement | null>(null)
  const mainDropzoneRef = useRef<HTMLButtonElement | null>(null)
  const headerRowRef = useRef<HTMLTableRowElement>(null)
  const getPasteData = useEventCallback(() => driveStore.getState().pasteData)

  useEffect(() => {
    setNewestFolderId(null)
  }, [category, setNewestFolderId])

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
    if (savedEnabledColumns != null) {
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

  const onKeyDown = useEventCallback((event: KeyboardEvent) => {
    const isTextInputFocused = isElementTextInput(document.activeElement)
    const isEventTextInputEvent =
      'key' in event && (isTextInputEvent(event) || event.key === 'Enter')
    const shouldIgnoreEvent = isTextInputFocused && isEventTextInputEvent
    if (shouldIgnoreEvent) {
      return
    }
    const { selectedAssets } = driveStore.getState()
    const prevIndex = mostRecentlySelectedIndexRef.current
    const item = prevIndex == null ? null : visibleItems[prevIndex]
    if (selectedAssets.length === 1 && item != null) {
      switch (event.key) {
        case 'Enter':
        case ' ': {
          if (event.key === ' ' && event.ctrlKey) {
            setSelectedAssets(
              selectedAssets.some((asset) => asset.id === item.id) ?
                selectedAssets.filter((asset) => asset.id !== item.id)
              : [...selectedAssets, item],
            )
          } else {
            switch (item.type) {
              case AssetType.directory: {
                event.preventDefault()
                event.stopPropagation()
                setDriveLocation(item.id, category.id)
                break
              }
              case AssetType.project: {
                event.preventDefault()
                event.stopPropagation()
                void openProjectLocally(item, backend.type)
                break
              }
              case AssetType.datalink: {
                event.preventDefault()
                event.stopPropagation()
                rightPanel.setTemporaryTab('settings')
                break
              }
              case AssetType.secret: {
                if (isAssetCredential(item)) {
                  toast.warning(getText('cannotEditCredentialError'))
                } else {
                  event.preventDefault()
                  event.stopPropagation()
                  const id = item.id
                  setModal(
                    <UpsertSecretModal
                      secretId={item.id}
                      name={item.title}
                      doCreate={async (title, value) => {
                        try {
                          await updateSecretMutation([id, { title, value }, item.title])
                        } catch (error) {
                          toastAndLog(null, error)
                        }
                      }}
                    />,
                  )
                }
                break
              }
              case AssetType.file:
              case AssetType.specialUp:
              default: {
                break
              }
            }
          }
          break
        }
      }
    }
    switch (event.key) {
      case ' ': {
        if (event.ctrlKey && item != null) {
          setSelectedAssets(
            selectedAssets.some((asset) => asset.id === item.id) ?
              selectedAssets.filter((asset) => asset.id !== item.id)
            : [...selectedAssets, item],
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
        const oldIndex = prevIndex ?? 0
        const index =
          event.key === 'ArrowUp' ?
            Math.max(0, oldIndex - 1)
          : Math.min(visibleItems.length - 1, oldIndex + 1)
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
          setSelectedAssets(selection)
        } else if (event.ctrlKey) {
          event.preventDefault()
          event.stopPropagation()
          selectionStartIndexRef.current = null
        } else if (index !== prevIndex) {
          event.preventDefault()
          event.stopPropagation()
          const newItem = visibleItems[index]
          if (newItem != null) {
            setSelectedAssets([newItem])
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

  const renameAssetMutationCallback = useMutationCallback(
    backendMutationOptions(backend, 'updateAsset'),
  )
  const closeProjectMutationCallback = useCloseProject()

  const doRenameAsset = useEventCallback((assetId: AssetId, newTitle: string) => {
    return renameAssetMutationCallback([
      assetId,
      { title: newTitle, parentDirectoryId: null, description: null },
      assetId,
    ])
  })

  const doOpenProject = useEventCallback((projectId: ProjectId) => {
    const project = assets.find((asset) => asset.id === projectId)

    if (project?.type !== AssetType.project) {
      return Promise.resolve()
    }

    return openProjectLocally(project, backend.type)
  })

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

  const doPaste = useEventCallback((newParentKey: DirectoryId, newParentId: DirectoryId) => {
    unsetModal()

    const { pasteData } = driveStore.getState()

    if (pasteData == null) {
      return
    }

    if (pasteData.data.assets.some((asset) => asset.id === newParentKey)) {
      toast.error('Cannot paste a folder into itself.')
      return
    }

    void paste({
      fromCategory: pasteData.data.category,
      toCategory: category,
      newParentId,
      pasteData: pasteData.data,
      method: pasteData.type,
    })

    setPasteData(null)
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
        doPaste={doPaste}
      />
    )

  const onDropzoneDragOver = (event: DragEvent<Element>) => {
    const payload = ASSET_ROWS.lookup(event)
    // Unconditionally handle drag event even if drop target is invalid
    // otherwise the drag modal stays around.
    if (payload || event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
      return
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
      doPaste,
      getAssetNodeById,
    }),
    [
      backend,
      category,
      currentDirectoryId,
      doCopy,
      doCut,
      doPaste,
      getAssetNodeById,
      hideColumn,
      query,
      setQuery,
      sortInfo,
    ],
  )

  const calculateNewSelection = useEventCallback(
    (
      event: MouseEvent | ReactMouseEvent,
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

  const grabRowKeyboardFocus = useEventCallback((item: AnyAsset) => {
    setSelectedAssets([item])
  })

  const onRowClick = useEventCallback(({ asset }: AssetRowInnerProps, event: ReactMouseEvent) => {
    event.stopPropagation()
    const newIndex = visibleItems.findIndex((otherAset) => otherAset.id === asset.id)
    const getRange = () => {
      if (mostRecentlySelectedIndexRef.current == null) {
        return [asset]
      } else {
        const index1 = mostRecentlySelectedIndexRef.current
        const index2 = newIndex
        const startIndex = Math.min(index1, index2)
        const endIndex = Math.max(index1, index2) + 1
        return visibleItems.slice(startIndex, endIndex)
      }
    }
    setSelectedAssets(calculateNewSelection(event, [asset], getRange))
    setMostRecentlySelectedIndex(newIndex)
    if (!event.shiftKey) {
      selectionStartIndexRef.current = null
    }
  })

  const selectRow = useEventCallback((asset: AnyAsset) => {
    setMostRecentlySelectedIndex(visibleItems.findIndex((otherAsset) => otherAsset.id === asset.id))
    selectionStartIndexRef.current = null
    setSelectedAssets([asset])
  })

  const onRowDragStart = useEventCallback(
    (event: DragEvent<HTMLTableRowElement>, asset: AnyAsset) => {
      startAutoScroll()

      onMouseEvent(event)

      let newSelectedKeys = driveStore.getState().selectedIds

      if (!newSelectedKeys.has(asset.id)) {
        setMostRecentlySelectedIndex(
          visibleItems.findIndex((otherAsset) => otherAsset.id === asset.id),
        )
        selectionStartIndexRef.current = null
        newSelectedKeys = new Set([asset.id])
        setSelectedAssets([asset])
      }
      const nodes = assets.filter((node) => newSelectedKeys.has(node.id))
      const isPayloadInvalid = nodes.some(
        (node) => node.type === AssetType.project && IS_OPENING_OR_OPENED[node.projectState.type],
      )
      if (isPayloadInvalid) {
        event.preventDefault()
        return
      }
      const payload: AssetRowsDragPayload = {
        category,
        items: nodes.map((node) => ({
          key: node.id,
          asset: node,
        })),
      }
      event.dataTransfer.setData(
        ASSETS_MIME_TYPE,
        JSON.stringify({
          category,
          items: nodes.map((node) => ({
            id: node.id,
            title: node.title,
            type: node.type,
            parentId: node.parentId,
            parentsPath: node.parentsPath,
            virtualParentsPath: node.virtualParentsPath,
          })),
        } satisfies AssetsDataTransferPayload),
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
              isNavigating={false}
              key={node.id}
              item={node}
              isOpened={false}
              backendType={backend.type}
              state={state}
              rowState={INITIAL_ROW_STATE}
              // The drag placeholder cannot be interacted with.
              isEditable={false}
              isPlaceholder={false}
              setSelected={noop}
              setRowState={noop}
              renameAsset={noopPromise}
              closeProject={noopPromise}
              openProject={noopPromise}
              labels={[]}
            />
          ))}
        </DragModal>,
      )
    },
  )

  const onRowDrop = useEventCallback(
    (event: DragEvent<HTMLElement>, item: AnyAsset | null = null) => {
      if (category.type === 'trash' || category.type === 'recent') {
        return
      }
      endAutoScroll()
      const directoryId = item?.type === AssetType.directory ? item.id : currentDirectoryId
      const payload = ASSET_ROWS.lookup(event)
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
      if (event.dataTransfer.types.includes('Files')) {
        event.preventDefault()
        event.stopPropagation()
        void uploadFiles(Array.from(event.dataTransfer.files), directoryId)
      }
    },
  )

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

  const itemRows = visibleItems.map((item) => {
    const isOpenedByYou = openedProjects.some(({ id }) => item.id === id)
    const isOpenedOnTheBackend =
      item.projectState?.type != null ? IS_OPENING_OR_OPENED[item.projectState.type] : false
    return (
      <AssetRow
        key={item.id + item.virtualParentsPath}
        isPlaceholder={false}
        isOpened={isOpenedByYou || isOpenedOnTheBackend}
        columns={columns}
        id={item.id}
        type={item.type}
        parentId={item.parentId}
        state={state}
        item={item}
        isKeyboardSelected={
          keyboardSelectedIndex != null && item === visibleItems[keyboardSelectedIndex]
        }
        grabKeyboardFocus={grabRowKeyboardFocus}
        onClick={onRowClick}
        select={selectRow}
        labels={labels ?? []}
        onDragStart={onRowDragStart}
        onDragEnd={endAutoScroll}
        onDrop={onRowDrop}
        renameAsset={doRenameAsset}
        closeProject={closeProjectMutationCallback}
        openProject={doOpenProject}
        tableRootRef={rootRef}
      />
    )
  })

  const specialEmptyText =
    query.query !== '' ? getText('noFilesMatchTheCurrentFilters')
    : currentDirectoryId !== category.homeDirectoryId ? getText('thisFolderIsEmpty')
    : null

  const table = (
    <div className="flex flex-none flex-col">
      <table className="isolate table-fixed border-collapse rounded-rows">
        <thead className="sticky top-0 isolate z-1 bg-dashboard before:absolute before:-inset-1 before:bottom-0 before:bg-dashboard">
          {headerRow}
        </thead>

        <tbody ref={bodyRef} className="isolate">
          {itemRows}
          <tr className="hidden h-row first:table-row">
            <td colSpan={columns.length} className="h-table-row bg-transparent">
              <Text className="px-cell-x placeholder" disableLineHeightCompensation>
                {category.type === 'trash' ?
                  (specialEmptyText ?? getText('yourTrashIsEmpty'))
                : category.type === 'recent' ?
                  (specialEmptyText ?? getText('youHaveNoRecentProjects'))
                : (specialEmptyText ?? getText('youHaveNoFiles'))}
              </Text>
            </td>
          </tr>
        </tbody>
      </table>

      <AssetsTableAssetsUnselector asChild>
        <div
          data-testid="root-directory-dropzone"
          className={twMerge(
            'sticky left-1 grid max-w-[calc(100cqw_-_0.5rem)] grow place-items-center pb-40 pt-20',
            (category.type === 'recent' || category.type === 'trash') && 'hidden',
          )}
          onDragEnter={onDropzoneDragOver}
          onDragOver={onDropzoneDragOver}
          onDrop={(event) => {
            event.preventDefault()
            event.stopPropagation()
            onRowDrop(event, null)
          }}
          onClick={() => {
            setSelectedAssets([])
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
              {getText('assetsDropzoneDescription')}
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
          <div
            className="inline-flex gap-icons"
            onFocus={() => {
              setKeyboardSelectedIndex(null)
            }}
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
        </div>
      )}

      <IsolateLayout className="isolate h-full w-full" useRAF>
        <div
          className="h-full w-full flex-1 scroll-p-24 overflow-auto scroll-smooth container-size"
          onKeyDown={onKeyDown}
          onBlur={(event) => {
            if (
              event.relatedTarget instanceof HTMLElement &&
              !event.currentTarget.contains(event.relatedTarget)
            ) {
              setKeyboardSelectedIndex(null)
            }
          }}
          ref={rootRef}
        >
          <SelectionBrush
            targetRef={rootRef}
            onDrag={onSelectionDrag}
            onDragEnd={onSelectionDragEnd}
            onDragCancel={onSelectionDragCancel}
            preventDrag={preventSelection}
          />
          <div
            className="flex h-max min-h-full w-max min-w-full flex-col"
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
                  doPaste={doPaste}
                />,
              )
            }}
          >
            <div
              className="flex h-full w-min min-w-full grow flex-col px-1"
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

    invariant(onlyChild != null, 'Children must be a single element when `asChild` is true')
    invariant(isValidElement(onlyChild), 'Children must be a JSX element when `asChild` is true')

    return cloneElement(
      onlyChild,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-explicit-any, no-restricted-syntax
      mergeProps<any>()(onlyChild.props as any, { onPointerDown }),
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
