/** @file Table displaying a list of projects. */
import DropFilesImage from '#/assets/drop_files.svg'
import { FileTrigger, mergeProps } from '#/components/aria'
import { Button } from '#/components/Button'
import type { ContextMenuApi } from '#/components/ContextMenu'
import { IsolateLayout } from '#/components/IsolateLayout'
import { Scroller } from '#/components/Scroller'
import { SelectionBrush, type OnDragParams } from '#/components/SelectionBrush'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import { Text } from '#/components/Text'
import { UserWithPopover } from '#/components/UserWithPopover'
import { ASSETS_MIME_TYPE } from '#/data/mimeTypes'
import { useAutoScroll } from '#/hooks/autoScrollHooks'
import {
  backendMutationOptions,
  backendQueryOptions,
  listDirectoryQueryOptions,
  searchDirectoryQueryOptions,
  useListDirectoryRefetchInterval,
} from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { usePaste } from '#/hooks/cutAndPasteHooks'
import { useDerivedDebouncedState } from '#/hooks/debounceCallbackHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useStore } from '#/hooks/storeHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import { useSetSuggestions } from '#/layouts/AssetSearchBar'
import { AssetsTableCombinedContextMenu } from '#/layouts/AssetsTableCombinedContextMenu'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useAssetsTableItems } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import DragModal from '#/modals/DragModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import AssetIcon from '#/pages/dashboard/components/AssetIcon'
import type { AssetRowInnerProps } from '#/pages/dashboard/components/AssetRow'
import { AssetRow } from '#/pages/dashboard/components/AssetRow'
import {
  Column,
  COLUMN_CSS_CLASS,
  COLUMN_ICONS,
  DEFAULT_ENABLED_COLUMNS,
  getColumnList,
} from '#/pages/dashboard/components/column/columnUtils'
import { COLUMN_HEADING } from '#/pages/dashboard/components/columnHeading'
import Label from '#/pages/dashboard/components/Label'
import { BindingFocusScopeContext } from '#/providers/BindingFocusScopeProvider'
import {
  setDriveLocation,
  useDriveStore,
  useSetAssetToRename,
  useSetCanDownload,
  useSetPasteData,
  useSetSelectedAssets,
  useSetVisuallySelectedKeys,
  type SelectedAssetInfo,
} from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import AssetQuery from '#/utilities/AssetQuery'
import { ASSET_ROWS, setDragImageToBlank, type AssetRowsDragPayload } from '#/utilities/drag'
import { isElementTextInput, isTextInputEvent } from '#/utilities/event'
import { DEFAULT_HANDLER } from '#/utilities/inputBindings'
import LocalStorage from '#/utilities/LocalStorage'
import { withPresence } from '#/utilities/set'
import type { SortInfo } from '#/utilities/sorting'
import { twMerge } from '#/utilities/tailwindMerge'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useFullUserSession, useLocalStorage, useText } from '$/providers/react'
import { useRightPanelData } from '$/providers/react/container'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { useOpenedProjects } from '$/providers/react/openedProjects'
import { useInfiniteQuery, useQuery } from '@tanstack/react-query'
import type {
  AssetId,
  AssetSortExpression,
  Backend,
  DirectoryId,
  PaginationToken,
} from 'enso-common/src/services/Backend'
import {
  AssetType,
  BackendType,
  IS_OPENING_OR_OPENED,
  isAssetCredential,
  LabelName,
  type AnyAsset,
} from 'enso-common/src/services/Backend'
import {
  userGroupIdToDirectoryId,
  userIdToDirectoryId,
} from 'enso-common/src/services/RemoteBackend/ids'
import { fileExtension } from 'enso-common/src/utilities/file'
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
  type HTMLAttributes,
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
import { SUGGESTIONS_FOR_TYPE } from './Drive/suggestionsConstants'

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
  readonly scrollContainerRef: RefObject<HTMLElement>
  readonly category: Category
  readonly sortInfo: SortInfo<AssetSortExpression> | null
  readonly setSortInfo: (sortInfo: SortInfo<AssetSortExpression> | null) => void
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
  readonly hideColumn: (column: Column) => void
  readonly getAssetNodeById: (id: AssetId) => AnyAsset | null
}

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
}

/** The table of project assets. */
function AssetsTable(props: AssetsTableProps) {
  const { query, setQuery } = props

  const contextMenuRef = useRef<ContextMenuApi>(null)
  const { category, associatedBackend: backend } = useCategoriesAPI()
  const { openProjectLocally } = useOpenedProjects()
  const setCanDownload = useSetCanDownload()
  const setSuggestions = useSetSuggestions()

  const { user } = useFullUserSession()
  const { data: labels } = useQuery(backendQueryOptions(backend, 'listTags', []))
  const localStorage = useLocalStorage()
  const { getText } = useText()
  const inputBindings = useInputBindings()
  const toastAndLog = useToastAndLog()
  const [enabledColumns, setEnabledColumns] = useState(DEFAULT_ENABLED_COLUMNS)
  const rightPanel = useRightPanelData()

  const allowedColumns = useMemo(
    () => getColumnList(user.plan, backend.type, category, query.query !== ''),
    [user.plan, backend.type, category, query.query],
  )

  const columns = useMemo(
    () => allowedColumns.filter((column) => enabledColumns.has(column)),
    [allowedColumns, enabledColumns],
  )

  const hiddenColumns = useMemo(
    () => allowedColumns.filter((column) => !enabledColumns.has(column)),
    [allowedColumns, enabledColumns],
  )

  const [sortInfo, setSortInfo] = useState<SortInfo<AssetSortExpression> | null>(null)
  const driveStore = useDriveStore()
  const setNewestFolderId = useSetAssetToRename()
  const setSelectedAssets = useSetSelectedAssets()
  const setVisuallySelectedKeys = useSetVisuallySelectedKeys()
  const setPasteData = useSetPasteData()

  const uploadFiles = useUploadFiles(backend, category)
  const updateSecretMutation = useMutationCallback(backendMutationOptions(backend, 'updateSecret'))
  const paste = usePaste(category)

  const { data: users } = useQuery(backendQueryOptions(backend, 'listUsers', []))
  const { data: userGroups } = useQuery(backendQueryOptions(backend, 'listUserGroups', []))

  const { queryDirectoryId: queryDirectoryIdRaw, currentDirectoryId } = useDirectoryIds({
    category,
  })
  const ownerLower = query.owners[0]?.toLowerCase()
  const queryDirectoryId = (() => {
    if (ownerLower == null) {
      return queryDirectoryIdRaw
    }
    const userId = users?.find((otherUser) =>
      otherUser.name.toLowerCase().includes(ownerLower),
    )?.userId
    if (userId != null) {
      return userIdToDirectoryId(userId)
    }
    const userGroupId = userGroups?.find((userGroup) =>
      userGroup.groupName.toLowerCase().includes(ownerLower),
    )?.id
    if (userGroupId != null) {
      return userGroupIdToDirectoryId(userGroupId)
    }
    return queryDirectoryIdRaw
  })()
  const queryDirectoryIdRef = useSyncRef(queryDirectoryId)
  const listDirectoryRefetchInterval = useListDirectoryRefetchInterval()
  const debouncedQueryDelayMs = useFeatureFlag('dataCatalogQueryDebounceDelay')
  const debouncedQuery = useDerivedDebouncedState(query, debouncedQueryDelayMs)
  const directoryQueryOptions =
    debouncedQuery.query === '' ?
      listDirectoryQueryOptions({
        infinite: true,
        backend,
        parentId: queryDirectoryId,
        category,
        refetchInterval: listDirectoryRefetchInterval,
        labels: debouncedQuery.labels.length !== 0 ? debouncedQuery.labels.map(LabelName) : null,
        sortExpression: sortInfo?.field ?? null,
        sortDirection: sortInfo?.direction ?? null,
      })
    : searchDirectoryQueryOptions({
        infinite: true,
        backend,
        parentId: queryDirectoryId,
        // The `query` parameter is not supported.
        query: null,
        title:
          debouncedQuery.keywords[0] != null ?
            debouncedQuery.keywords.join(' ')
          : (debouncedQuery.names[0] ?? null),
        extension: debouncedQuery.extensions[0] ?? null,
        description: debouncedQuery.descriptions[0] ?? null,
        type: debouncedQuery.types[0] ?? null,
        labels: debouncedQuery.labels.length !== 0 ? debouncedQuery.labels.map(LabelName) : null,
        sortExpression: sortInfo?.field ?? null,
        sortDirection: sortInfo?.direction ?? null,
      })
  const pageSize = useFeatureFlag('listDirectoryPageSize')
  const assetsPages = useInfiniteQuery({
    ...directoryQueryOptions,
    queryKey: directoryQueryOptions.queryKey,
    queryFn: (context) =>
      directoryQueryOptions.queryFn(context, {
        from: context.pageParam,
        pageSize,
      }),
    initialPageParam: ((): PaginationToken | null => null)(),
    getNextPageParam: (lastPage) =>
      lastPage.assets.length === pageSize && category.type !== 'recent' ?
        lastPage.paginationToken
      : null,
    retry: () => {
      if (queryDirectoryId === queryDirectoryIdRef.current) {
        setDriveLocation(null, category.id)
      }
      return false
    },
  })
  const assets = useMemo(
    () => assetsPages.data?.pages.flatMap((page) => page.assets) ?? [],
    [assetsPages.data?.pages],
  )
  const fetchNextAssetPage = assetsPages.fetchNextPage
  const isFetching = assetsPages.isFetching

  const isCloud = backend.type === BackendType.remote
  const rootRef = useRef<HTMLDivElement | null>(null)
  const scrollerRef = useRef<HTMLDivElement | null>(null)
  const getPasteData = useEventCallback(() => driveStore.getState().pasteData)

  useEffect(() => {
    // Do not request next page while refetching. This causes data not being updated.
    // See https://github.com/TanStack/query/discussions/6709#discussioncomment-8142957
    if (isFetching) return
    const scrollerEl = scrollerRef.current
    if (!scrollerEl) return
    const tableEl = scrollerEl.children[0]
    if (!tableEl) return
    if (scrollerEl.scrollTop + scrollerEl.clientHeight >= tableEl.scrollHeight) {
      void fetchNextAssetPage()
    }
  }, [isFetching, fetchNextAssetPage, assetsPages.data?.pages])

  useAssetsTableItems({ parentId: currentDirectoryId, assets })

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
    const allVisible = () =>
      assets.map(
        (node): assetSearchBar.Suggestion => ({
          key: node.id,
          render: () => node.title,
          addToQuery: (oldQuery) => oldQuery.add('names', [node.title]),
          deleteFromQuery: (oldQuery) => oldQuery.delete('names', [node.title]),
        }),
      )

    const terms = AssetQuery.terms(query.query)
    const term = terms.find((otherTerm) => otherTerm.values.length === 0) ?? terms[terms.length - 1]
    const termValues = term?.values ?? []
    const shouldOmitNames = terms.some((otherTerm) => otherTerm.tag === 'name')

    if (termValues.length !== 0) {
      setSuggestions(shouldOmitNames ? [] : allVisible())
    } else {
      switch (term?.tag ?? null) {
        case null:
        case '':
        case 'name': {
          setSuggestions(allVisible())
          break
        }
        case 'type': {
          setSuggestions(SUGGESTIONS_FOR_TYPE)
          break
        }
        case 'ext':
        case 'extension': {
          const extensions = assets
            .filter((node) => node.type === AssetType.file)
            .map((node) => fileExtension(node.title))
          setSuggestions(
            Array.from(
              new Set(extensions),
              (extension): assetSearchBar.Suggestion => ({
                key: extension,
                render: () => AssetQuery.termToString({ tag: 'extension', values: [extension] }),
                addToQuery: (oldQuery) => oldQuery.add('extensions', [extension]),
                deleteFromQuery: (oldQuery) => oldQuery.delete('extensions', [extension]),
              }),
            ),
          )
          break
        }
        case 'modified': {
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
                    tag: 'modified',
                    values: [modified],
                  }),
                addToQuery: (oldQuery) => oldQuery.add('modifieds', [modified]),
                deleteFromQuery: (oldQuery) => oldQuery.delete('modifieds', [modified]),
              }),
            ),
          )
          break
        }
        case 'label': {
          setSuggestions(
            (labels ?? []).map(
              (label): assetSearchBar.Suggestion => ({
                key: label.value,
                render: () => (
                  <Label active color={label.color} onPress={() => {}}>
                    {label.value}
                  </Label>
                ),
                addToQuery: (oldQuery) => oldQuery.add('labels', [label.value]),
                deleteFromQuery: (oldQuery) => oldQuery.delete('labels', [label.value]),
              }),
            ),
          )
          break
        }
        case 'owner': {
          setSuggestions([
            ...(users ?? []).map(
              (otherUser): assetSearchBar.Suggestion => ({
                key: otherUser.userId,
                render: () => <UserWithPopover user={otherUser} />,
                addToQuery: (oldQuery) => oldQuery.add('owners', [otherUser.name]),
                deleteFromQuery: (oldQuery) => oldQuery.delete('owners', [otherUser.name]),
              }),
            ),
            ...(userGroups ?? []).map(
              (userGroup): assetSearchBar.Suggestion => ({
                key: userGroup.id,
                render: () =>
                  AssetQuery.termToString({ tag: 'owner', values: [userGroup.groupName] }),
                addToQuery: (oldQuery) => oldQuery.add('owners', [userGroup.groupName]),
                deleteFromQuery: (oldQuery) => oldQuery.delete('owners', [userGroup.groupName]),
              }),
            ),
          ])
          break
        }
        default: {
          setSuggestions(shouldOmitNames ? [] : allVisible())
          break
        }
      }
    }
  }, [isCloud, query, labels, setSuggestions, assets, users, userGroups])

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
        const predicate = (type: AssetType | undefined) =>
          type === AssetType.directory ||
          type === AssetType.project ||
          type === AssetType.file ||
          type === AssetType.datalink
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
    const item = prevIndex == null ? null : assets[prevIndex]
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
          : Math.min(assets.length - 1, oldIndex + 1)
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
          const selection = assets.slice(startIndex, endIndex)
          setSelectedAssets(selection)
        } else if (event.ctrlKey) {
          event.preventDefault()
          event.stopPropagation()
          selectionStartIndexRef.current = null
        } else if (index !== prevIndex) {
          event.preventDefault()
          event.stopPropagation()
          const newItem = assets[index]
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

  const doCopy = useEventCallback(() => {
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

  const doPaste = useEventCallback((newParentId: DirectoryId) => {
    const { pasteData } = driveStore.getState()
    if (pasteData == null) return
    if (pasteData.data.assets.some((asset) => asset.id === newParentId)) {
      if (pasteData.data.assets[0] && pasteData.data.assets.length === 1) {
        // The folder is the only thing selected.
        // Instead of pasting into itself, instead we paste into its parent.
        newParentId = pasteData.data.assets[0].parentId
      } else {
        toast.error('Cannot paste a folder into itself.')
        return
      }
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
    (): AssetsTableState => ({
      backend,
      scrollContainerRef: rootRef,
      category,
      sortInfo,
      setSortInfo,
      query,
      setQuery,
      hideColumn,
      getAssetNodeById,
    }),
    [backend, category, getAssetNodeById, hideColumn, query, setQuery, sortInfo],
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
          const otherAssets = assets.slice(range.start, range.end).map((node) => node)
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
      const otherAssets = assets.slice(range.start, range.end).map((node) => node)
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
    const newIndex = assets.findIndex((otherAset) => otherAset.id === asset.id)
    const getRange = () => {
      if (mostRecentlySelectedIndexRef.current == null) {
        return [asset]
      } else {
        const index1 = mostRecentlySelectedIndexRef.current
        const index2 = newIndex
        const startIndex = Math.min(index1, index2)
        const endIndex = Math.max(index1, index2) + 1
        return assets.slice(startIndex, endIndex)
      }
    }
    setSelectedAssets(calculateNewSelection(event, [asset], getRange))
    setMostRecentlySelectedIndex(newIndex)
    if (!event.shiftKey) {
      selectionStartIndexRef.current = null
    }
  })

  const selectRow = useEventCallback((asset: AnyAsset) => {
    setMostRecentlySelectedIndex(assets.findIndex((otherAsset) => otherAsset.id === asset.id))
    selectionStartIndexRef.current = null
    setSelectedAssets([asset])
  })

  const onRowDragStart = useEventCallback(
    (event: DragEvent<HTMLTableRowElement>, asset: AnyAsset) => {
      startAutoScroll()

      onMouseEvent(event)

      let newSelectedKeys = driveStore.getState().selectedIds

      if (!newSelectedKeys.has(asset.id)) {
        setMostRecentlySelectedIndex(assets.findIndex((otherAsset) => otherAsset.id === asset.id))
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
          {nodes.map((otherAsset) => (
            <div key={otherAsset.id} className="flex h-[34px] items-center gap-2 px-2">
              <AssetIcon asset={otherAsset} />
              <Text>{otherAsset.title}</Text>
            </div>
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

  const specialEmptyText =
    query.query !== '' ? getText('noFilesMatchTheCurrentFilters')
    : currentDirectoryId !== category.homeDirectoryId ? getText('thisFolderIsEmpty')
    : null

  const table = (
    <Scroller
      ref={scrollerRef}
      scrollbar
      fullSize
      orientation="vertical"
      className="h-full flex-1"
      shadowStartClassName="top-8"
      onScroll={(event) => {
        // Do not request next page while refetching. This causes data not being updated.
        // See https://github.com/TanStack/query/discussions/6709#discussioncomment-8142957
        if (isFetching) return
        const element = event.currentTarget
        const tableEl = element.children[0]
        if (!tableEl) return
        if (element.scrollTop + element.clientHeight >= tableEl.scrollHeight) {
          void assetsPages.fetchNextPage()
        }
      }}
    >
      {/* The `max-w-[calc(100cqw_-_0.5rem)]` is necessary otherwise it shifts slightly
       * when scrolling horizontally. */}
      <table className="isolate max-w-[calc(100cqw_-_0.5rem)] table-fixed border-collapse rounded-rows">
        <thead className="sticky top-0 isolate z-1 bg-dashboard before:absolute before:-inset-1 before:bottom-0 before:bg-dashboard">
          <tr className="rounded-none text-sm font-semibold">
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
        </thead>

        <tbody ref={bodyRef} className="isolate">
          {assets.map((item) => {
            return (
              <AssetRow
                key={item.id + item.virtualParentsPath}
                contextMenuRef={contextMenuRef}
                isPlaceholder={false}
                columns={columns}
                id={item.id}
                type={item.type}
                parentId={item.parentId}
                state={state}
                item={item}
                isKeyboardSelected={
                  keyboardSelectedIndex != null && item === assets[keyboardSelectedIndex]
                }
                grabKeyboardFocus={grabRowKeyboardFocus}
                onClick={onRowClick}
                select={selectRow}
                labels={labels ?? []}
                onDragStart={onRowDragStart}
                onDragEnd={endAutoScroll}
                onDrop={onRowDrop}
              />
            )
          })}
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
          {(assetsPages.isLoading || assetsPages.isFetchingNextPage) && (
            <tr className="h-row">
              <td colSpan={columns.length} className="rounded-full bg-transparent">
                <div className="flex justify-center">
                  <StatelessSpinner size={32} phase="loading-medium" />
                </div>
              </td>
            </tr>
          )}
        </tbody>
      </table>
      <AssetsTableAssetsUnselector asChild>
        <div
          data-testid="root-directory-dropzone"
          // The `max-w-[calc(100cqw_-_0.5rem)]` is necessary otherwise it shifts slightly
          // when scrolling horizontally.
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
              icon={DropFilesImage}
              className="rounded-2xl"
              contentClassName="h-[186px] flex flex-col items-center gap-3 text-primary/30 transition-colors duration-200 hover:text-primary/50"
            >
              {getText('assetsDropzoneDescription')}
            </Button>
          </FileTrigger>
        </div>
      </AssetsTableAssetsUnselector>
    </Scroller>
  )

  return (
    <BindingFocusScopeContext.Provider value={rootRef}>
      <div className="relative grow contain-strict">
        <AssetsTableCombinedContextMenu
          ref={contextMenuRef}
          currentDirectoryId={currentDirectoryId}
          doCopy={doCopy}
          doCut={doCut}
          doPaste={doPaste}
        />

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

        <IsolateLayout className="isolate h-full w-full">
          <div
            tabIndex={-1}
            className="h-full w-full flex-1 container-size"
            onKeyDown={onKeyDown}
            onBlur={(event) => {
              if (
                event.relatedTarget instanceof HTMLElement &&
                !event.currentTarget.contains(event.relatedTarget)
              ) {
                setKeyboardSelectedIndex(null)
              }
            }}
            ref={(el) => {
              rootRef.current = el
              if (document.activeElement === document.body) {
                el?.focus()
              }
            }}
          >
            <div
              className="flex h-full w-full min-w-full flex-col"
              onContextMenu={(event) => {
                if (
                  event.target instanceof HTMLElement &&
                  event.target.dataset.testid === 'underlay'
                ) {
                  return
                }
                event.preventDefault()
                event.stopPropagation()
                contextMenuRef.current?.open(event)
              }}
            >
              <div
                className="flex h-full w-min min-w-full grow flex-col px-1"
                onDrop={(event) => {
                  onRowDrop(event, null)
                }}
              >
                {table}
              </div>
            </div>
          </div>
        </IsolateLayout>
        <SelectionBrush
          targetRef={rootRef}
          onDrag={onSelectionDrag}
          onDragEnd={onSelectionDragEnd}
          onDragCancel={onSelectionDragCancel}
          preventDrag={preventSelection}
        />
      </div>
    </BindingFocusScopeContext.Provider>
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
      aria-label={getText(`${column}ColumnName`)}
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
      // eslint-disable-next-line no-restricted-syntax
      mergeProps<HTMLAttributes<HTMLElement>>()(onlyChild.props as never, { onPointerDown }),
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
