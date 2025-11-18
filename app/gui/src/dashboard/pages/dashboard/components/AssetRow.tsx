/** @file A table row for an arbitrary asset. */
import type { ContextMenuApi } from '#/components/ContextMenu'
import {
  useDeleteAssetsMutationState,
  useMoveAssetsMutationState,
  useRestoreAssetsMutationState,
} from '#/hooks/backendBatchedHooks'
import { useBackendMutationState } from '#/hooks/backendHooks'
import * as dragAndDropHooks from '#/hooks/dragAndDropHooks'
import { useDragDelayAction } from '#/hooks/dragDelayHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import type * as assetsTable from '#/layouts/AssetsTable'
import { isLocalCategory } from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import * as columnModule from '#/pages/dashboard/components/column'
import * as columnUtils from '#/pages/dashboard/components/column/columnUtils'
import {
  setDriveLocation,
  useDriveStore,
  useSetDragTargetAssetId,
  useSetSelectedAssets,
} from '#/providers/DriveProvider'
import { unsetModal } from '#/providers/ModalProvider'
import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import { useStore } from '#/utilities/zustand'
import { useFullUserSession } from '$/providers/react'
import { useIsProjectClosing } from '$/providers/react/openedProjects'
import type { Label } from 'enso-common/src/services/Backend'
import * as backendModule from 'enso-common/src/services/Backend'
import {
  canPermissionModifyDirectoryContents,
  isTeamPath,
  tryFindSelfPermission,
} from 'enso-common/src/utilities/permissions'
import * as React from 'react'
import { useTransition } from 'react'
import invariant from 'tiny-invariant'

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
  readonly asset: backendModule.AnyAsset
  readonly state: assetsTable.AssetsTableState
}

/** Props for an {@link AssetRow}. */
export interface AssetRowProps {
  readonly item: backendModule.AnyAsset
  readonly isPlaceholder: boolean
  readonly id: backendModule.AssetId
  readonly parentId: backendModule.DirectoryId
  readonly contextMenuRef: React.RefObject<ContextMenuApi>
  readonly type: backendModule.AssetType
  readonly state: assetsTable.AssetsTableState
  readonly columns: readonly columnUtils.Column[]
  readonly isKeyboardSelected: boolean
  readonly labels: readonly Label[]
  readonly grabKeyboardFocus: (item: backendModule.AnyAsset) => void
  readonly onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
  readonly select: (item: backendModule.AnyAsset) => void
  readonly onDragStart: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: backendModule.AnyAsset,
  ) => void
  readonly onDragEnd: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: backendModule.AnyAsset,
  ) => void
  readonly onDrop: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: backendModule.AnyAsset,
  ) => void
}

/** Render a real asset row. */
export const AssetRow = React.memo(function AssetRow(props: AssetRowProps) {
  const {
    id,
    parentId,
    contextMenuRef,
    isKeyboardSelected,
    select,
    state,
    columns,
    onClick,
    isPlaceholder,
    type,
    item,
    labels,
    grabKeyboardFocus,
  } = props

  const { category, associatedBackend: backend } = useCategoriesAPI()
  const [isNavigating, startNavigation] = useTransition()

  const driveStore = useDriveStore()
  const { user } = useFullUserSession()
  const setSelectedAssets = useSetSelectedAssets()
  const getAsset = useGetAsset()
  const { isSelected, isMultiSelected } = useStore(
    driveStore,
    ({ visuallySelectedKeys, selectedIds }) => {
      const selection = visuallySelectedKeys ?? selectedIds

      return {
        isSelected: selection.has(id),
        isMultiSelected: selection.size > 1,
      }
    },
    { areEqual: 'shallow', unsafeEnableTransition: true },
  )

  const draggableProps = dragAndDropHooks.useDraggable({ isDisabled: !isSelected })
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const setDragTargetAssetId = useSetDragTargetAssetId()
  const rootRef = React.useRef<HTMLElement | null>(null)
  const grabKeyboardFocusRef = useSyncRef(grabKeyboardFocus)

  const isEditingName = useStore(driveStore, ({ assetToRename }) => assetToRename === item.id)

  const isDeletingSingleAsset =
    useBackendMutationState(backend, 'deleteAsset', {
      predicate: ({ state: { variables } }) => variables?.[0] === item.id,
      select: () => null,
    }).length !== 0
  const isDeletingMultipleAssets =
    useDeleteAssetsMutationState(backend, {
      predicate: ({ state: { variables: [assetIds = []] = [] } }) => assetIds.includes(item.id),
      select: () => null,
    }).length !== 0
  const isDeleting = isDeletingSingleAsset || isDeletingMultipleAssets
  const isRestoringSingleAsset =
    useBackendMutationState(backend, 'undoDeleteAsset', {
      predicate: ({ state: { variables } }) => variables?.[0] === item.id,
      select: () => null,
    }).length !== 0
  const isRestoringMultipleAssets =
    useRestoreAssetsMutationState(backend, {
      predicate: ({ state: { variables = { ids: [], parentId: null } } }) =>
        variables.ids.includes(item.id),
      select: () => null,
    }).length !== 0
  const isRestoring = isRestoringSingleAsset || isRestoringMultipleAssets
  const isUpdatingSingleAsset =
    useBackendMutationState(backend, 'updateAsset', {
      predicate: ({ state: { variables } }) => variables?.[0] === item.id,
      select: () => null,
    }).length !== 0
  const isMovingMultipleAssets =
    useMoveAssetsMutationState(backend, {
      predicate: ({ state: { variables: [assetIds = []] = [] } }) => assetIds.includes(item.id),
      select: () => null,
    }).length !== 0

  const isUpdating = isUpdatingSingleAsset || isMovingMultipleAssets
  const isClosing = useIsProjectClosing(
    item.type === backendModule.AssetType.project ? item.id : null,
  )

  const insertionVisibility = useStore(driveStore, (driveState) => {
    return (
        driveState.pasteData?.type === 'move' &&
          driveState.pasteData.data.assets.some((asset) => asset.id === item.id)
      ) ?
        'opacity-50'
      : Visibility.visible
  })
  const visibility = isDeleting || isRestoring || isUpdating ? 'opacity-50' : insertionVisibility

  const setSelected = useEventCallback((newSelected: boolean) => {
    const { selectedAssets } = driveStore.getState()
    setSelectedAssets(
      newSelected ?
        [...selectedAssets, item]
      : selectedAssets.filter((otherAsset) => otherAsset.id !== item.id),
    )
  })

  React.useEffect(() => {
    if (isSelected && (isDeleting || isRestoring)) {
      setSelected(false)
    }
  }, [isSelected, setSelected, isDeleting, isRestoring])

  React.useEffect(() => {
    if (isKeyboardSelected) {
      rootRef.current?.focus()
      grabKeyboardFocusRef.current(item)
    }
  }, [grabKeyboardFocusRef, isKeyboardSelected, item])

  const dragDelayProps = useDragDelayAction(
    item.type === backendModule.AssetType.directory ?
      () => {
        startNavigation(() => {
          setDriveLocation(item.id, category.id)
        })
      }
    : undefined,
  )

  const onDragOver = (event: React.DragEvent<Element>) => {
    const directoryId = item.type === backendModule.AssetType.directory ? id : parentId
    const payload = drag.ASSET_ROWS.lookup(event)
    const isPayloadMatch =
      payload != null && payload.items.every((innerItem) => innerItem.key !== directoryId)
    const canPaste = (() => {
      if (!isPayloadMatch) {
        return false
      }
      if (isLocalCategory(category)) {
        return true
      }
      return payload.items.every(({ asset }) => {
        const payloadParentId = getAsset(asset.id)?.parentId
        const parent = payloadParentId == null ? null : getAsset(payloadParentId)
        if (!parent) {
          // Assume the parent is the root directory.
          return true
        }
        if (isTeamPath(parent.ensoPath)) {
          return true
        }
        // Assume user path; check permissions
        const permission = tryFindSelfPermission(user, item.permissions)
        return permission != null && canPermissionModifyDirectoryContents(permission.permission)
      })
    })()

    if ((isPayloadMatch && canPaste) || event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
      if (item.type === backendModule.AssetType.directory && state.category.type !== 'trash') {
        setIsDraggedOver(true)
      }
    }
  }

  switch (type) {
    case backendModule.AssetType.directory:
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.secret: {
      const innerProps: AssetRowInnerProps = { asset: item, state }

      return (
        <>
          <tr
            data-testid="asset-row"
            tabIndex={0}
            data-selected={isSelected}
            data-id={item.id}
            onDoubleClick={() => {
              if (item.type === backendModule.AssetType.directory) {
                startNavigation(() => {
                  setDriveLocation(item.id, category.id)
                })
              }
            }}
            ref={(element) => {
              rootRef.current = element

              if (isKeyboardSelected && element?.contains(document.activeElement) === false) {
                element.scrollIntoView({ block: 'nearest' })
                element.focus()
              }
            }}
            className={tailwindMerge.twMerge(
              'h-table-row rounded-full transition-all ease-in-out rounded-rows-child',
              visibility,
              (isDraggedOver || isSelected) && 'selected',
            )}
            {...draggableProps}
            onClick={(event) => {
              unsetModal()
              onClick(innerProps, event)
              if (
                item.type === backendModule.AssetType.directory &&
                eventModule.isDoubleClick(event) &&
                !isEditingName
              ) {
                // This must be processed on the next tick, otherwise it will be overridden
                // by the default click handler.
                window.setTimeout(() => {
                  setSelected(false)
                })
              }
            }}
            onContextMenu={(event) => {
              // We show the asset row context menu if the asset is included in the selection.
              // Or we click on a asset row outside of the selection. In that case we reset the
              // selection to the clicked asset.
              if (isSelected && isMultiSelected) {
                return
              }

              event.preventDefault()
              event.stopPropagation()

              if (!isSelected) {
                select(item)
              }

              driveStore.setState({
                contextMenuData: {
                  triggerRef: rootRef,
                  initialContextMenuPosition: event,
                },
              })
              contextMenuRef.current?.open(event)
            }}
            onDragStart={(event) => {
              if (isEditingName) {
                event.preventDefault()
              }

              if (
                item.type === backendModule.AssetType.project &&
                (backendModule.IS_OPENING_OR_OPENED[item.projectState.type] || isClosing)
              ) {
                event.preventDefault()
              }

              props.onDragStart(event, item)
            }}
            onDragEnter={(event) => {
              // Required because `dragover` does not fire on `mouseenter`.
              onDragOver(event)
              dragDelayProps.onDragEnter(event)
            }}
            onDragOver={(event) => {
              if (state.category.type === 'trash') {
                event.dataTransfer.dropEffect = 'none'
              }
              onDragOver(event)
            }}
            onDragEnd={(event) => {
              setIsDraggedOver(false)
              props.onDragEnd(event, item)
            }}
            onDragLeave={(event) => {
              if (
                event.relatedTarget instanceof Node &&
                !event.currentTarget.contains(event.relatedTarget)
              ) {
                setIsDraggedOver(false)
                setDragTargetAssetId(null)
              }
              dragDelayProps.onDragLeave(event)
            }}
            onDrop={(event) => {
              event.preventDefault()
              event.stopPropagation()

              setIsDraggedOver(false)
              dragDelayProps.onDrop(event)
              props.onDrop(event, item)
            }}
          >
            {columns.map((column) => {
              const Render = columnModule.COLUMN_RENDERER[column]
              return (
                <td key={column} className={columnUtils.COLUMN_CSS_CLASS[column]}>
                  <Render
                    isNavigating={isNavigating}
                    labels={labels}
                    isPlaceholder={isPlaceholder}
                    backendType={backend.type}
                    item={item}
                    setSelected={setSelected}
                    state={state}
                    isEditable={state.category.type !== 'trash'}
                  />
                </td>
              )
            })}
          </tr>
        </>
      )
    }
    default: {
      invariant(false, `Unsupported asset type '${String(type)}'`)
    }
  }
})
