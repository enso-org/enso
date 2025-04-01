/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import { useStore } from '#/utilities/zustand'
import invariant from 'tiny-invariant'

import BlankIcon from '#/assets/blank.svg'

import * as dragAndDropHooks from '#/hooks/dragAndDropHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'

import type { DrivePastePayload } from '#/providers/DriveProvider'
import {
  useDriveStore,
  useSetCurrentDirectoryId,
  useSetDragTargetAssetId,
  useSetIsDraggingOverSelectedRow,
  useSetLabelsDragPayload,
  useSetSelectedAssets,
} from '#/providers/DriveProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'
import { isLocalCategory } from '#/layouts/CategorySwitcher/Category'

import * as backendModule from '#/services/Backend'

import { Text } from '#/components/AriaComponents'
import { IndefiniteSpinner } from '#/components/Spinner'
import {
  useDeleteAssetsMutationState,
  useMoveAssetsMutationState,
  useRestoreAssetsMutationState,
} from '#/hooks/backendBatchedHooks'
import { useBackendMutationState } from '#/hooks/backendHooks'
import { BUSY_PROJECT_STATES } from '#/hooks/projectHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import type { LaunchedProject } from '#/providers/ProjectsProvider'
import type { Label } from '#/services/Backend'
import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as object from '#/utilities/object'
import {
  canPermissionModifyDirectoryContents,
  isTeamParentsPath,
  tryFindSelfPermission,
} from '#/utilities/permissions'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import { useTransition } from 'react'

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
  readonly asset: backendModule.AnyAsset
  readonly state: assetsTable.AssetsTableState
  readonly rowState: assetsTable.AssetRowState
  readonly setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for an {@link AssetRow}. */
export interface AssetRowProps {
  readonly item: backendModule.AnyAsset
  readonly isOpened: boolean
  readonly isPlaceholder: boolean
  readonly id: backendModule.AssetId
  readonly parentId: backendModule.DirectoryId
  readonly type: backendModule.AssetType
  readonly state: assetsTable.AssetsTableState
  readonly columns: columnUtils.Column[]
  readonly isKeyboardSelected: boolean
  readonly labels: readonly Label[]
  readonly cutAndPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
    pasteData: DrivePastePayload,
  ) => void
  readonly grabKeyboardFocus: (item: backendModule.AnyAsset) => void
  readonly onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
  readonly select: (item: backendModule.AnyAsset) => void
  readonly onDragStart?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: backendModule.AnyAsset,
  ) => void
  readonly onDragLeave?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: backendModule.AnyAsset,
  ) => void
  readonly onDragEnd?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: backendModule.AnyAsset,
  ) => void
  readonly onDrop?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: backendModule.AnyAsset,
  ) => void
  readonly onCutAndPaste?: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
    pasteData: DrivePastePayload,
  ) => void
  readonly uploadFiles: (
    files: readonly File[],
    parentId: backendModule.DirectoryId,
  ) => Promise<void>
  readonly renameAsset: (assetId: backendModule.AssetId, newTitle: string) => Promise<void>
  readonly closeProject: (project: LaunchedProject) => Promise<void>
  readonly openProject: (projectId: backendModule.ProjectId) => Promise<void>
}

/** A row containing an {@link backendModule.AnyAsset}. */

export const AssetRow = React.memo(function AssetRow(props: AssetRowProps) {
  const { type, columns, id, item } = props

  switch (type) {
    case backendModule.AssetType.specialLoading:
    case backendModule.AssetType.specialEmpty:
    case backendModule.AssetType.specialError:
    case backendModule.AssetType.specialUp: {
      return <AssetSpecialRow columnsLength={columns.length} type={type} />
    }
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.secret:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.directory:
    default: {
      // This is safe because we filter out special asset types in the switch statement above.
      // eslint-disable-next-line no-restricted-syntax
      return <RealAssetRow {...props} id={id as backendModule.RealAssetId} item={item} />
    }
  }
})

/** Props for a {@link AssetSpecialRow}. */
export interface AssetSpecialRowProps {
  readonly type: backendModule.AssetType
  readonly columnsLength: number
}

/** Renders a special asset row. */
const AssetSpecialRow = React.memo(function AssetSpecialRow(props: AssetSpecialRowProps) {
  const { type, columnsLength } = props

  const { getText } = textProvider.useText()

  switch (type) {
    case backendModule.AssetType.specialUp: {
      // TODO: Implement this.
      // @MrFlashAccount [Cloud v2 #1810](https://github.com/enso-org/cloud-v2/issues/1810)
      return null
    }

    case backendModule.AssetType.specialLoading: {
      return (
        <tr>
          <td colSpan={columnsLength} className="border-r p-0">
            <div className="flex h-table-row items-center justify-center rounded-full">
              <IndefiniteSpinner size={24} />
            </div>
          </td>
        </tr>
      )
    }
    case backendModule.AssetType.specialEmpty: {
      return (
        <tr>
          <td colSpan={columnsLength} className="border-r p-0">
            <div className="flex h-table-row items-center rounded-full">
              <img src={BlankIcon} />
              <Text className="px-name-column-x placeholder" disableLineHeightCompensation>
                {getText('thisFolderIsEmpty')}
              </Text>
            </div>
          </td>
        </tr>
      )
    }
    case backendModule.AssetType.specialError: {
      return (
        <tr>
          <td colSpan={columnsLength} className="border-r p-0">
            <div className="flex h-table-row items-center rounded-full">
              <img src={BlankIcon} />
              <Text
                className="px-name-column-x text-danger placeholder"
                disableLineHeightCompensation
              >
                {getText('thisFolderFailedToFetch')}
              </Text>
            </div>
          </td>
        </tr>
      )
    }
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.secret:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.directory:
    default: {
      invariant(false, 'Unsupported special asset type: ' + type)
    }
  }
})

/** Props for a {@link RealAssetRow}. */
type RealAssetRowProps = AssetRowProps

/** Render a real asset row. */
export function RealAssetRow(props: RealAssetRowProps) {
  const {
    id,
    parentId,
    isKeyboardSelected,
    isOpened,
    select,
    state,
    columns,
    onClick,
    isPlaceholder,
    type,
    item,
    cutAndPaste,
    labels,
    grabKeyboardFocus,
    uploadFiles,
    renameAsset,
    closeProject,
    openProject,
  } = props
  const { category, backend, currentDirectoryId, doCopy, doCut, doPaste } = state

  const [isNavigating, startNavigation] = useTransition()

  const driveStore = useDriveStore()
  const { user } = useFullUserSession()
  const setSelectedAssets = useSetSelectedAssets()
  const getAsset = useGetAsset()
  const selected = useStore(driveStore, ({ visuallySelectedKeys, selectedIds }) =>
    (visuallySelectedKeys ?? selectedIds).has(id),
  )
  const isSoleSelected = useStore(
    driveStore,
    ({ selectedIds, visuallySelectedKeys }) =>
      selected && (visuallySelectedKeys ?? selectedIds).size === 1,
  )
  const allowContextMenu = useStore(
    driveStore,
    ({ selectedIds }) => selectedIds.size === 0 || !selected || isSoleSelected,
  )
  const setCurrentDirectoryId = useSetCurrentDirectoryId()
  const draggableProps = dragAndDropHooks.useDraggable({ isDisabled: !selected })
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const setIsDraggingOverSelectedRow = useSetIsDraggingOverSelectedRow()
  const setDragTargetAssetId = useSetDragTargetAssetId()
  const rootRef = React.useRef<HTMLElement | null>(null)
  const dragOverTimeoutHandle = React.useRef<number | null>(null)
  const grabKeyboardFocusRef = useSyncRef(grabKeyboardFocus)
  const [innerRowState, setRowState] = React.useState<assetsTable.AssetRowState>(
    assetRowUtils.INITIAL_ROW_STATE,
  )
  const setLabelsDragPayload = useSetLabelsDragPayload()

  const isNewlyCreated = useStore(driveStore, ({ newestFolderId }) => newestFolderId === item.id)
  const isEditingName = innerRowState.isEditingName || isNewlyCreated

  const rowState = React.useMemo(() => {
    return object.merge(innerRowState, { isEditingName })
  }, [isEditingName, innerRowState])

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
      predicate: ({ state: { variables: assetIds = [] } }) => assetIds.includes(item.id),
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

  const insertionVisibility = useStore(driveStore, (driveState) =>
    driveState.pasteData?.type === 'move' && driveState.pasteData.data.ids.has(id) ?
      Visibility.faded
    : Visibility.visible,
  )
  const visibility =
    isDeleting || isRestoring || isUpdating ? Visibility.faded : insertionVisibility

  const setSelected = useEventCallback((newSelected: boolean) => {
    const { selectedAssets } = driveStore.getState()
    setSelectedAssets(
      newSelected ?
        [...selectedAssets, item]
      : selectedAssets.filter((otherAsset) => otherAsset.id !== item.id),
    )
  })

  React.useEffect(() => {
    if (selected && insertionVisibility !== Visibility.visible) {
      setSelected(false)
    }
  }, [selected, insertionVisibility, setSelected])

  React.useEffect(() => {
    if (isKeyboardSelected) {
      rootRef.current?.focus()
      grabKeyboardFocusRef.current(item)
    }
  }, [grabKeyboardFocusRef, isKeyboardSelected, item])

  const onDragOver = (event: React.DragEvent<Element>) => {
    const directoryId = item.type === backendModule.AssetType.directory ? id : parentId
    const { labelsDragPayload, isDraggingOverSelectedRow } = driveStore.getState()
    if (labelsDragPayload) {
      event.preventDefault()
      event.stopPropagation()
      setDragTargetAssetId(item.id)
      if (selected !== isDraggingOverSelectedRow) {
        setIsDraggingOverSelectedRow(selected)
      }
      return
    }
    const payload = drag.ASSET_ROWS.lookup(event)
    const isPayloadMatch =
      payload != null && payload.every((innerItem) => innerItem.key !== directoryId)
    const canPaste = (() => {
      if (!isPayloadMatch) {
        return false
      }
      if (isLocalCategory(category)) {
        return true
      }
      return payload.every((payloadItem) => {
        const payloadParentId = getAsset(payloadItem.key)?.parentId
        const parent = payloadParentId == null ? null : getAsset(payloadParentId)
        if (!parent) {
          // Assume the parent is the root directory.
          return true
        }
        if (isTeamParentsPath(parent.parentsPath, [])) {
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
      const innerProps: AssetRowInnerProps = {
        asset: item,
        state,
        rowState,
        setRowState,
      }

      return (
        <>
          <tr
            data-testid="asset-row"
            tabIndex={0}
            data-selected={selected}
            data-id={item.id}
            onDoubleClick={() => {
              if (item.type === backendModule.AssetType.directory) {
                startNavigation(() => {
                  setCurrentDirectoryId({
                    current: item.id,
                    parent: parentId,
                  })
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
              'h-table-row rounded-full transition-all ease-in-out rounded-rows-child [contain-intrinsic-size:44px] [content-visibility:auto]',
              visibility,
              (isDraggedOver || selected) && 'selected',
            )}
            {...draggableProps}
            onClick={(event) => {
              unsetModal()
              onClick(innerProps, event)
              if (
                item.type === backendModule.AssetType.directory &&
                eventModule.isDoubleClick(event) &&
                !rowState.isEditingName
              ) {
                // This must be processed on the next tick, otherwise it will be overridden
                // by the default click handler.
                window.setTimeout(() => {
                  setSelected(false)
                })
              }
            }}
            onContextMenu={(event) => {
              if (allowContextMenu) {
                event.preventDefault()
                event.stopPropagation()
                if (!selected) {
                  select(item)
                }
                setModal(
                  <AssetContextMenu
                    innerProps={innerProps}
                    currentDirectoryId={currentDirectoryId}
                    triggerRef={rootRef}
                    event={event}
                    eventTarget={
                      event.target instanceof HTMLElement ? event.target : event.currentTarget
                    }
                    doCopy={doCopy}
                    doCut={doCut}
                    doPaste={doPaste}
                  />,
                )
              }
            }}
            onDragStart={(event) => {
              if (rowState.isEditingName) {
                event.preventDefault()
              }

              if (
                item.type === backendModule.AssetType.project &&
                BUSY_PROJECT_STATES.has(item.projectState.type)
              ) {
                event.preventDefault()
              }

              props.onDragStart?.(event, item)
            }}
            onDragEnter={(event) => {
              if (dragOverTimeoutHandle.current != null) {
                window.clearTimeout(dragOverTimeoutHandle.current)
              }
              // Required because `dragover` does not fire on `mouseenter`.
              onDragOver(event)
            }}
            onDragOver={(event) => {
              if (state.category.type === 'trash') {
                event.dataTransfer.dropEffect = 'none'
              }
              onDragOver(event)
            }}
            onDragEnd={(event) => {
              setIsDraggedOver(false)
              setLabelsDragPayload(null)
              props.onDragEnd?.(event, item)
            }}
            onDragLeave={(event) => {
              if (
                dragOverTimeoutHandle.current != null &&
                (!(event.relatedTarget instanceof Node) ||
                  !event.currentTarget.contains(event.relatedTarget))
              ) {
                window.clearTimeout(dragOverTimeoutHandle.current)
              }
              if (
                event.relatedTarget instanceof Node &&
                !event.currentTarget.contains(event.relatedTarget)
              ) {
                setIsDraggedOver(false)
                setDragTargetAssetId(null)
              }
              props.onDragLeave?.(event, item)
            }}
            onDrop={(event) => {
              if (state.category.type === 'trash' || state.category.type === 'recent') {
                return
              }
              props.onDrop?.(event, item)
              setIsDraggedOver(false)
              const directoryId =
                item.type === backendModule.AssetType.directory ? item.id : parentId
              const payload = drag.ASSET_ROWS.lookup(event)
              if (payload != null && payload.every((innerItem) => innerItem.key !== directoryId)) {
                event.preventDefault()
                event.stopPropagation()
                unsetModal()
                const ids = payload
                  .filter((payloadItem) => payloadItem.asset.parentId !== directoryId)
                  .map((dragItem) => dragItem.key)
                cutAndPaste(directoryId, directoryId, {
                  backendType: backend.type,
                  ids: new Set(ids),
                  category,
                })
              } else if (event.dataTransfer.types.includes('Files')) {
                event.preventDefault()
                event.stopPropagation()
                void uploadFiles(Array.from(event.dataTransfer.files), directoryId)
              }
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
                    isOpened={isOpened}
                    backendType={backend.type}
                    item={item}
                    setSelected={setSelected}
                    state={state}
                    rowState={rowState}
                    setRowState={setRowState}
                    isEditable={state.category.type !== 'trash'}
                    renameAsset={renameAsset}
                    closeProject={closeProject}
                    openProject={openProject}
                  />
                </td>
              )
            })}
          </tr>

          {isSoleSelected && allowContextMenu && (
            // This is a copy of the context menu, since the context menu registers keyboard
            // shortcut handlers. This is a bit of a hack, however it is preferable to duplicating
            // the entire context menu (once for the keyboard actions, once for the JSX).
            <AssetContextMenu
              hidden
              innerProps={innerProps}
              currentDirectoryId={currentDirectoryId}
              triggerRef={rootRef}
              event={{ pageX: 0, pageY: 0 }}
              eventTarget={null}
              doCopy={doCopy}
              doCut={doCut}
              doPaste={doPaste}
            />
          )}
        </>
      )
    }
    case backendModule.AssetType.specialUp:
    case backendModule.AssetType.specialLoading:
    case backendModule.AssetType.specialEmpty:
    case backendModule.AssetType.specialError:
    default: {
      invariant(
        false,
        'Unsupported asset type, expected one of: directory, project, file, datalink, secret, but got: ' +
          type,
      )
    }
  }
}
