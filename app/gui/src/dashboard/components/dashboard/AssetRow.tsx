/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import { useStore } from '#/utilities/zustand'
import { useMutation, useQuery } from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import BlankIcon from '#/assets/blank.svg'

import * as dragAndDropHooks from '#/hooks/dragAndDropHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'

import type { DrivePastePayload } from '#/providers/DriveProvider'
import {
  useDriveStore,
  useSetDragTargetAssetId,
  useSetIsDraggingOverSelectedRow,
  useSetLabelsDragPayload,
  useSetSelectedAssets,
  useToggleDirectoryExpansion,
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
  useRestoreAssetsMutationState,
} from '#/hooks/backendBatchedHooks'
import { backendMutationOptions, useBackendMutationState } from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { useCutAndPaste } from '#/hooks/cutAndPasteHooks'
import { createGetProjectDetailsQuery } from '#/hooks/projectHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'

/**
 * The amount of time (in milliseconds) the drag item must be held over this component
 * to make a directory row expand.
 */
const DRAG_EXPAND_DELAY_MS = 1_500

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
  readonly asset: backendModule.AnyAsset
  readonly path: string
  readonly state: assetsTable.AssetsTableState
  readonly rowState: assetsTable.AssetRowState
  readonly setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for an {@link AssetRow}. */
export interface AssetRowProps {
  readonly isOpened: boolean

  readonly isPlaceholder: boolean
  readonly visibility: Visibility | undefined
  readonly id: backendModule.AssetId
  readonly parentId: backendModule.DirectoryId
  readonly type: backendModule.AssetType
  readonly hidden: boolean
  readonly path: string
  readonly depth: number
  readonly state: assetsTable.AssetsTableState
  readonly columns: columnUtils.Column[]
  readonly isKeyboardSelected: boolean
  readonly grabKeyboardFocus: (item: backendModule.AnyAsset) => void
  readonly onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
  readonly select: (item: backendModule.AnyAsset) => void
  readonly isExpanded: boolean
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
    nodeMap: ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>,
  ) => void
}

/** A row containing an {@link backendModule.AnyAsset}. */
// eslint-disable-next-line no-restricted-syntax
export const AssetRow = React.memo(function AssetRow(props: AssetRowProps) {
  const { type, columns, depth, id } = props

  switch (type) {
    case backendModule.AssetType.specialLoading:
    case backendModule.AssetType.specialEmpty:
    case backendModule.AssetType.specialError: {
      return <AssetSpecialRow columnsLength={columns.length} depth={depth} type={type} />
    }
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.secret:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.directory:
    default: {
      // This is safe because we filter out special asset types in the switch statement above.
      // eslint-disable-next-line no-restricted-syntax
      return <RealAssetRow {...props} id={id as backendModule.RealAssetId} />
    }
  }
})

/** Props for a {@link AssetSpecialRow}. */
export interface AssetSpecialRowProps {
  readonly type: backendModule.AssetType
  readonly columnsLength: number
  readonly depth: number
}

/** Renders a special asset row. */
// eslint-disable-next-line no-restricted-syntax
const AssetSpecialRow = React.memo(function AssetSpecialRow(props: AssetSpecialRowProps) {
  const { type, columnsLength, depth } = props

  const { getText } = textProvider.useText()

  switch (type) {
    case backendModule.AssetType.specialLoading: {
      return (
        <tr>
          <td colSpan={columnsLength} className="border-r p-0 rounded-rows-skip-level">
            <div
              className={tailwindMerge.twJoin(
                'flex h-table-row w-container items-center justify-center rounded-full rounded-rows-child',
                indent.indentClass(depth),
              )}
            >
              <IndefiniteSpinner size={24} />
            </div>
          </td>
        </tr>
      )
    }
    case backendModule.AssetType.specialEmpty: {
      return (
        <tr>
          <td colSpan={columnsLength} className="border-r p-0 rounded-rows-skip-level">
            <div
              className={tailwindMerge.twJoin(
                'flex h-table-row items-center rounded-full rounded-rows-child',
                indent.indentClass(depth),
              )}
            >
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
          <td colSpan={columnsLength} className="border-r p-0 rounded-rows-skip-level">
            <div
              className={tailwindMerge.twJoin(
                'flex h-table-row items-center rounded-full rounded-rows-child',
                indent.indentClass(depth),
              )}
            >
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
type RealAssetRowProps = AssetRowProps & { readonly id: backendModule.RealAssetId }

/** Renders a real asset row. */
// eslint-disable-next-line no-restricted-syntax
const RealAssetRow = React.memo(function RealAssetRow(props: RealAssetRowProps) {
  const { id } = props

  const asset = useAsset(id)

  // should never happen since we only render real assets and they are always defined
  if (asset == null) {
    return null
  }

  return <RealAssetInternalRow {...props} asset={asset} />
})

/** Internal props for a {@link RealAssetRow}. */
export interface RealAssetRowInternalProps extends AssetRowProps {
  readonly asset: backendModule.AnyAsset
}

/** Internal implementation of a {@link RealAssetRow}. */
export function RealAssetInternalRow(props: RealAssetRowInternalProps) {
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
    isExpanded,
    type,
    asset,
  } = props
  const { path, hidden: hiddenRaw, grabKeyboardFocus, visibility: visibilityRaw, depth } = props
  const { nodeMap, doCopy, doCut, doPaste } = state
  const { category, rootDirectoryId, backend } = state

  const driveStore = useDriveStore()
  const { user } = useFullUserSession()
  const setSelectedAssets = useSetSelectedAssets()
  const selected = useStore(driveStore, ({ visuallySelectedKeys, selectedKeys }) =>
    (visuallySelectedKeys ?? selectedKeys).has(id),
  )
  const isSoleSelected = useStore(
    driveStore,
    ({ selectedKeys }) => selected && selectedKeys.size === 1,
  )
  const allowContextMenu = useStore(
    driveStore,
    ({ selectedKeys }) => selectedKeys.size === 0 || !selected || isSoleSelected,
  )
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
  const cutAndPaste = useCutAndPaste(backend, category)
  const toggleDirectoryExpansion = useToggleDirectoryExpansion()
  const setLabelsDragPayload = useSetLabelsDragPayload()

  const isNewlyCreated = useStore(driveStore, ({ newestFolderId }) => newestFolderId === asset.id)
  const isEditingName = innerRowState.isEditingName || isNewlyCreated

  const rowState = React.useMemo(() => {
    return object.merge(innerRowState, { isEditingName })
  }, [isEditingName, innerRowState])

  const nodeParentKeysRef = React.useRef<{
    readonly nodeMap: WeakRef<ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>>
    readonly parentKeys: Map<backendModule.AssetId, backendModule.DirectoryId>
  } | null>(null)

  const isDeletingSingleAsset =
    useBackendMutationState(backend, 'deleteAsset', {
      predicate: ({ state: { variables = EMPTY_ARRAY } }) => variables[0] === asset.id,
      select: () => null,
    }).length !== 0
  const isDeletingMultipleAssets =
    useDeleteAssetsMutationState(backend, {
      predicate: ({ state: { variables: [assetIds = EMPTY_ARRAY] = EMPTY_ARRAY } }) =>
        assetIds.includes(asset.id),
      select: () => null,
    }).length !== 0
  const isDeleting = isDeletingSingleAsset || isDeletingMultipleAssets
  const isRestoringSingleAsset =
    useBackendMutationState(backend, 'undoDeleteAsset', {
      predicate: ({ state: { variables = EMPTY_ARRAY } }) => variables[0] === asset.id,
      select: () => null,
    }).length !== 0
  const isRestoringMultipleAssets =
    useRestoreAssetsMutationState(backend, {
      predicate: ({ state: { variables: assetIds = EMPTY_ARRAY } }) => assetIds.includes(asset.id),
      select: () => null,
    }).length !== 0
  const isRestoring = isRestoringSingleAsset || isRestoringMultipleAssets

  const { data: projectState } = useQuery({
    ...createGetProjectDetailsQuery({
      // This is safe because we disable the query when the asset is not a project.
      // see `enabled` property below.
      // eslint-disable-next-line no-restricted-syntax
      assetId: asset.id as backendModule.ProjectId,
      backend,
    }),
    select: (data) => data.state.type,
    enabled: asset.type === backendModule.AssetType.project && !isPlaceholder && isOpened,
  })

  const uploadFiles = useUploadFiles(backend, category)
  const createPermissionMutation = useMutation(
    backendMutationOptions(backend, 'createPermission', {
      meta: {
        invalidates: [[backend.type, 'listDirectory', asset.parentId]],
        awaitInvalidates: true,
      },
    }),
  )

  const insertionVisibility = useStore(driveStore, (driveState) =>
    driveState.pasteData?.type === 'move' && driveState.pasteData.data.ids.has(id) ?
      Visibility.faded
    : Visibility.visible,
  )
  const createPermissionVariables = createPermissionMutation.variables?.[0]
  const isRemovingSelf =
    createPermissionVariables?.actorsIds[0] === user.userId &&
    createPermissionVariables.action == null
  const visibility =
    isDeleting || isRestoring ? Visibility.faded
    : isRemovingSelf ? Visibility.hidden
    : visibilityRaw === Visibility.visible ? insertionVisibility
    : (visibilityRaw ?? insertionVisibility)
  const hidden = hiddenRaw || visibility === Visibility.hidden

  const setSelected = useEventCallback((newSelected: boolean) => {
    const { selectedAssets } = driveStore.getState()
    setSelectedAssets(
      newSelected ?
        [...selectedAssets, asset]
      : selectedAssets.filter((otherAsset) => otherAsset.id !== asset.id),
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
      grabKeyboardFocusRef.current(asset)
    }
  }, [grabKeyboardFocusRef, isKeyboardSelected, asset])

  const onDragOver = (event: React.DragEvent<Element>) => {
    const directoryId = asset.type === backendModule.AssetType.directory ? id : parentId
    const labelsPayload = drag.LABELS.lookup(event)
    if (labelsPayload) {
      event.preventDefault()
      event.stopPropagation()
      setDragTargetAssetId(asset.id)
      const { isDraggingOverSelectedRow } = driveStore.getState()
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
      } else {
        if (nodeMap.current !== nodeParentKeysRef.current?.nodeMap.deref()) {
          const parentKeys = new Map(
            Array.from(nodeMap.current.entries()).map(([otherId, otherAsset]) => [
              otherId,
              otherAsset.item.parentId,
            ]),
          )
          nodeParentKeysRef.current = { nodeMap: new WeakRef(nodeMap.current), parentKeys }
        }

        if (isLocalCategory(category)) {
          return true
        }

        return payload.every((payloadItem) => {
          const parentKey = nodeParentKeysRef.current?.parentKeys.get(payloadItem.key)
          const parent = parentKey == null ? null : nodeMap.current.get(parentKey)
          if (!parent) {
            return false
          } else if (permissions.isTeamPath(parent.path)) {
            return true
          } else {
            // Assume user path; check permissions
            const permission = permissions.tryFindSelfPermission(user, asset.permissions)
            return (
              permission != null &&
              permissions.canPermissionModifyDirectoryContents(permission.permission)
            )
          }
        })
      }
    })()

    if ((isPayloadMatch && canPaste) || event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
      if (asset.type === backendModule.AssetType.directory && state.category.type !== 'trash') {
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
        asset,
        path,
        state,
        rowState,
        setRowState,
      }

      if (hidden) {
        return null
      }

      return (
        <>
          <tr
            data-testid="asset-row"
            tabIndex={0}
            data-selected={selected}
            data-id={asset.id}
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
              (isDraggedOver || selected) && 'selected',
            )}
            {...draggableProps}
            onClick={(event) => {
              unsetModal()
              onClick(innerProps, event)
              if (
                asset.type === backendModule.AssetType.directory &&
                eventModule.isDoubleClick(event) &&
                !rowState.isEditingName
              ) {
                // This must be processed on the next tick, otherwise it will be overridden
                // by the default click handler.
                window.setTimeout(() => {
                  setSelected(false)
                })
                toggleDirectoryExpansion(asset.id)
              }
            }}
            onContextMenu={(event) => {
              if (allowContextMenu) {
                event.preventDefault()
                event.stopPropagation()
                if (!selected) {
                  select(asset)
                }
                setModal(
                  <AssetContextMenu
                    innerProps={innerProps}
                    rootDirectoryId={rootDirectoryId}
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
              if (
                rowState.isEditingName ||
                (projectState !== backendModule.ProjectState.closed &&
                  projectState !== backendModule.ProjectState.created &&
                  projectState != null)
              ) {
                event.preventDefault()
              } else {
                props.onDragStart?.(event, asset)
              }
            }}
            onDragEnter={(event) => {
              if (dragOverTimeoutHandle.current != null) {
                window.clearTimeout(dragOverTimeoutHandle.current)
              }
              if (asset.type === backendModule.AssetType.directory) {
                dragOverTimeoutHandle.current = window.setTimeout(() => {
                  toggleDirectoryExpansion(asset.id, true)
                }, DRAG_EXPAND_DELAY_MS)
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
              props.onDragEnd?.(event, asset)
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
              }
              props.onDragLeave?.(event, asset)
            }}
            onDrop={(event) => {
              if (state.category.type !== 'trash') {
                props.onDrop?.(event, asset)
                setIsDraggedOver(false)
                const directoryId =
                  asset.type === backendModule.AssetType.directory ? asset.id : parentId
                const payload = drag.ASSET_ROWS.lookup(event)
                if (
                  payload != null &&
                  payload.every((innerItem) => innerItem.key !== directoryId)
                ) {
                  event.preventDefault()
                  event.stopPropagation()
                  unsetModal()
                  toggleDirectoryExpansion(directoryId, true)
                  const ids = payload
                    .filter((payloadItem) => payloadItem.asset.parentId !== directoryId)
                    .map((dragItem) => dragItem.key)
                  cutAndPaste(
                    directoryId,
                    directoryId,
                    { backendType: backend.type, ids: new Set(ids), category },
                    nodeMap.current,
                  )
                } else if (event.dataTransfer.types.includes('Files')) {
                  event.preventDefault()
                  event.stopPropagation()
                  toggleDirectoryExpansion(directoryId, true)
                  void uploadFiles(Array.from(event.dataTransfer.files), directoryId, null)
                }
              }
            }}
          >
            {columns.map((column) => {
              const Render = columnModule.COLUMN_RENDERER[column]
              return (
                <td key={column} className={columnUtils.COLUMN_CSS_CLASS[column]}>
                  <Render
                    isPlaceholder={isPlaceholder}
                    isExpanded={isExpanded}
                    isOpened={isOpened}
                    backendType={backend.type}
                    item={asset}
                    depth={depth}
                    selected={selected}
                    setSelected={setSelected}
                    isSoleSelected={isSoleSelected}
                    state={state}
                    rowState={rowState}
                    setRowState={setRowState}
                    isEditable={state.category.type !== 'trash'}
                  />
                </td>
              )
            })}
          </tr>

          {selected && allowContextMenu && (
            // This is a copy of the context menu, since the context menu registers keyboard
            // shortcut handlers. This is a bit of a hack, however it is preferable to duplicating
            // the entire context menu (once for the keyboard actions, once for the JSX).
            <AssetContextMenu
              hidden
              innerProps={innerProps}
              rootDirectoryId={rootDirectoryId}
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
