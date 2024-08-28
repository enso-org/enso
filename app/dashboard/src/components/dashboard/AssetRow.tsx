/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import { useStore } from 'zustand'

import BlankIcon from '#/assets/blank.svg'

import * as dragAndDropHooks from '#/hooks/dragAndDropHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'

import { useDriveStore, useSetSelectedKeys } from '#/providers/DriveProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import FocusRing from '#/components/styled/FocusRing'

import EditAssetDescriptionModal from '#/modals/EditAssetDescriptionModal'

import * as backendModule from '#/services/Backend'

import { createGetProjectDetailsQuery } from '#/hooks/projectHooks'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as set from '#/utilities/set'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import { useQuery } from '@tanstack/react-query'

// =================
// === Constants ===
// =================

/** The height of the header row. */
const HEADER_HEIGHT_PX = 40
/** The amount of time (in milliseconds) the drag item must be held over this component
 * to make a directory row expand. */
const DRAG_EXPAND_DELAY_MS = 500

// ================
// === AssetRow ===
// ================

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
  readonly key: backendModule.AssetId
  readonly item: assetTreeNode.AnyAssetTreeNode
  readonly setItem: React.Dispatch<React.SetStateAction<assetTreeNode.AnyAssetTreeNode>>
  readonly state: assetsTable.AssetsTableState
  readonly rowState: assetsTable.AssetRowState
  readonly setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for an {@link AssetRow}. */
export interface AssetRowProps
  extends Readonly<Omit<JSX.IntrinsicElements['tr'], 'onClick' | 'onContextMenu'>> {
  readonly isOpened: boolean
  readonly item: assetTreeNode.AnyAssetTreeNode
  readonly state: assetsTable.AssetsTableState
  readonly hidden: boolean
  readonly columns: columnUtils.Column[]
  readonly isKeyboardSelected: boolean
  readonly grabKeyboardFocus: () => void
  readonly onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
  readonly select: () => void
  readonly updateAssetRef: React.Ref<(asset: backendModule.AnyAsset) => void>
}

/** A row containing an {@link backendModule.AnyAsset}. */
export default function AssetRow(props: AssetRowProps) {
  const { isKeyboardSelected, isOpened, select, state, columns, onClick } = props
  const { item: rawItem, hidden: hiddenRaw, updateAssetRef, grabKeyboardFocus } = props
  const {
    nodeMap,
    setAssetPanelProps,
    doToggleDirectoryExpansion,
    doCopy,
    doCut,
    doPaste,
    doDelete: doDeleteRaw,
  } = state
  const { setIsAssetPanelTemporarilyVisible, scrollContainerRef, rootDirectoryId, backend } = state
  const { visibilities } = state

  const [item, setItem] = React.useState(rawItem)
  const driveStore = useDriveStore()
  const setSelectedKeys = useSetSelectedKeys()
  const selected = useStore(driveStore, ({ visuallySelectedKeys, selectedKeys }) =>
    (visuallySelectedKeys ?? selectedKeys).has(item.key),
  )
  const isSoleSelected = useStore(
    driveStore,
    ({ selectedKeys }) => selected && selectedKeys.size === 1,
  )
  const allowContextMenu = useStore(
    driveStore,
    ({ selectedKeys }) => selectedKeys.size === 0 || !selected || isSoleSelected,
  )
  const draggableProps = dragAndDropHooks.useDraggable()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const rootRef = React.useRef<HTMLElement | null>(null)
  const dragOverTimeoutHandle = React.useRef<number | null>(null)
  const grabKeyboardFocusRef = React.useRef(grabKeyboardFocus)
  grabKeyboardFocusRef.current = grabKeyboardFocus
  const asset = item.item
  const [insertionVisibility, setInsertionVisibility] = React.useState(Visibility.visible)
  const [rowState, setRowState] = React.useState<assetsTable.AssetRowState>(() =>
    object.merge(assetRowUtils.INITIAL_ROW_STATE, { setVisibility: setInsertionVisibility }),
  )
  const nodeParentKeysRef = React.useRef<{
    readonly nodeMap: WeakRef<ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>>
    readonly parentKeys: Map<backendModule.AssetId, backendModule.DirectoryId>
  } | null>(null)

  const outerVisibility = visibilities.get(item.key)
  const visibility =
    outerVisibility == null || outerVisibility === Visibility.visible ?
      insertionVisibility
    : outerVisibility
  const hidden = hiddenRaw || visibility === Visibility.hidden

  const { data: projectState } = useQuery({
    // This is SAFE, as `isOpened` is only true for projects.
    // eslint-disable-next-line no-restricted-syntax
    ...createGetProjectDetailsQuery.createPassiveListener(item.item.id as backendModule.ProjectId),
    select: (data) => data.state.type,
    enabled: item.type === backendModule.AssetType.project,
  })

  const setSelected = useEventCallback((newSelected: boolean) => {
    const { selectedKeys } = driveStore.getState()
    setSelectedKeys(set.withPresence(selectedKeys, item.key, newSelected))
  })

  React.useEffect(() => {
    setItem(rawItem)
  }, [rawItem])

  React.useEffect(() => {
    // Mutation is HIGHLY INADVISABLE in React, however it is useful here as we want to update the
    // parent's state while avoiding re-rendering the parent.
    rawItem.item = asset
  }, [asset, rawItem])
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

  React.useEffect(() => {
    if (selected && insertionVisibility !== Visibility.visible) {
      setSelected(false)
    }
  }, [selected, insertionVisibility, setSelected])

  React.useEffect(() => {
    if (isKeyboardSelected) {
      rootRef.current?.focus()
      grabKeyboardFocusRef.current()
    }
  }, [isKeyboardSelected])

  React.useImperativeHandle(updateAssetRef, () => setAsset)

  React.useEffect(() => {
    if (isSoleSelected) {
      setAssetPanelProps({ backend, item, setItem })
      setIsAssetPanelTemporarilyVisible(false)
    }
  }, [item, isSoleSelected, backend, setAssetPanelProps, setIsAssetPanelTemporarilyVisible])

  const doDelete = React.useCallback(
    async (forever = false) => {
      doDeleteRaw(forever, item.item)
    },
    [doDeleteRaw, item.item],
  )

  const doTriggerDescriptionEdit = React.useCallback(() => {
    setModal(
      <EditAssetDescriptionModal
        doChangeDescription={async (description) => {
          if (description !== asset.description) {
            setAsset(object.merger({ description }))

            await backend
              .updateAsset(item.item.id, { parentDirectoryId: null, description }, item.item.title)
              .catch((error) => {
                setAsset(object.merger({ description: asset.description }))
                throw error
              })
          }
        }}
        initialDescription={asset.description}
      />,
    )
  }, [setModal, asset.description, setAsset, backend, item.item.id, item.item.title])

  const clearDragState = React.useCallback(() => {
    setIsDraggedOver(false)
    setRowState((oldRowState) =>
      oldRowState.temporarilyAddedLabels === set.EMPTY_SET ?
        oldRowState
      : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY_SET }),
    )
  }, [])

  const onDragOver = (event: React.DragEvent<Element>) => {
    const directoryKey =
      item.item.type === backendModule.AssetType.directory ? item.key : item.directoryKey
    const payload = drag.ASSET_ROWS.lookup(event)
    const isPayloadMatch =
      payload != null && payload.every((innerItem) => innerItem.key !== directoryKey)
    const canPaste = (() => {
      if (!isPayloadMatch) {
        return true
      } else {
        if (nodeMap.current !== nodeParentKeysRef.current?.nodeMap.deref()) {
          const parentKeys = new Map(
            Array.from(nodeMap.current.entries()).map(([id, otherAsset]) => [
              id,
              otherAsset.directoryKey,
            ]),
          )
          nodeParentKeysRef.current = { nodeMap: new WeakRef(nodeMap.current), parentKeys }
        }
        return !payload.some((payloadItem) => {
          const parentKey = nodeParentKeysRef.current?.parentKeys.get(payloadItem.key)
          const parent = parentKey == null ? null : nodeMap.current.get(parentKey)
          return !parent ? true : (
              permissions.isTeamPath(parent.path) && permissions.isUserPath(item.path)
            )
        })
      }
    })()
    if ((isPayloadMatch && canPaste) || event.dataTransfer.types.includes('Files')) {
      event.preventDefault()
      if (
        item.item.type === backendModule.AssetType.directory &&
        state.category.type !== categoryModule.CategoryType.trash
      ) {
        setIsDraggedOver(true)
      }
    }
  }

  switch (asset.type) {
    case backendModule.AssetType.directory:
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.secret: {
      const innerProps: AssetRowInnerProps = {
        key: item.key,
        item,
        setItem,
        state,
        rowState,
        setRowState,
      }
      return (
        <>
          {!hidden && (
            <FocusRing>
              <tr
                data-testid="asset-row"
                tabIndex={0}
                ref={(element) => {
                  rootRef.current = element

                  requestAnimationFrame(() => {
                    if (isSoleSelected && element != null && scrollContainerRef.current != null) {
                      const rect = element.getBoundingClientRect()
                      const scrollRect = scrollContainerRef.current.getBoundingClientRect()
                      const scrollUp = rect.top - (scrollRect.top + HEADER_HEIGHT_PX)
                      const scrollDown = rect.bottom - scrollRect.bottom

                      if (scrollUp < 0 || scrollDown > 0) {
                        scrollContainerRef.current.scrollBy({
                          top: scrollUp < 0 ? scrollUp : scrollDown,
                          behavior: 'smooth',
                        })
                      }
                    }
                  })

                  if (isKeyboardSelected && element?.contains(document.activeElement) === false) {
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
                    item.type === backendModule.AssetType.directory &&
                    eventModule.isDoubleClick(event) &&
                    !rowState.isEditingName
                  ) {
                    // This must be processed on the next tick, otherwise it will be overridden
                    // by the default click handler.
                    window.setTimeout(() => {
                      setSelected(false)
                    })
                    doToggleDirectoryExpansion(item.item.id, item.key)
                  }
                }}
                onContextMenu={(event) => {
                  if (allowContextMenu) {
                    event.preventDefault()
                    event.stopPropagation()
                    if (!selected) {
                      select()
                    }
                    setModal(
                      <AssetContextMenu
                        innerProps={innerProps}
                        rootDirectoryId={rootDirectoryId}
                        event={event}
                        eventTarget={
                          event.target instanceof HTMLElement ? event.target : event.currentTarget
                        }
                        doCopy={doCopy}
                        doCut={doCut}
                        doPaste={doPaste}
                        doDelete={doDelete}
                        doTriggerDescriptionEdit={doTriggerDescriptionEdit}
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
                    props.onDragStart?.(event)
                  }
                }}
                onDragEnter={(event) => {
                  if (dragOverTimeoutHandle.current != null) {
                    window.clearTimeout(dragOverTimeoutHandle.current)
                  }
                  if (item.type === backendModule.AssetType.directory) {
                    dragOverTimeoutHandle.current = window.setTimeout(() => {
                      doToggleDirectoryExpansion(item.item.id, item.key, true)
                    }, DRAG_EXPAND_DELAY_MS)
                  }
                  // Required because `dragover` does not fire on `mouseenter`.
                  props.onDragOver?.(event)
                  onDragOver(event)
                }}
                onDragOver={(event) => {
                  if (state.category.type === categoryModule.CategoryType.trash) {
                    event.dataTransfer.dropEffect = 'none'
                  }
                  props.onDragOver?.(event)
                  onDragOver(event)
                }}
                onDragEnd={(event) => {
                  clearDragState()
                  props.onDragEnd?.(event)
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
                    clearDragState()
                  }
                  props.onDragLeave?.(event)
                }}
                onDrop={(event) => {
                  if (state.category.type !== categoryModule.CategoryType.trash) {
                    props.onDrop?.(event)
                    clearDragState()
                    const [directoryKey, directoryId] =
                      item.type === backendModule.AssetType.directory ?
                        [item.key, item.item.id]
                      : [item.directoryKey, item.directoryId]
                    const payload = drag.ASSET_ROWS.lookup(event)
                    if (
                      payload != null &&
                      payload.every((innerItem) => innerItem.key !== directoryKey)
                    ) {
                      event.preventDefault()
                      event.stopPropagation()
                      unsetModal()
                      doToggleDirectoryExpansion(directoryId, directoryKey, true)
                      const ids = payload
                        .filter((payloadItem) => payloadItem.asset.parentId !== directoryId)
                        .map((dragItem) => dragItem.key)
                      dispatchAssetEvent({
                        type: AssetEventType.move,
                        newParentKey: directoryKey,
                        newParentId: directoryId,
                        ids: new Set(ids),
                      })
                    } else if (event.dataTransfer.types.includes('Files')) {
                      event.preventDefault()
                      event.stopPropagation()
                      doToggleDirectoryExpansion(directoryId, directoryKey, true)
                      dispatchAssetListEvent({
                        type: AssetListEventType.uploadFiles,
                        parentKey: directoryKey,
                        parentId: directoryId,
                        files: Array.from(event.dataTransfer.files),
                      })
                    }
                  }
                }}
              >
                {columns.map((column) => {
                  // This is a React component even though it does not contain JSX.
                  // eslint-disable-next-line no-restricted-syntax
                  const Render = columnModule.COLUMN_RENDERER[column]
                  return (
                    <td key={column} className={columnUtils.COLUMN_CSS_CLASS[column]}>
                      <Render
                        keyProp={item.key}
                        isOpened={isOpened}
                        backendType={backend.type}
                        item={item}
                        setItem={setItem}
                        selected={selected}
                        setSelected={setSelected}
                        isSoleSelected={isSoleSelected}
                        state={state}
                        rowState={rowState}
                        setRowState={setRowState}
                        isEditable={state.category.type !== categoryModule.CategoryType.trash}
                      />
                    </td>
                  )
                })}
              </tr>
            </FocusRing>
          )}
          {selected && allowContextMenu && !hidden && (
            // This is a copy of the context menu, since the context menu registers keyboard
            // shortcut handlers. This is a bit of a hack, however it is preferable to duplicating
            // the entire context menu (once for the keyboard actions, once for the JSX).
            <AssetContextMenu
              hidden
              innerProps={{
                key: item.key,
                item,
                setItem,
                state,
                rowState,
                setRowState,
              }}
              rootDirectoryId={rootDirectoryId}
              event={{ pageX: 0, pageY: 0 }}
              eventTarget={null}
              doCopy={doCopy}
              doCut={doCut}
              doPaste={doPaste}
              doDelete={doDelete}
              doTriggerDescriptionEdit={doTriggerDescriptionEdit}
            />
          )}
        </>
      )
    }
    case backendModule.AssetType.specialLoading: {
      return hidden ? null : (
          <tr>
            <td colSpan={columns.length} className="border-r p-0 rounded-rows-skip-level">
              <div
                className={tailwindMerge.twMerge(
                  'flex h-table-row w-container items-center justify-center rounded-full rounded-rows-child',
                  indent.indentClass(item.depth),
                )}
              >
                <StatelessSpinner size={24} state={statelessSpinner.SpinnerState.loadingMedium} />
              </div>
            </td>
          </tr>
        )
    }
    case backendModule.AssetType.specialEmpty: {
      return hidden ? null : (
          <tr>
            <td colSpan={columns.length} className="border-r p-0 rounded-rows-skip-level">
              <div
                className={tailwindMerge.twMerge(
                  'flex h-table-row items-center rounded-full rounded-rows-child',
                  indent.indentClass(item.depth),
                )}
              >
                <img src={BlankIcon} />
                <aria.Text className="px-name-column-x placeholder">
                  {getText('thisFolderIsEmpty')}
                </aria.Text>
              </div>
            </td>
          </tr>
        )
    }
  }
}
