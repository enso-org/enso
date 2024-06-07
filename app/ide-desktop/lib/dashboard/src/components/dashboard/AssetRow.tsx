/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import BlankIcon from 'enso-assets/blank.svg'

import * as mimeTypes from '#/data/mimeTypes'

import * as store from '#/store'

import * as backendHooks from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'
import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import NameColumn from '#/components/dashboard/column/NameColumn'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import FocusRing from '#/components/styled/FocusRing'

import DragModal from '#/modals/DragModal'

import * as backendModule from '#/services/Backend'

import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as set from '#/utilities/set'

// =================
// === Constants ===
// =================

/** The height of the header row. */
const HEADER_HEIGHT_PX = 34
/** The amount of time (in milliseconds) the drag item must be held over this component
 * to make a directory row expand. */
const DRAG_EXPAND_DELAY_MS = 500

// ================
// === AssetRow ===
// ================

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
  readonly item: backendModule.AnyAsset
  readonly state: assetsTable.AssetsTableState
  readonly rowState: assetsTable.AssetRowState
  readonly setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for an {@link AssetRow}. */
export interface AssetRowProps {
  readonly parentRef: React.RefObject<HTMLTableRowElement>
  readonly item: backendHooks.WithPlaceholder<backendModule.AnyAsset>
  readonly depth: number
  readonly state: assetsTable.AssetsTableState
  readonly columns: readonly columnUtils.Column[]
}

// FIXME: use `parentRef` - it should be focused on when left is pressed
/** A row containing an {@link backendModule.AnyAsset}. */
export default function AssetRow(props: AssetRowProps) {
  const { item, depth, state, columns } = props
  const { backend, scrollContainerRef, rootDirectoryId, doPaste } = state

  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const setIsAssetOpen = store.useStore(storeState => storeState.setIsAssetOpen)
  const toggleIsAssetOpen = store.useStore(storeState => storeState.toggleIsAssetOpen)
  const setIsAssetSelected = store.useStore(storeState => storeState.setIsAssetSelected)
  const setAssetsTemporaryLabels = store.useStore(storeState => storeState.setAssetsTemporaryLabels)
  const getSelectedAssetIds = store.useStore(storeState => storeState.getSelectedAssetIds)
  const setSelectedAssetIds = store.useStore(storeState => storeState.setSelectedAssetIds)
  const isSelected = store.useStore(
    storeState => storeState.getAssetState(backend.type, item.id).isSelected
  )
  const isSoleSelected = store.useStore(
    storeState => isSelected && storeState.backends[backend.type].selectedCount === 1
  )
  const areNoKeysSelected = store.useStore(
    storeState => storeState.backends[backend.type].selectedCount === 0
  )
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const rootRef = React.useRef<HTMLElement | null>(null)
  const dragOverTimeoutHandle = React.useRef<number | null>(null)
  const [rowState, setRowState] = React.useState<assetsTable.AssetRowState>(
    assetRowUtils.INITIAL_ROW_STATE
  )
  const allowContextMenu = areNoKeysSelected || !isSelected || isSoleSelected

  const uploadFilesMutation = backendHooks.useBackendUploadFilesMutation(backend)
  const updateAssetMutation = backendHooks.useBackendMutation(backend, 'updateAsset')

  const clearDragState = () => {
    setIsDraggedOver(false)
    setRowState(oldRowState =>
      oldRowState.temporarilyAddedLabels === set.EMPTY
        ? oldRowState
        : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
    )
  }

  const onDragStart = (event: React.DragEvent<Element>) => {
    if (rowState.isEditingName) {
      event.preventDefault()
    } else {
      let newSelectedKeys = getSelectedAssetIds(backend.type)
      if (!newSelectedKeys.includes(item.id)) {
        newSelectedKeys = [item.id]
        setSelectedAssetIds(backend.type, newSelectedKeys)
      }
      const assets = rootDirectoryEntries
        .preorderTraversal()
        .filter(node => newSelectedKeys.has(node.id))
      const payload: drag.AssetRowsDragPayload = assets.map(node => ({
        key: node.key,
        asset: node,
      }))
      event.dataTransfer.setData(
        mimeTypes.ASSETS_MIME_TYPE,
        JSON.stringify(assets.map(node => node.id))
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
          {assets.map(asset => (
            <NameColumn
              key={asset.id}
              item={asset}
              state={state}
              // Default states.
              depth={0}
              rowState={assetRowUtils.INITIAL_ROW_STATE}
              // The drag placeholder cannot be interacted with.
              setRowState={() => {}}
              isEditable={false}
            />
          ))}
        </DragModal>
      )
    }
  }

  const onDragOver = (event: React.DragEvent<Element>) => {
    if (state.category === Category.trash) {
      event.dataTransfer.dropEffect = 'none'
    }
    const directoryKey = item.type === backendModule.AssetType.directory ? item.id : item.parentId
    const payload = drag.ASSET_ROWS.lookup(event)
    if (
      (payload != null && payload.every(innerItem => innerItem.id !== directoryKey)) ||
      event.dataTransfer.types.includes('Files')
    ) {
      event.preventDefault()
      if (item.type === backendModule.AssetType.directory && state.category !== Category.trash) {
        setIsDraggedOver(true)
      }
    }
    const labelsPayload = drag.LABELS.lookup(event)
    if (labelsPayload != null) {
      event.preventDefault()
      event.stopPropagation()
      const storeState = store.useStore.getState()
      const assetState = storeState.getAssetState(backend.type, item.id)
      const ids = !assetState.isSelected ? [item.id] : storeState.getSelectedAssetIds(backend.type)
      setAssetsTemporaryLabels(backend.type, ids, {
        type: event.shiftKey ? 'remove' : 'add',
        labels: labelsPayload,
      })
    }
  }

  const onDragEnd = () => {
    clearDragState()
    setAssetsTemporaryLabels(backend.type, [], null)
  }

  const onDrop = (event: React.DragEvent<Element>) => {
    if (state.category === Category.trash) {
      return
    } else {
      clearDragState()
      const directoryId = item.type === backendModule.AssetType.directory ? item.id : item.parentId
      const payload = drag.ASSET_ROWS.lookup(event)
      if (payload != null && payload.every(innerItem => innerItem.id !== directoryId)) {
        event.preventDefault()
        event.stopPropagation()
        unsetModal()
        setIsAssetOpen(backend.type, directoryId, true)
        const ids = payload
          .filter(payloadItem => payloadItem.parentId !== directoryId)
          .map(dragItem => dragItem.id)
        for (const id of ids) {
          updateAssetMutation.mutate([id, { description: null, parentDirectoryId: directoryId }])
        }
      } else if (event.dataTransfer.types.includes('Files')) {
        event.preventDefault()
        event.stopPropagation()
        setIsAssetOpen(backend.type, directoryId, true)
        uploadFilesMutation.mutate([event.dataTransfer.files, directoryId])
      }
      const labelsPayload = drag.LABELS.lookup(event)
      if (labelsPayload != null) {
        event.preventDefault()
        event.stopPropagation()
        const storeState = store.useStore.getState()
        const assetState = storeState.getAssetState(backend.type, item.id)
        const ids = !assetState.isSelected
          ? [item.id]
          : storeState.getSelectedAssetIds(backend.type)
        if (event.shiftKey) {
          for (const id of ids) {
            // FIXME: remove label(s) mutation
          }
        } else {
          for (const id of ids) {
            // FIXME: add label(s) mutation
          }
        }
      }
      setAssetsTemporaryLabels(backend.type, [], null)
      return
    }
  }

  switch (item.type) {
    case backendModule.AssetType.directory:
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.secret: {
      const innerProps: AssetRowInnerProps = { item, state, rowState, setRowState }
      return (
        <>
          <FocusRing>
            <tr
              draggable
              tabIndex={0}
              ref={element => {
                rootRef.current = element
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
                if (isKeyboardSelected && element?.contains(document.activeElement) === false) {
                  element.focus()
                }
              }}
              className={tailwindMerge.twMerge(
                'h-row rounded-full transition-all ease-in-out rounded-rows-child',
                item.isPlaceholder && 'placeholder',
                (isDraggedOver || isSelected) && 'selected'
              )}
              onClick={event => {
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
                    setIsAssetSelected(backend.type, item.id, false)
                  })
                  toggleIsAssetOpen(backend.type, item.id)
                }
              }}
              onContextMenu={event => {
                if (allowContextMenu) {
                  event.preventDefault()
                  event.stopPropagation()
                  setModal(
                    <AssetContextMenu
                      innerProps={innerProps}
                      rootDirectoryId={rootDirectoryId}
                      event={event}
                      eventTarget={
                        event.target instanceof HTMLElement ? event.target : event.currentTarget
                      }
                      doPaste={doPaste}
                    />
                  )
                }
              }}
              onDragStart={onDragStart}
              onDragEnter={event => {
                if (dragOverTimeoutHandle.current != null) {
                  window.clearTimeout(dragOverTimeoutHandle.current)
                }
                if (item.type === backendModule.AssetType.directory) {
                  dragOverTimeoutHandle.current = window.setTimeout(() => {
                    setIsAssetOpen(backend.type, item.id, true)
                  }, DRAG_EXPAND_DELAY_MS)
                }
                // Required because `dragover` does not fire on `mouseenter`.
                onDragOver(event)
              }}
              onDragOver={onDragOver}
              onDragEnd={onDragEnd}
              onDragLeave={event => {
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
              }}
              onDrop={onDrop}
            >
              {columns.map(column => {
                // This is a React component even though it does not contain JSX.
                // eslint-disable-next-line no-restricted-syntax
                const Render = columnModule.COLUMN_RENDERER[column]
                return (
                  <td key={column} className={columnUtils.COLUMN_CSS_CLASS[column]}>
                    <Render
                      item={item}
                      depth={depth}
                      state={state}
                      rowState={rowState}
                      setRowState={setRowState}
                      isEditable={state.category !== Category.trash}
                    />
                  </td>
                )
              })}
            </tr>
          </FocusRing>
          {isSelected && allowContextMenu && (
            // This is a copy of the context menu, since the context menu registers keyboard
            // shortcut handlers. This is a bit of a hack, however it is preferable to duplicating
            // the entire context menu (once for the keyboard actions, once for the JSX).
            <AssetContextMenu
              hidden
              innerProps={{
                item,
                state,
                rowState,
                setRowState,
              }}
              rootDirectoryId={rootDirectoryId}
              event={{ pageX: 0, pageY: 0 }}
              eventTarget={null}
              doPaste={doPaste}
            />
          )}
        </>
      )
    }
    case backendModule.AssetType.specialLoading: {
      return (
        <tr>
          <td colSpan={columns.length} className="border-r p-0 rounded-rows-skip-level">
            <div
              className={tailwindMerge.twMerge(
                'flex h-row w-container justify-center rounded-full rounded-rows-child',
                indent.indentClass(depth)
              )}
            >
              <StatelessSpinner size={24} state={statelessSpinner.SpinnerState.loadingMedium} />
            </div>
          </td>
        </tr>
      )
    }
    case backendModule.AssetType.specialEmpty: {
      return (
        <tr>
          <td colSpan={columns.length} className="border-r p-0 rounded-rows-skip-level">
            <div
              className={tailwindMerge.twMerge(
                'flex h-row items-center rounded-full rounded-rows-child',
                indent.indentClass(depth)
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
