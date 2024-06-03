/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import BlankIcon from 'enso-assets/blank.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'
import Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import FocusRing from '#/components/styled/FocusRing'

import EditAssetDescriptionModal from '#/modals/EditAssetDescriptionModal'

import * as backendModule from '#/services/Backend'

import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as set from '#/utilities/set'
import Visibility from '#/utilities/Visibility'

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
export interface AssetRowProps
  extends Readonly<Omit<JSX.IntrinsicElements['tr'], 'onClick' | 'onContextMenu'>> {
  readonly item: backendModule.AnyAsset
  readonly state: assetsTable.AssetsTableState
  readonly hidden: boolean
  readonly columns: columnUtils.Column[]
  readonly selected: boolean
  readonly setSelected: (selected: boolean) => void
  readonly isSoleSelected: boolean
  readonly isKeyboardSelected: boolean
  readonly grabKeyboardFocus: () => void
  readonly allowContextMenu: boolean
  readonly onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
  readonly onContextMenu?: (
    props: AssetRowInnerProps,
    event: React.MouseEvent<HTMLTableRowElement>
  ) => void
}

/** A row containing an {@link backendModule.AnyAsset}. */
export default function AssetRow(props: AssetRowProps) {
  const { item, hidden: hiddenRaw, selected, isSoleSelected, isKeyboardSelected } = props
  const { setSelected, allowContextMenu, onContextMenu, state, columns, onClick } = props
  const { grabKeyboardFocus } = props
  const { backend, visibilities, scrollContainerRef, rootDirectoryId } = state
  const { setAssetPanelProps, doToggleDirectoryExpansion, doCopy, doCut, doPaste } = state
  const { setIsAssetPanelTemporarilyVisible } = state

  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const rootRef = React.useRef<HTMLElement | null>(null)
  const dragOverTimeoutHandle = React.useRef<number | null>(null)
  const grabKeyboardFocusRef = React.useRef(grabKeyboardFocus)
  grabKeyboardFocusRef.current = grabKeyboardFocus
  const [insertionVisibility, setInsertionVisibility] = React.useState(Visibility.visible)
  const [rowState, setRowState] = React.useState<assetsTable.AssetRowState>(() =>
    object.merge(assetRowUtils.INITIAL_ROW_STATE, { setVisibility: setInsertionVisibility })
  )
  const isCloud = backend.type === backendModule.BackendType.remote
  const outerVisibility = visibilities.get(item.id)
  const visibility =
    outerVisibility == null || outerVisibility === Visibility.visible
      ? insertionVisibility
      : outerVisibility
  const hidden = hiddenRaw || visibility === Visibility.hidden

  const updateAssetMutation = backendHooks.useBackendMutation(backend, 'updateAsset')
  const deleteAssetMutation = backendHooks.useBackendMutation(backend, 'deleteAsset')
  const undoDeleteAssetMutation = backendHooks.useBackendMutation(backend, 'undoDeleteAsset')
  const openProjectMutation = backendHooks.useBackendMutation(backend, 'openProject')
  const closeProjectMutation = backendHooks.useBackendMutation(backend, 'closeProject')
  const updateAssetMutate = updateAssetMutation.mutateAsync
  const deleteAssetMutate = deleteAssetMutation.mutateAsync
  const undoDeleteAssetMutate = undoDeleteAssetMutation.mutateAsync
  const openProjectMutate = openProjectMutation.mutateAsync
  const closeProjectMutate = closeProjectMutation.mutateAsync

  React.useEffect(() => {
    if (selected && insertionVisibility !== Visibility.visible) {
      setSelected(false)
    }
  }, [selected, insertionVisibility, /* should never change */ setSelected])

  React.useEffect(() => {
    if (isKeyboardSelected) {
      rootRef.current?.focus()
      grabKeyboardFocusRef.current()
    }
  }, [isKeyboardSelected])

  React.useEffect(() => {
    if (isSoleSelected) {
      setAssetPanelProps({ backend, item })
      setIsAssetPanelTemporarilyVisible(false)
    }
  }, [
    item,
    isSoleSelected,
    /* should never change */ backend,
    /* should never change */ setAssetPanelProps,
    /* should never change */ setIsAssetPanelTemporarilyVisible,
  ])

  const doDelete = React.useCallback(
    async (forever = false) => {
      setInsertionVisibility(Visibility.hidden)
      if (item.type === backendModule.AssetType.directory) {
        dispatchAssetListEvent({
          type: AssetListEventType.closeFolder,
          id: item.id,
          // This is SAFE, as this asset is already known to be a directory.
          // eslint-disable-next-line no-restricted-syntax
          key: item.id as backendModule.DirectoryId,
        })
      }
      try {
        dispatchAssetListEvent({ type: AssetListEventType.willDelete, key: item.id })
        if (
          item.type === backendModule.AssetType.project &&
          backend.type === backendModule.BackendType.local
        ) {
          if (
            item.projectState.type !== backendModule.ProjectState.placeholder &&
            item.projectState.type !== backendModule.ProjectState.closed
          ) {
            await openProjectMutate([item.id, null, item.title])
          }
          try {
            await closeProjectMutate([item.id, item.title])
          } catch {
            // Ignored. The project was already closed.
          }
        }
        await deleteAssetMutate([item.id, { force: forever, parentId: item.parentId }, item.title])
        dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.id })
      } catch (error) {
        setInsertionVisibility(Visibility.visible)
        toastAndLog('deleteAssetError', error, item.title)
      }
    },
    [
      backend.type,
      dispatchAssetListEvent,
      item,
      /* should never change */ openProjectMutate,
      /* should never change */ closeProjectMutate,
      /* should never change */ deleteAssetMutate,
      /* should never change */ item.id,
      /* should never change */ toastAndLog,
    ]
  )

  const doRestore = React.useCallback(async () => {
    // Visually, the asset is deleted from the Trash view.
    setInsertionVisibility(Visibility.hidden)
    try {
      await undoDeleteAssetMutate([item.id, item.title])
      dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.id })
    } catch (error) {
      setInsertionVisibility(Visibility.visible)
      toastAndLog('restoreAssetError', error, item.title)
    }
  }, [
    item,
    toastAndLog,
    /* should never change */ undoDeleteAssetMutate,
    /* should never change */ item.id,
  ])

  const doTriggerDescriptionEdit = React.useCallback(() => {
    setModal(
      <EditAssetDescriptionModal
        doChangeDescription={async description => {
          if (description !== item.description) {
            void updateAssetMutate([item.id, { parentDirectoryId: null, description }, item.title])
          }
        }}
        initialDescription={item.description}
      />
    )
  }, [setModal, item.description, backend, item.id, item.title])

  const clearDragState = React.useCallback(() => {
    setIsDraggedOver(false)
    setRowState(oldRowState =>
      oldRowState.temporarilyAddedLabels === set.EMPTY
        ? oldRowState
        : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
    )
  }, [])

  const onDragOver = (event: React.DragEvent<Element>) => {
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
          {!hidden && (
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
                  visibility,
                  (isDraggedOver || selected) && 'selected'
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
                      setSelected(false)
                    })
                    doToggleDirectoryExpansion(item.id, item.title)
                  }
                }}
                onContextMenu={event => {
                  if (allowContextMenu) {
                    event.preventDefault()
                    event.stopPropagation()
                    onContextMenu?.(innerProps, event)
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
                      />
                    )
                  } else {
                    onContextMenu?.(innerProps, event)
                  }
                }}
                onDragStart={event => {
                  if (rowState.isEditingName) {
                    event.preventDefault()
                  } else {
                    props.onDragStart?.(event)
                  }
                }}
                onDragEnter={event => {
                  if (dragOverTimeoutHandle.current != null) {
                    window.clearTimeout(dragOverTimeoutHandle.current)
                  }
                  if (item.type === backendModule.AssetType.directory) {
                    dragOverTimeoutHandle.current = window.setTimeout(() => {
                      doToggleDirectoryExpansion(item.id, item.title, true)
                    }, DRAG_EXPAND_DELAY_MS)
                  }
                  // Required because `dragover` does not fire on `mouseenter`.
                  props.onDragOver?.(event)
                  onDragOver(event)
                }}
                onDragOver={event => {
                  if (state.category === Category.trash) {
                    event.dataTransfer.dropEffect = 'none'
                  }

                  props.onDragOver?.(event)
                  onDragOver(event)
                }}
                onDragEnd={event => {
                  clearDragState()
                  props.onDragEnd?.(event)
                }}
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
                  props.onDragLeave?.(event)
                }}
                onDrop={event => {
                  if (state.category !== Category.trash) {
                    props.onDrop?.(event)
                    clearDragState()
                    const [directoryId, directoryTitle] =
                      item.type === backendModule.AssetType.directory
                        ? [item.id, item.title]
                        : [item.parentId, null]
                    const payload = drag.ASSET_ROWS.lookup(event)
                    if (
                      payload != null &&
                      payload.every(innerItem => innerItem.id !== directoryId)
                    ) {
                      event.preventDefault()
                      event.stopPropagation()
                      unsetModal()
                      doToggleDirectoryExpansion(directoryId, directoryTitle, true)
                      const ids = payload
                        .filter(payloadItem => payloadItem.parentId !== directoryId)
                        .map(dragItem => dragItem.id)
                      dispatchAssetEvent({
                        type: AssetEventType.move,
                        newParentId: directoryId,
                        ids: new Set(ids),
                      })
                    } else if (event.dataTransfer.types.includes('Files')) {
                      event.preventDefault()
                      event.stopPropagation()
                      doToggleDirectoryExpansion(directoryId, directoryTitle, true)
                      dispatchAssetListEvent({
                        type: AssetListEventType.uploadFiles,
                        parentId: directoryId,
                        files: Array.from(event.dataTransfer.files),
                      })
                    }
                  }
                }}
              >
                {columns.map(column => {
                  // This is a React component even though it does not contain JSX.
                  // eslint-disable-next-line no-restricted-syntax
                  const Render = columnModule.COLUMN_RENDERER[column]
                  return (
                    <td key={column} className={columnUtils.COLUMN_CSS_CLASS[column]}>
                      <Render
                        keyProp={key}
                        item={item}
                        setItem={setItem}
                        depth={item.depth}
                        selected={selected}
                        setSelected={setSelected}
                        isSoleSelected={isSoleSelected}
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
          )}
          {selected && allowContextMenu && !hidden && (
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
                'flex h-row w-container justify-center rounded-full rounded-rows-child',
                indent.indentClass(item.depth)
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
                'flex h-row items-center rounded-full rounded-rows-child',
                indent.indentClass(item.depth)
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
