/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import * as eventHooks from '#/hooks/eventHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'

import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import * as backendModule from '#/services/Backend'

import AssetTreeNode from '#/utilities/AssetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as download from '#/utilities/download'
import * as drag from '#/utilities/drag'
import * as errorModule from '#/utilities/error'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
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
/** Placeholder row for directories that are empty. */
const EMPTY_DIRECTORY_PLACEHOLDER = (
  <span className="px-name-column-x placeholder">This folder is empty.</span>
)

// ================
// === AssetRow ===
// ================

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
  readonly key: backendModule.AssetId
  readonly item: AssetTreeNode
  readonly setItem: React.Dispatch<React.SetStateAction<AssetTreeNode>>
  readonly state: assetsTable.AssetsTableState
  readonly rowState: assetsTable.AssetRowState
  readonly setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for an {@link AssetRow}. */
export interface AssetRowProps {
  readonly item: AssetTreeNode
  readonly state: assetsTable.AssetsTableState
  readonly visibility: Visibility | null
  readonly columns: columnUtils.Column[]
  readonly selected: boolean
  readonly setSelected: (selected: boolean) => void
  readonly isSoleSelected: boolean
  readonly isKeyboardSelected: boolean
  readonly allowContextMenu: boolean
  readonly onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
  readonly onContextMenu: (
    props: AssetRowInnerProps,
    event: React.MouseEvent<HTMLTableRowElement>
  ) => void
  readonly onDragStart: React.DragEventHandler<HTMLTableRowElement>
  readonly onDragOver: React.DragEventHandler<HTMLTableRowElement>
  readonly onDragEnd: React.DragEventHandler<HTMLTableRowElement>
  readonly onDrop: React.DragEventHandler<HTMLTableRowElement>
}

/** A row containing an {@link backendModule.AnySmartAsset}. */
export default function AssetRow(props: AssetRowProps) {
  const { item: rawItem, visibility: visibilityRaw, selected, setSelected, isSoleSelected } = props
  const { isKeyboardSelected, allowContextMenu, onContextMenu, state, columns, onClick } = props
  const { onDragStart, onDragOver: onDragOverRaw, onDragEnd, onDrop } = props
  const { nodeMap, rootDirectory, assetEvents, dispatchAssetEvent, dispatchAssetListEvent } = state
  const { isCloud, setAssetPanelProps, doToggleDirectoryExpansion, doCopy, doCut, doPaste } = state
  const { scrollContainerRef, setIsAssetPanelTemporarilyVisible } = state

  const { user, userInfo } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const [item, setItem] = React.useState(rawItem)
  const dragOverTimeoutHandle = React.useRef<number | null>(null)
  const smartAsset = item.item
  const asset = smartAsset.value
  const [insertionVisibility, setInsertionVisibility] = React.useState(Visibility.visible)
  const [rowState, setRowState] = React.useState<assetsTable.AssetRowState>(() =>
    object.merge(assetRowUtils.INITIAL_ROW_STATE, { setVisibility: setInsertionVisibility })
  )
  const key = AssetTreeNode.getKey(item)
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const visibility =
    visibilityRaw == null || visibilityRaw === Visibility.visible
      ? insertionVisibility
      : visibilityRaw
  const hidden = visibility === Visibility.hidden

  React.useEffect(() => {
    setItem(rawItem)
  }, [rawItem])

  // Materialize the asset on the backend. If it already exists, this will not send a request to
  // the backend.
  React.useEffect(() => {
    const materializedOrPromise = smartAsset.materialize()
    if (!(materializedOrPromise instanceof Promise)) {
      setAsset(materializedOrPromise.value)
    } else {
      void (async () => {
        try {
          rowState.setVisibility(Visibility.faded)
          const materialized = await materializedOrPromise
          rowState.setVisibility(Visibility.visible)
          setAsset(materialized.value)
          if (
            backendModule.assetIsProject(asset) &&
            asset.projectState.type === backendModule.ProjectState.placeholder &&
            backendModule.assetIsProject(materialized.value)
          ) {
            dispatchAssetEvent({
              type: AssetEventType.openProject,
              id: materialized.value.id,
              shouldAutomaticallySwitchPage: true,
              runInBackground: false,
            })
          }
        } catch (error) {
          rowState.setVisibility(Visibility.visible)
        }
      })()
    }
    // This MUST only run once, on initialization.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  React.useEffect(() => {
    // Mutation is HIGHLY INADVISABLE in React, however it is useful here as we want to avoid
    // re - rendering the parent.
    // @ts-expect-error Because `smartAsset` is of an unknown type, its parameter is contravariant.
    // However, this is safe because the type of an asset cannot change.
    rawItem.item = smartAsset.withValue(asset)
    // FIXME: Must this be omitted?
    // `smartAsset` is NOT a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [asset, rawItem])

  React.useEffect(() => {
    if (selected && insertionVisibility !== Visibility.visible) {
      setSelected(false)
    }
  }, [selected, insertionVisibility, /* should never change */ setSelected])

  const doCopyOnBackend = React.useCallback(
    async (newParentId: backendModule.DirectoryId | null) => {
      try {
        setAsset(oldAsset =>
          object.merge(oldAsset, {
            title: oldAsset.title + ' (copy)',
            labels: [],
            permissions: permissions.tryGetSingletonOwnerPermission(user, userInfo),
            modifiedAt: dateTime.toRfc3339(new Date()),
          })
        )
        newParentId ??= rootDirectory.value.id
        const copiedAsset = await smartAsset.copy(
          newParentId,
          nodeMap.current.get(newParentId)?.item.value.title ?? '(unknown)'
        )
        setAsset(
          // This is SAFE, as the type of the copied asset is guaranteed to be the same
          // as the type of the original asset.
          // eslint-disable-next-line no-restricted-syntax
          object.merger(copiedAsset.asset as Partial<backendModule.AnyAsset>)
        )
      } catch (error) {
        toastAndLog(`Could not copy '${asset.title}'`, error)
        // Delete the new component representing the asset that failed to insert.
        dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
      }
    },
    [
      rootDirectory.value.id,
      user,
      userInfo,
      smartAsset,
      asset,
      item.key,
      /* should never change */ nodeMap,
      /* should never change */ setAsset,
      /* should never change */ toastAndLog,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const doMove = React.useCallback(
    async (
      newParentKey: backendModule.AssetId | null,
      newParent: backendModule.SmartDirectory | null
    ) => {
      const nonNullNewParentKey = newParentKey ?? rootDirectory.value.id
      const nonNullNewParent = newParent ?? rootDirectory
      try {
        dispatchAssetListEvent({
          type: AssetListEventType.move,
          newParentKey: nonNullNewParentKey,
          newParent: nonNullNewParent,
          key: item.key,
          item: smartAsset,
        })
        setItem(oldItem =>
          oldItem.with({ directoryKey: nonNullNewParentKey, directory: nonNullNewParent })
        )
        setAsset(object.merger({ parentId: nonNullNewParent.value.id }))
        await smartAsset.update({ parentDirectoryId: nonNullNewParent.value.id })
      } catch (error) {
        toastAndLog(`Could not move '${smartAsset.value.title}'`, error)
        setAsset(object.merger({ parentId: asset.parentId }))
        setItem(oldItem =>
          oldItem.with({ directoryKey: item.directoryKey, directory: item.directory })
        )
        // Move the asset back to its original position.
        dispatchAssetListEvent({
          type: AssetListEventType.move,
          newParentKey: item.directoryKey,
          newParent: item.directory,
          key: item.key,
          item: smartAsset,
        })
      }
    },
    [
      rootDirectory,
      smartAsset,
      asset.parentId,
      item.directory,
      item.directoryKey,
      item.key,
      /* should never change */ setAsset,
      /* should never change */ toastAndLog,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  React.useEffect(() => {
    if (isSoleSelected) {
      setAssetPanelProps({ item, setItem })
      setIsAssetPanelTemporarilyVisible(false)
    }
  }, [
    item,
    isSoleSelected,
    /* should never change */ setAssetPanelProps,
    /* should never change */ setIsAssetPanelTemporarilyVisible,
  ])

  const doDelete = React.useCallback(
    async (forever = false) => {
      setInsertionVisibility(Visibility.hidden)
      if (smartAsset.type === backendModule.AssetType.directory) {
        dispatchAssetListEvent({
          type: AssetListEventType.closeFolder,
          folder: smartAsset,
          // This is SAFE, as this asset is already known to be a directory.
          // eslint-disable-next-line no-restricted-syntax
          key: item.key as backendModule.DirectoryId,
        })
      }
      try {
        dispatchAssetListEvent({ type: AssetListEventType.willDelete, key: item.key })
        if (smartAsset.type === backendModule.AssetType.project && !isCloud) {
          if (
            smartAsset.value.projectState.type !== backendModule.ProjectState.placeholder &&
            smartAsset.value.projectState.type !== backendModule.ProjectState.closed
          ) {
            await smartAsset.open()
          }
          try {
            await smartAsset.close()
          } catch {
            // Ignored. The project was already closed.
          }
        }
        await smartAsset.delete(forever)
        dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
      } catch (error) {
        setInsertionVisibility(Visibility.visible)
        toastAndLog(
          errorModule.tryGetMessage(error)?.slice(0, -1) ??
            `Could not delete ${backendModule.ASSET_TYPE_NAME[smartAsset.type]}`
        )
      }
    },
    [
      isCloud,
      dispatchAssetListEvent,
      smartAsset,
      /* should never change */ item.key,
      /* should never change */ toastAndLog,
    ]
  )

  const doRestore = React.useCallback(async () => {
    // Visually, the asset is deleted from the Trash view.
    setInsertionVisibility(Visibility.hidden)
    try {
      await smartAsset.undoDelete()
      dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
    } catch (error) {
      setInsertionVisibility(Visibility.visible)
      toastAndLog(`Unable to restore ${backendModule.ASSET_TYPE_NAME[smartAsset.type]}`, error)
    }
  }, [
    dispatchAssetListEvent,
    smartAsset,
    /* should never change */ item.key,
    /* should never change */ toastAndLog,
  ])

  eventHooks.useEventHandler(assetEvents, async event => {
    switch (event.type) {
      // These events are handled in the specific `NameColumn` files.
      case AssetEventType.updateFiles:
      case AssetEventType.openProject:
      case AssetEventType.closeProject: {
        break
      }
      case AssetEventType.copy: {
        if (event.ids.has(item.key)) {
          await doCopyOnBackend(event.newParent.value.id)
        }
        break
      }
      case AssetEventType.cut: {
        if (event.ids.has(item.key)) {
          setInsertionVisibility(Visibility.faded)
        }
        break
      }
      case AssetEventType.cancelCut: {
        if (event.ids.has(item.key)) {
          setInsertionVisibility(Visibility.visible)
        }
        break
      }
      case AssetEventType.move: {
        if (event.ids.has(item.key)) {
          setInsertionVisibility(Visibility.visible)
          await doMove(event.newParentKey, event.newParent)
        }
        break
      }
      case AssetEventType.delete: {
        if (event.ids.has(item.key)) {
          await doDelete(false)
        }
        break
      }
      case AssetEventType.deleteForever: {
        if (event.ids.has(item.key)) {
          await doDelete(true)
        }
        break
      }
      case AssetEventType.restore: {
        if (event.ids.has(item.key)) {
          await doRestore()
        }
        break
      }
      case AssetEventType.download:
      case AssetEventType.downloadSelected: {
        if (event.type === AssetEventType.downloadSelected ? selected : event.ids.has(item.key)) {
          if (isCloud) {
            switch (smartAsset.type) {
              case backendModule.AssetType.project: {
                try {
                  const details = await smartAsset.getDetails()
                  if (details.url != null) {
                    download.download(details.url, asset.title)
                  } else {
                    toastAndLog(
                      `Could not download project '${asset.title}': project has no source files`
                    )
                  }
                } catch (error) {
                  toastAndLog(`Could not download project '${asset.title}'`, error)
                }
                break
              }
              case backendModule.AssetType.file: {
                try {
                  const details = await smartAsset.getDetails()
                  if (details.url != null) {
                    download.download(details.url, asset.title)
                  } else {
                    toastAndLog(`Could not download file '${asset.title}': file not found`)
                  }
                } catch (error) {
                  toastAndLog(`Could not download file '${asset.title}'`, error)
                }
                break
              }
              case backendModule.AssetType.dataLink: {
                try {
                  const value = await smartAsset.getValue()
                  const fileName = `${asset.title}.datalink`
                  download.download(
                    URL.createObjectURL(
                      new File([JSON.stringify(value)], fileName, {
                        type: 'application/json+x-enso-data-link',
                      })
                    ),
                    fileName
                  )
                } catch (error) {
                  toastAndLog(`Could not download Data Link '${asset.title}'`, error)
                }
                break
              }
              default: {
                toastAndLog('You can only download files and Data Links')
                break
              }
            }
          } else {
            download.download(
              `./api/project-manager/projects/${asset.id}/enso-project`,
              `${asset.title}.enso-project`
            )
          }
        }
        break
      }
      case AssetEventType.removeSelf: {
        // This is not triggered from the asset list, so it uses `item.id` instead of `key`.
        if (event.id === asset.id && userInfo != null) {
          setInsertionVisibility(Visibility.hidden)
          try {
            await smartAsset.setPermissions({ action: null, userSubjects: [userInfo.id] })
            dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
          } catch (error) {
            setInsertionVisibility(Visibility.visible)
            toastAndLog(null, error)
          }
        }
        break
      }
      case AssetEventType.temporarilyAddLabels: {
        const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY
        setRowState(oldRowState =>
          oldRowState.temporarilyAddedLabels === labels &&
          oldRowState.temporarilyRemovedLabels === set.EMPTY
            ? oldRowState
            : object.merge(oldRowState, {
                temporarilyAddedLabels: labels,
                temporarilyRemovedLabels: set.EMPTY,
              })
        )
        break
      }
      case AssetEventType.temporarilyRemoveLabels: {
        const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY
        setRowState(oldRowState =>
          oldRowState.temporarilyAddedLabels === set.EMPTY &&
          oldRowState.temporarilyRemovedLabels === labels
            ? oldRowState
            : object.merge(oldRowState, {
                temporarilyAddedLabels: set.EMPTY,
                temporarilyRemovedLabels: labels,
              })
        )
        break
      }
      case AssetEventType.addLabels: {
        setRowState(oldRowState =>
          oldRowState.temporarilyAddedLabels === set.EMPTY
            ? oldRowState
            : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
        )
        const labels = asset.labels
        if (
          event.ids.has(item.key) &&
          (labels == null || [...event.labelNames].some(label => !labels.includes(label)))
        ) {
          const newLabels = [
            ...(labels ?? []),
            ...[...event.labelNames].filter(label => labels?.includes(label) !== true),
          ]
          setAsset(object.merger({ labels: newLabels }))
          try {
            await smartAsset.setTags(newLabels)
          } catch (error) {
            setAsset(object.merger({ labels }))
            toastAndLog(null, error)
          }
        }
        break
      }
      case AssetEventType.removeLabels: {
        setRowState(oldRowState =>
          oldRowState.temporarilyAddedLabels === set.EMPTY
            ? oldRowState
            : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
        )
        const labels = asset.labels
        if (
          event.ids.has(item.key) &&
          labels != null &&
          [...event.labelNames].some(label => labels.includes(label))
        ) {
          const newLabels = labels.filter(label => !event.labelNames.has(label))
          setAsset(object.merger({ labels: newLabels }))
          try {
            await smartAsset.setTags(newLabels)
          } catch (error) {
            setAsset(object.merger({ labels }))
            toastAndLog(null, error)
          }
        }
        break
      }
      case AssetEventType.deleteLabel: {
        setAsset(oldAsset => {
          // The IIFE is required to prevent TypeScript from narrowing this value.
          let found = (() => false)()
          const labels =
            oldAsset.labels?.filter(label => {
              if (label === event.labelName) {
                found = true
                return false
              } else {
                return true
              }
            }) ?? null
          return found ? object.merge(oldAsset, { labels }) : oldAsset
        })
        break
      }
    }
  })

  const clearDragState = React.useCallback(() => {
    setIsDraggedOver(false)
    setRowState(oldRowState =>
      oldRowState.temporarilyAddedLabels === set.EMPTY
        ? oldRowState
        : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
    )
  }, [])

  const onDragOver = (event: React.DragEvent<HTMLTableRowElement>) => {
    onDragOverRaw(event)
    const directoryId =
      item.item.type === backendModule.AssetType.directory
        ? item.item.value.id
        : item.directory.value.id
    const payload = drag.ASSET_ROWS.lookup(event)
    if (
      (payload != null && payload.every(innerItem => innerItem.asset.parentId !== directoryId)) ||
      event.dataTransfer.types.includes('Files')
    ) {
      event.preventDefault()
      if (item.item.type === backendModule.AssetType.directory) {
        setIsDraggedOver(true)
      }
    }
  }

  switch (asset.type) {
    case backendModule.AssetType.directory:
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.dataLink:
    case backendModule.AssetType.secret: {
      const innerProps: AssetRowInnerProps = {
        key,
        item,
        setItem,
        state,
        rowState,
        setRowState,
      }
      return (
        <>
          {!hidden && (
            <tr
              draggable
              tabIndex={-1}
              ref={element => {
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
              }}
              className={`h-row rounded-full outline-2 -outline-offset-2 outline-primary ease-in-out ${visibility} ${
                isKeyboardSelected ? 'outline' : ''
              } ${isDraggedOver || selected ? 'selected' : ''}`}
              onClick={event => {
                unsetModal()
                onClick(innerProps, event)
                if (
                  smartAsset.type === backendModule.AssetType.directory &&
                  eventModule.isDoubleClick(event) &&
                  !rowState.isEditingName
                ) {
                  // This must be processed on the next tick, otherwise it will be overridden
                  // by the default click handler.
                  window.setTimeout(() => {
                    setSelected(false)
                  })
                  doToggleDirectoryExpansion(smartAsset, item.key)
                }
              }}
              onContextMenu={event => {
                if (allowContextMenu) {
                  event.preventDefault()
                  event.stopPropagation()
                  onContextMenu(innerProps, event)
                  setModal(
                    <AssetContextMenu
                      isCloud={isCloud}
                      innerProps={innerProps}
                      event={event}
                      eventTarget={
                        event.target instanceof HTMLElement ? event.target : event.currentTarget
                      }
                      doCopy={doCopy}
                      doCut={doCut}
                      doPaste={doPaste}
                      doDelete={doDelete}
                    />
                  )
                } else {
                  onContextMenu(innerProps, event)
                }
              }}
              onDragStart={event => {
                if (rowState.isEditingName || !isCloud) {
                  event.preventDefault()
                } else {
                  onDragStart(event)
                }
              }}
              onDragEnter={event => {
                if (dragOverTimeoutHandle.current != null) {
                  window.clearTimeout(dragOverTimeoutHandle.current)
                }
                if (smartAsset.type === backendModule.AssetType.directory) {
                  dragOverTimeoutHandle.current = window.setTimeout(() => {
                    doToggleDirectoryExpansion(smartAsset, item.key, true)
                  }, DRAG_EXPAND_DELAY_MS)
                }
                // Required because `dragover` does not fire on `mouseenter`.
                onDragOver(event)
              }}
              onDragOver={onDragOver}
              onDragEnd={event => {
                clearDragState()
                onDragEnd(event)
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
              }}
              onDrop={event => {
                clearDragState()
                onDrop(event)
                const [newParentKey, newParent] =
                  item.item.type === backendModule.AssetType.directory
                    ? [item.key, item.item]
                    : [item.directoryKey, item.directory]
                const payload = drag.ASSET_ROWS.lookup(event)
                if (payload != null && payload.every(innerItem => innerItem.key !== newParentKey)) {
                  event.preventDefault()
                  event.stopPropagation()
                  unsetModal()
                  doToggleDirectoryExpansion(newParent, newParentKey, true)
                  const ids = new Set(payload.map(dragItem => dragItem.key))
                  dispatchAssetEvent({ type: AssetEventType.move, newParentKey, newParent, ids })
                } else if (event.dataTransfer.types.includes('Files')) {
                  event.preventDefault()
                  event.stopPropagation()
                  doToggleDirectoryExpansion(newParent, newParentKey, true)
                  dispatchAssetListEvent({
                    type: AssetListEventType.uploadFiles,
                    // This is SAFE, as it is guarded by the condition above:
                    // `item.item.type === backendModule.AssetType.directory`
                    // eslint-disable-next-line no-restricted-syntax
                    parentKey: newParentKey as backendModule.DirectoryId,
                    parent: newParent,
                    files: Array.from(event.dataTransfer.files),
                  })
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
                      item={item}
                      setItem={setItem}
                      selected={selected}
                      setSelected={setSelected}
                      isSoleSelected={isSoleSelected}
                      state={state}
                      rowState={rowState}
                      setRowState={setRowState}
                    />
                  </td>
                )
              })}
            </tr>
          )}
          {selected && allowContextMenu && !hidden && (
            // This is a copy of the context menu, since the context menu registers keyboard
            // shortcut handlers. This is a bit of a hack, however it is preferable to duplicating
            // the entire context menu (once for the keyboard actions, once for the JSX).
            <AssetContextMenu
              hidden
              isCloud={isCloud}
              innerProps={{
                key,
                item,
                setItem,
                state,
                rowState,
                setRowState,
              }}
              event={{ pageX: 0, pageY: 0 }}
              eventTarget={null}
              doCopy={doCopy}
              doCut={doCut}
              doPaste={doPaste}
              doDelete={doDelete}
            />
          )}
        </>
      )
    }
    case backendModule.AssetType.specialLoading: {
      return hidden ? null : (
        <tr>
          <td colSpan={columns.length} className="border-r p rounded-rows-skip-level">
            <div
              className={`flex h-row w-container justify-center rounded-full ${indent.indentClass(
                item.depth
              )}`}
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
          <td colSpan={columns.length} className="border-r p rounded-rows-skip-level">
            <div
              className={`flex h-row items-center rounded-full ${indent.indentClass(item.depth)}`}
            >
              <img src={BlankIcon} />
              {EMPTY_DIRECTORY_PLACEHOLDER}
            </div>
          </td>
        </tr>
      )
    }
  }
}
