/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import * as eventHooks from '#/hooks/eventHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

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
import * as localBackend from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'

import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import AssetTreeNode from '#/utilities/AssetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as download from '#/utilities/download'
import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as fileInfo from '#/utilities/fileInfo'
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
export interface AssetRowProps {
  readonly item: assetTreeNode.AnyAssetTreeNode
  readonly state: assetsTable.AssetsTableState
  readonly visibility: Visibility | null
  readonly columns: columnUtils.Column[]
  readonly selected: boolean
  readonly setSelected: (selected: boolean) => void
  readonly isSoleSelected: boolean
  readonly isKeyboardSelected: boolean
  readonly grabKeyboardFocus: () => void
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
  const { item: rawItem, visibility: visibilityRaw, selected, isSoleSelected } = props
  const { isKeyboardSelected, setSelected, allowContextMenu, state, columns } = props
  const { onContextMenu, onClick, grabKeyboardFocus } = props
  const { isCloud, rootDirectory, assetEvents, dispatchAssetEvent, dispatchAssetListEvent } = state
  const { setAssetPanelProps, doToggleDirectoryExpansion, doCopy, doCut, doPaste } = state
  const { setIsAssetPanelTemporarilyVisible, scrollContainerRef, nodeMap, category } = state

  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const [item, setItem] = React.useState(rawItem)
  const rootRef = React.useRef<HTMLElement | null>(null)
  const dragOverTimeoutHandle = React.useRef<number | null>(null)
  const smartAsset = item.item
  const asset = smartAsset.value
  const grabKeyboardFocusRef = React.useRef(grabKeyboardFocus)
  grabKeyboardFocusRef.current = grabKeyboardFocus
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

  React.useEffect(() => {
    if (isKeyboardSelected) {
      rootRef.current?.focus()
      grabKeyboardFocusRef.current()
    }
  }, [isKeyboardSelected])

  const doCopyOnBackend = React.useCallback(
    async (newParentId: backendModule.DirectoryId | null) => {
      try {
        setAsset(oldAsset =>
          object.merge(oldAsset, {
            title: oldAsset.title + ' (copy)',
            labels: [],
            permissions: permissions.tryGetSingletonOwnerPermission(user),
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
        toastAndLog('copyAssetError', error, smartAsset.value.title)
        // Delete the new component representing the asset that failed to insert.
        dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
      }
    },
    [
      rootDirectory.value.id,
      user,
      smartAsset,
      item.key,
      toastAndLog,
      /* should never change */ nodeMap,
      /* should never change */ setAsset,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const doMove = React.useCallback(
    async (
      newParentKey: backendModule.DirectoryId | null,
      newParent: backendModule.SmartDirectory | null
    ) => {
      const nonNullNewParentKey = newParentKey ?? rootDirectory.value.id
      const nonNullNewParent = newParent ?? rootDirectory
      try {
        setItem(oldItem =>
          oldItem.with({ directoryKey: nonNullNewParentKey, directory: nonNullNewParent })
        )
        const newParentPath = localBackend.extractTypeAndId(nonNullNewParent.value.id).id
        const newProjectState =
          smartAsset.value.projectState == null
            ? null
            : object.merge(
                smartAsset.value.projectState,
                smartAsset.value.projectState.path == null
                  ? {}
                  : {
                      path: projectManager.joinPath(
                        newParentPath,
                        fileInfo.fileName(smartAsset.value.projectState.path)
                      ),
                    }
              )
        let newId = smartAsset.value.id
        if (!isCloud) {
          const oldPath = localBackend.extractTypeAndId(newId).id
          const newPath = projectManager.joinPath(newParentPath, fileInfo.fileName(oldPath))
          switch (smartAsset.value.type) {
            case backendModule.AssetType.file: {
              newId = localBackend.newFileId(newPath)
              break
            }
            case backendModule.AssetType.directory: {
              newId = localBackend.newDirectoryId(newPath)
              break
            }
            case backendModule.AssetType.project:
            case backendModule.AssetType.secret:
            case backendModule.AssetType.dataLink:
            case backendModule.AssetType.specialLoading:
            case backendModule.AssetType.specialEmpty: {
              // Ignored.
              // Project paths are not stored in their `id`;
              // The other asset types either do not exist on the Local backend,
              // or do not have a path.
              break
            }
          }
        }
        const newAsset = object.merge(smartAsset.value, {
          // This is SAFE as the type of `newId` is not changed from its original type.
          // eslint-disable-next-line no-restricted-syntax
          id: newId as never,
          parentId: nonNullNewParent.value.id,
          projectState: newProjectState,
        })
        dispatchAssetListEvent({
          type: AssetListEventType.move,
          newParentKey: nonNullNewParentKey,
          newParent: nonNullNewParent,
          key: item.key,
          // @ts-expect-error This is SAFE as the type of the contained asset is unchanged.
          item: smartAsset.withValue(newAsset),
        })
        setAsset(newAsset)
        await smartAsset.update({
          parentDirectoryId: nonNullNewParent.value.id,
          ...(smartAsset.value.projectState?.path == null
            ? {}
            : { projectPath: smartAsset.value.projectState.path }),
        })
      } catch (error) {
        toastAndLog('moveAssetError', error, smartAsset.value.title)
        setAsset(
          object.merger({
            // This is SAFE as the type of `newId` is not changed from its original type.
            // eslint-disable-next-line no-restricted-syntax
            id: smartAsset.value.id as never,
            parentId: smartAsset.value.parentId,
            projectState: smartAsset.value.projectState,
          })
        )
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
      isCloud,
      item.directory,
      item.directoryKey,
      item.key,
      toastAndLog,
      /* should never change */ setAsset,
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
      if (smartAsset.isDirectory()) {
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
        if (smartAsset.isProject() && !isCloud) {
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
        await smartAsset.delete({ force: forever, parentId: smartAsset.value.parentId })
        dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
      } catch (error) {
        setInsertionVisibility(Visibility.visible)
        toastAndLog('deleteAssetError', error, smartAsset.value.title)
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
      toastAndLog('restoreAssetError', error, smartAsset.value.title)
    }
  }, [dispatchAssetListEvent, smartAsset, toastAndLog, /* should never change */ item.key])

  const doTriggerDescriptionEdit = React.useCallback(() => {
    setModal(
      <EditAssetDescriptionModal
        doChangeDescription={async description => {
          if (description !== smartAsset.value.description) {
            setAsset(object.merger({ description }))
            await smartAsset.update({ parentDirectoryId: null, description }).catch(error => {
              setAsset(object.merger({ description: smartAsset.value.description }))
              throw error
            })
          }
        }}
        initialDescription={smartAsset.value.description}
      />
    )
  }, [setModal, smartAsset, setAsset])

  eventHooks.useEventHandler(assetEvents, async event => {
    if (category === Category.trash) {
      switch (event.type) {
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
        default: {
          return
        }
      }
    } else {
      switch (event.type) {
        // These events are handled in the specific `NameColumn` files.
        case AssetEventType.updateFiles:
        case AssetEventType.openProject:
        case AssetEventType.closeProject: {
          break
        }
        case AssetEventType.delete:
        case AssetEventType.deleteLabel: {
          console.log()
          break
        }
        case AssetEventType.deleteForever: {
          // This event only makes sense in the `Trash` category.
          break
        }
        case AssetEventType.restore: {
          if (event.ids.has(item.key)) {
            await doRestore()
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
                      const error: unknown = getText('projectHasNoSourceFilesPhrase')
                      toastAndLog('downloadProjectError', error, asset.title)
                    }
                    break
                  } catch (error) {
                    toastAndLog('downloadProjectError', error, asset.title)
                  }
                  break
                }
                case backendModule.AssetType.file: {
                  try {
                    const details = await smartAsset.getDetails()
                    if (details.url != null) {
                      download.download(details.url, asset.title)
                    } else {
                      const error: unknown = getText('fileNotFoundPhrase')
                      toastAndLog('downloadFileError', error, asset.title)
                    }
                    break
                  } catch (error) {
                    toastAndLog('downloadFileError', error, asset.title)
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
                    toastAndLog('downloadDataLinkError', error, asset.title)
                  }
                  break
                }
                default: {
                  // These assets cannot be downloaded.
                  break
                }
              }
            } else {
              if (smartAsset.isProject()) {
                const uuid = localBackend.extractTypeAndId(smartAsset.value.id).id
                download.download(
                  `./api/project-manager/projects/${uuid}/enso-project`,
                  `${asset.title}.enso-project`
                )
              }
            }
          }
          break
        }
        case AssetEventType.removeSelf: {
          // This is not triggered from the asset list, so it uses `item.id` instead of `key`.
          if (event.id === asset.id && user != null && user.value.isEnabled) {
            setInsertionVisibility(Visibility.hidden)
            try {
              await smartAsset.setPermissions({ action: null, actorsIds: [user.value.userId] })
              dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
            } catch (error) {
              setInsertionVisibility(Visibility.visible)
              toastAndLog(null, error)
            }
            break
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
            break
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
    props.onDragOver(event)
    const directoryId = item.item.isDirectory() ? item.item.value.id : item.directory.value.id
    const payload = drag.ASSET_ROWS.lookup(event)
    if (
      (payload != null && payload.every(innerItem => innerItem.asset.parentId !== directoryId)) ||
      event.dataTransfer.types.includes('Files')
    ) {
      event.preventDefault()
      if (item.item.isDirectory() && state.category !== Category.trash) {
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
                className={`h-row rounded-full transition-all ease-in-out ${visibility} ${isDraggedOver || selected ? 'selected' : ''}`}
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
                    doToggleDirectoryExpansion(item.item, item.key)
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
                        doTriggerDescriptionEdit={doTriggerDescriptionEdit}
                      />
                    )
                  } else {
                    onContextMenu(innerProps, event)
                  }
                }}
                onDragStart={event => {
                  if (rowState.isEditingName) {
                    event.preventDefault()
                  } else {
                    props.onDragStart(event)
                  }
                }}
                onDragEnter={event => {
                  if (dragOverTimeoutHandle.current != null) {
                    window.clearTimeout(dragOverTimeoutHandle.current)
                  }
                  if (item.type === backendModule.AssetType.directory) {
                    dragOverTimeoutHandle.current = window.setTimeout(() => {
                      doToggleDirectoryExpansion(item.item, item.key, true)
                    }, DRAG_EXPAND_DELAY_MS)
                  }
                  // Required because `dragover` does not fire on `mouseenter`.
                  onDragOver(event)
                }}
                onDragOver={event => {
                  if (state.category === Category.trash) {
                    event.dataTransfer.dropEffect = 'none'
                  }
                  onDragOver(event)
                }}
                onDragEnd={event => {
                  clearDragState()
                  props.onDragEnd(event)
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
                  if (category !== Category.trash) {
                    props.onDrop(event)
                    clearDragState()
                    const [directoryKey, directory] =
                      item.type === backendModule.AssetType.directory
                        ? [item.key, item.item]
                        : [item.directoryKey, item.directory]
                    const payload = drag.ASSET_ROWS.lookup(event)
                    if (
                      payload != null &&
                      payload.every(innerItem => innerItem.key !== directoryKey)
                    ) {
                      event.preventDefault()
                      event.stopPropagation()
                      unsetModal()
                      doToggleDirectoryExpansion(directory, directoryKey, true)
                      const ids = payload
                        .filter(payloadItem => payloadItem.asset.parentId !== directory.value.id)
                        .map(dragItem => dragItem.key)
                      dispatchAssetEvent({
                        type: AssetEventType.move,
                        newParentKey: directoryKey,
                        newParent: directory,
                        ids: new Set(ids),
                      })
                    } else if (event.dataTransfer.types.includes('Files')) {
                      event.preventDefault()
                      event.stopPropagation()
                      doToggleDirectoryExpansion(directory, directoryKey, true)
                      dispatchAssetListEvent({
                        type: AssetListEventType.uploadFiles,
                        parentKey: directoryKey,
                        parent: directory,
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
                        item={item}
                        setItem={setItem}
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
              doTriggerDescriptionEdit={doTriggerDescriptionEdit}
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
