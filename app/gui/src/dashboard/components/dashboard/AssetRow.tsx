/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { useStore } from 'zustand'

import BlankIcon from '#/assets/blank.svg'

import * as dragAndDropHooks from '#/hooks/dragAndDropHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'

import { useDriveStore, useSetSelectedKeys } from '#/providers/DriveProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import FocusRing from '#/components/styled/FocusRing'
import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'
import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import { isCloudCategory } from '#/layouts/CategorySwitcher/Category'
import * as localBackend from '#/services/LocalBackend'

import * as backendModule from '#/services/Backend'

import { Text } from '#/components/AriaComponents'
import { useCutAndPaste } from '#/events/assetListEvent'
import {
  backendMutationOptions,
  backendQueryOptions,
  useBackendMutationState,
} from '#/hooks/backendHooks'
import { createGetProjectDetailsQuery } from '#/hooks/projectHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import { download } from '#/utilities/download'
import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as set from '#/utilities/set'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'

// =================
// === Constants ===
// =================

/** The height of the header row. */
const HEADER_HEIGHT_PX = 40
/**
 * The amount of time (in milliseconds) the drag item must be held over this component
 * to make a directory row expand.
 */
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
  readonly isOpened: boolean
  readonly item: assetTreeNode.AnyAssetTreeNode
  readonly state: assetsTable.AssetsTableState
  readonly hidden: boolean
  readonly columns: columnUtils.Column[]
  readonly isKeyboardSelected: boolean
  readonly grabKeyboardFocus: (item: assetTreeNode.AnyAssetTreeNode) => void
  readonly onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
  readonly select: (item: assetTreeNode.AnyAssetTreeNode) => void
  readonly onDragStart?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: assetTreeNode.AnyAssetTreeNode,
  ) => void
  readonly onDragOver?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: assetTreeNode.AnyAssetTreeNode,
  ) => void
  readonly onDragLeave?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: assetTreeNode.AnyAssetTreeNode,
  ) => void
  readonly onDragEnd?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: assetTreeNode.AnyAssetTreeNode,
  ) => void
  readonly onDrop?: (
    event: React.DragEvent<HTMLTableRowElement>,
    item: assetTreeNode.AnyAssetTreeNode,
  ) => void
  readonly updateAssetRef: React.RefObject<
    Record<backendModule.AssetId, (asset: backendModule.AnyAsset) => void>
  >
}

/** A row containing an {@link backendModule.AnyAsset}. */
export const AssetRow = React.memo(function AssetRow(props: AssetRowProps) {
  const { isKeyboardSelected, isOpened, select, state, columns, onClick } = props
  const { item: rawItem, hidden: hiddenRaw, updateAssetRef, grabKeyboardFocus } = props
  const {
    nodeMap,
    doToggleDirectoryExpansion,
    doCopy,
    doCut,
    doPaste,
    doDelete: doDeleteRaw,
    doRestore,
    doMove,
    category,
  } = state
  const { scrollContainerRef, rootDirectoryId, backend } = state
  const { visibilities } = state

  const [item, setItem] = React.useState(rawItem)
  const driveStore = useDriveStore()
  const queryClient = useQueryClient()
  const { user } = useFullUserSession()
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
  const wasSoleSelectedRef = React.useRef(isSoleSelected)
  const draggableProps = dragAndDropHooks.useDraggable()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const cutAndPaste = useCutAndPaste(category)
  const [isDraggedOver, setIsDraggedOver] = React.useState(false)
  const rootRef = React.useRef<HTMLElement | null>(null)
  const dragOverTimeoutHandle = React.useRef<number | null>(null)
  const grabKeyboardFocusRef = useSyncRef(grabKeyboardFocus)
  const asset = item.item
  const [innerRowState, setRowState] = React.useState<assetsTable.AssetRowState>(
    assetRowUtils.INITIAL_ROW_STATE,
  )

  const isNewlyCreated = useStore(driveStore, ({ newestFolderId }) => newestFolderId === asset.id)
  const isEditingName = innerRowState.isEditingName || isNewlyCreated

  const rowState = React.useMemo(() => {
    return object.merge(innerRowState, { isEditingName })
  }, [isEditingName, innerRowState])

  const nodeParentKeysRef = React.useRef<{
    readonly nodeMap: WeakRef<ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>>
    readonly parentKeys: Map<backendModule.AssetId, backendModule.DirectoryId>
  } | null>(null)

  const isDeleting =
    useBackendMutationState(backend, 'deleteAsset', {
      predicate: ({ state: { variables: [assetId] = [] } }) => assetId === asset.id,
    }).length !== 0
  const isRestoring =
    useBackendMutationState(backend, 'undoDeleteAsset', {
      predicate: ({ state: { variables: [assetId] = [] } }) => assetId === asset.id,
    }).length !== 0
  const isCloud = isCloudCategory(category)

  const { data: projectState } = useQuery({
    // This is SAFE, as `isOpened` is only true for projects.
    // eslint-disable-next-line no-restricted-syntax
    ...createGetProjectDetailsQuery.createPassiveListener(item.item.id as backendModule.ProjectId),
    select: (data) => data?.state.type,
    enabled: item.type === backendModule.AssetType.project,
  })

  const toastAndLog = useToastAndLog()

  const createPermissionMutation = useMutation(backendMutationOptions(backend, 'createPermission'))
  const associateTagMutation = useMutation(backendMutationOptions(backend, 'associateTag'))

  const outerVisibility = visibilities.get(item.key)
  const insertionVisibility = useStore(driveStore, (driveState) =>
    driveState.pasteData?.type === 'move' && driveState.pasteData.data.ids.has(item.key) ?
      Visibility.faded
    : Visibility.visible,
  )
  const createPermissionVariables = createPermissionMutation.variables?.[0]
  const isRemovingSelf =
    createPermissionVariables != null &&
    createPermissionVariables.action == null &&
    createPermissionVariables.actorsIds[0] === user.userId
  const visibility =
    isRemovingSelf ? Visibility.hidden
    : outerVisibility === Visibility.visible ? insertionVisibility
    : outerVisibility ?? insertionVisibility
  const hidden = isDeleting || isRestoring || hiddenRaw || visibility === Visibility.hidden

  const setSelected = useEventCallback((newSelected: boolean) => {
    const { selectedKeys } = driveStore.getState()
    setSelectedKeys(set.withPresence(selectedKeys, item.key, newSelected))
  })

  React.useEffect(() => {
    setItem(rawItem)
  }, [rawItem])

  const rawItemRef = useSyncRef(rawItem)
  React.useEffect(() => {
    // Mutation is HIGHLY INADVISABLE in React, however it is useful here as we want to update the
    // parent's state while avoiding re-rendering the parent.
    rawItemRef.current.item = asset
  }, [asset, rawItemRef])
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)

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

  React.useImperativeHandle(updateAssetRef, () => ({ setAsset, item }))

  if (updateAssetRef.current) {
    updateAssetRef.current[item.item.id] = setAsset
  }

  React.useEffect(() => {
    return () => {
      if (updateAssetRef.current) {
        // eslint-disable-next-line react-hooks/exhaustive-deps, @typescript-eslint/no-dynamic-delete
        delete updateAssetRef.current[item.item.id]
      }
    }
  }, [item.item.id, updateAssetRef])

  const doDelete = React.useCallback(
    (forever = false) => {
      void doDeleteRaw(item.item, forever)
    },
    [doDeleteRaw, item.item],
  )

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
        return false
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
        return payload.every((payloadItem) => {
          const parentKey = nodeParentKeysRef.current?.parentKeys.get(payloadItem.key)
          const parent = parentKey == null ? null : nodeMap.current.get(parentKey)
          if (!parent) {
            return false
          } else if (permissions.isTeamPath(parent.path)) {
            return true
          } else {
            // Assume user path; check permissions
            const permission = permissions.tryFindSelfPermission(user, item.item.permissions)
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
      if (item.item.type === backendModule.AssetType.directory && state.category.type !== 'trash') {
        setIsDraggedOver(true)
      }
    }
  }

  eventListProvider.useAssetEventListener(async (event) => {
    switch (event.type) {
      case AssetEventType.move: {
        if (event.ids.has(item.key)) {
          await doMove(event.newParentKey, item.item)
        }
        break
      }
      case AssetEventType.delete: {
        if (event.ids.has(item.key)) {
          doDelete(false)
        }
        break
      }
      case AssetEventType.deleteForever: {
        if (event.ids.has(item.key)) {
          doDelete(true)
        }
        break
      }
      case AssetEventType.restore: {
        if (event.ids.has(item.key)) {
          await doRestore(item.item)
        }
        break
      }
      case AssetEventType.download:
      case AssetEventType.downloadSelected: {
        if (event.type === AssetEventType.downloadSelected ? selected : event.ids.has(asset.id)) {
          if (isCloud) {
            switch (asset.type) {
              case backendModule.AssetType.project: {
                try {
                  const details = await queryClient.fetchQuery(
                    backendQueryOptions(backend, 'getProjectDetails', [
                      asset.id,
                      asset.parentId,
                      asset.title,
                    ]),
                  )
                  if (details.url != null) {
                    await backend.download(details.url, `${asset.title}.enso-project`)
                  } else {
                    const error: unknown = getText('projectHasNoSourceFilesPhrase')
                    toastAndLog('downloadProjectError', error, asset.title)
                  }
                } catch (error) {
                  toastAndLog('downloadProjectError', error, asset.title)
                }
                break
              }
              case backendModule.AssetType.file: {
                try {
                  const details = await queryClient.fetchQuery(
                    backendQueryOptions(backend, 'getFileDetails', [asset.id, asset.title]),
                  )
                  if (details.url != null) {
                    await backend.download(details.url, asset.title)
                  } else {
                    const error: unknown = getText('fileNotFoundPhrase')
                    toastAndLog('downloadFileError', error, asset.title)
                  }
                } catch (error) {
                  toastAndLog('downloadFileError', error, asset.title)
                }
                break
              }
              case backendModule.AssetType.datalink: {
                try {
                  const value = await queryClient.fetchQuery(
                    backendQueryOptions(backend, 'getDatalink', [asset.id, asset.title]),
                  )
                  const fileName = `${asset.title}.datalink`
                  download(
                    URL.createObjectURL(
                      new File([JSON.stringify(value)], fileName, {
                        type: 'application/json+x-enso-data-link',
                      }),
                    ),
                    fileName,
                  )
                } catch (error) {
                  toastAndLog('downloadDatalinkError', error, asset.title)
                }
                break
              }
              default: {
                toastAndLog('downloadInvalidTypeError')
                break
              }
            }
          } else {
            if (asset.type === backendModule.AssetType.project) {
              const projectsDirectory = localBackend.extractTypeAndId(asset.parentId).id
              const uuid = localBackend.extractTypeAndId(asset.id).id
              const queryString = new URLSearchParams({ projectsDirectory }).toString()
              await backend.download(
                `./api/project-manager/projects/${uuid}/enso-project?${queryString}`,
                `${asset.title}.enso-project`,
              )
            }
          }
        }
        break
      }
      case AssetEventType.removeSelf: {
        // This is not triggered from the asset list, so it uses `item.id` instead of `key`.
        if (event.id === asset.id && user.isEnabled) {
          try {
            await createPermissionMutation.mutateAsync([
              {
                action: null,
                resourceId: asset.id,
                actorsIds: [user.userId],
              },
            ])
            dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
          } catch (error) {
            toastAndLog(null, error)
          }
        }
        break
      }
      case AssetEventType.temporarilyAddLabels: {
        const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY_SET
        setRowState((oldRowState) =>
          (
            oldRowState.temporarilyAddedLabels === labels &&
            oldRowState.temporarilyRemovedLabels === set.EMPTY_SET
          ) ?
            oldRowState
          : object.merge(oldRowState, {
              temporarilyAddedLabels: labels,
              temporarilyRemovedLabels: set.EMPTY_SET,
            }),
        )
        break
      }
      case AssetEventType.temporarilyRemoveLabels: {
        const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY_SET
        setRowState((oldRowState) =>
          (
            oldRowState.temporarilyAddedLabels === set.EMPTY_SET &&
            oldRowState.temporarilyRemovedLabels === labels
          ) ?
            oldRowState
          : object.merge(oldRowState, {
              temporarilyAddedLabels: set.EMPTY_SET,
              temporarilyRemovedLabels: labels,
            }),
        )
        break
      }
      case AssetEventType.addLabels: {
        setRowState((oldRowState) =>
          oldRowState.temporarilyAddedLabels === set.EMPTY_SET ?
            oldRowState
          : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY_SET }),
        )
        const labels = asset.labels
        if (
          event.ids.has(item.key) &&
          (labels == null || [...event.labelNames].some((label) => !labels.includes(label)))
        ) {
          const newLabels = [
            ...(labels ?? []),
            ...[...event.labelNames].filter((label) => labels?.includes(label) !== true),
          ]
          setAsset(object.merger({ labels: newLabels }))
          try {
            await associateTagMutation.mutateAsync([asset.id, newLabels, asset.title])
          } catch (error) {
            setAsset(object.merger({ labels }))
            toastAndLog(null, error)
          }
        }
        break
      }
      case AssetEventType.removeLabels: {
        setRowState((oldRowState) =>
          oldRowState.temporarilyAddedLabels === set.EMPTY_SET ?
            oldRowState
          : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY_SET }),
        )
        const labels = asset.labels
        if (
          event.ids.has(item.key) &&
          labels != null &&
          [...event.labelNames].some((label) => labels.includes(label))
        ) {
          const newLabels = labels.filter((label) => !event.labelNames.has(label))
          setAsset(object.merger({ labels: newLabels }))
          try {
            await associateTagMutation.mutateAsync([asset.id, newLabels, asset.title])
          } catch (error) {
            setAsset(object.merger({ labels }))
            toastAndLog(null, error)
          }
        }
        break
      }
      case AssetEventType.deleteLabel: {
        setAsset((oldAsset) => {
          const oldLabels = oldAsset.labels ?? []
          const labels: backendModule.LabelName[] = []

          for (const label of oldLabels) {
            if (label !== event.labelName) {
              labels.push(label)
            }
          }

          return oldLabels.length !== labels.length ? object.merge(oldAsset, { labels }) : oldAsset
        })
        break
      }
      case AssetEventType.setItem: {
        if (asset.id === event.id) {
          setAsset(event.valueOrUpdater)
        }
        break
      }
      default: {
        return
      }
    }
  }, item.initialAssetEvents)

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
                    if (
                      isSoleSelected &&
                      !wasSoleSelectedRef.current &&
                      element != null &&
                      scrollContainerRef.current != null
                    ) {
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
                    wasSoleSelectedRef.current = isSoleSelected
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
                      select(item)
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
                        doDelete={doDelete}
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
                    props.onDragStart?.(event, item)
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
                  props.onDragOver?.(event, item)
                  onDragOver(event)
                }}
                onDragOver={(event) => {
                  if (state.category.type === 'trash') {
                    event.dataTransfer.dropEffect = 'none'
                  }
                  props.onDragOver?.(event, item)
                  onDragOver(event)
                }}
                onDragEnd={(event) => {
                  clearDragState()
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
                    clearDragState()
                  }
                  props.onDragLeave?.(event, item)
                }}
                onDrop={(event) => {
                  if (state.category.type !== 'trash') {
                    props.onDrop?.(event, item)
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
                      cutAndPaste(
                        directoryKey,
                        directoryId,
                        { backendType: backend.type, ids: new Set(ids), category },
                        nodeMap.current,
                      )
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
                        isEditable={state.category.type !== 'trash'}
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
              triggerRef={rootRef}
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
                <Text className="px-name-column-x placeholder" disableLineHeightCompensation>
                  {getText('thisFolderIsEmpty')}
                </Text>
              </div>
            </td>
          </tr>
        )
    }
    case backendModule.AssetType.specialError: {
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
  }
})
