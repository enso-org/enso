/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'
import { useStore } from 'zustand'

import BlankIcon from '#/assets/blank.svg'

import { backendMutationOptions, useListUserGroups, useListUsers } from '#/hooks/backendHooks'
import * as dragAndDropHooks from '#/hooks/dragAndDropHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import { useDriveStore, useSetSelectedKeys } from '#/providers/DriveProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import * as aria from '#/components/aria'
import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import * as columnModule from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import FocusRing from '#/components/styled/FocusRing'

import EditAssetDescriptionModal from '#/modals/EditAssetDescriptionModal'

import * as backendModule from '#/services/Backend'
import * as localBackend from '#/services/LocalBackend'

import { createGetProjectDetailsQuery } from '#/hooks/projectHooks'
import { isCloudCategory } from '#/layouts/CategorySwitcher/Category'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as download from '#/utilities/download'
import * as drag from '#/utilities/drag'
import * as eventModule from '#/utilities/event'
import * as fileInfo from '#/utilities/fileInfo'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as path from '#/utilities/path'
import * as permissions from '#/utilities/permissions'
import * as set from '#/utilities/set'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import { useQuery } from '@tanstack/react-query'

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
  const { nodeMap, setAssetPanelProps, doToggleDirectoryExpansion, doCopy, doCut, doPaste } = state
  const { setIsAssetPanelTemporarilyVisible, scrollContainerRef, rootDirectoryId, backend } = state
  const { visibilities, category } = state

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
  const { user } = authProvider.useFullUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const users = useListUsers(backend)
  const userGroups = useListUserGroups(backend)
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
  const isCloud = isCloudCategory(category)
  const outerVisibility = visibilities.get(item.key)
  const visibility =
    outerVisibility == null || outerVisibility === Visibility.visible ?
      insertionVisibility
    : outerVisibility
  const hidden = hiddenRaw || visibility === Visibility.hidden

  const copyAssetMutation = useMutation(backendMutationOptions(backend, 'copyAsset'))
  const updateAssetMutation = useMutation(backendMutationOptions(backend, 'updateAsset'))
  const deleteAssetMutation = useMutation(backendMutationOptions(backend, 'deleteAsset'))
  const undoDeleteAssetMutation = useMutation(backendMutationOptions(backend, 'undoDeleteAsset'))
  const openProjectMutation = useMutation(backendMutationOptions(backend, 'openProject'))
  const closeProjectMutation = useMutation(backendMutationOptions(backend, 'closeProject'))
  const getProjectDetailsMutation = useMutation(
    backendMutationOptions(backend, 'getProjectDetails'),
  )
  const getFileDetailsMutation = useMutation(backendMutationOptions(backend, 'getFileDetails'))
  const getDatalinkMutation = useMutation(backendMutationOptions(backend, 'getDatalink'))
  const createPermissionMutation = useMutation(backendMutationOptions(backend, 'createPermission'))
  const associateTagMutation = useMutation(backendMutationOptions(backend, 'associateTag'))
  const copyAsset = copyAssetMutation.mutateAsync
  const updateAsset = updateAssetMutation.mutateAsync
  const deleteAsset = deleteAssetMutation.mutateAsync
  const undoDeleteAsset = undoDeleteAssetMutation.mutateAsync
  const openProject = openProjectMutation.mutateAsync
  const closeProject = closeProjectMutation.mutateAsync

  const { data: projectState } = useQuery({
    // This is SAFE, as `isOpened` is only true for projects.
    // eslint-disable-next-line no-restricted-syntax
    ...createGetProjectDetailsQuery.createPassiveListener(item.item.id as backendModule.ProjectId),
    select: (data) => data?.state.type,
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

  const doCopyOnBackend = React.useCallback(
    async (newParentId: backendModule.DirectoryId | null) => {
      try {
        setAsset((oldAsset) =>
          object.merge(oldAsset, {
            title: oldAsset.title + ' (copy)',
            labels: [],
            permissions: permissions.tryCreateOwnerPermission(
              `${item.path} (copy)`,
              category,
              user,
              users ?? [],
              userGroups ?? [],
            ),
            modifiedAt: dateTime.toRfc3339(new Date()),
          }),
        )
        newParentId ??= rootDirectoryId
        const copiedAsset = await copyAsset([
          asset.id,
          newParentId,
          asset.title,
          nodeMap.current.get(newParentId)?.item.title ?? '(unknown)',
        ])
        setAsset(
          // This is SAFE, as the type of the copied asset is guaranteed to be the same
          // as the type of the original asset.
          // eslint-disable-next-line no-restricted-syntax
          object.merger({
            ...copiedAsset.asset,
            state: { type: backendModule.ProjectState.new },
          } as Partial<backendModule.AnyAsset>),
        )
      } catch (error) {
        toastAndLog('copyAssetError', error, asset.title)
        // Delete the new component representing the asset that failed to insert.
        dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
      }
    },
    [
      setAsset,
      rootDirectoryId,
      copyAsset,
      asset.id,
      asset.title,
      nodeMap,
      item.path,
      item.key,
      category,
      user,
      users,
      userGroups,
      toastAndLog,
      dispatchAssetListEvent,
    ],
  )

  const doMove = React.useCallback(
    async (
      newParentKey: backendModule.DirectoryId | null,
      newParentId: backendModule.DirectoryId | null,
    ) => {
      const nonNullNewParentKey = newParentKey ?? rootDirectoryId
      const nonNullNewParentId = newParentId ?? rootDirectoryId
      try {
        setItem((oldItem) =>
          oldItem.with({ directoryKey: nonNullNewParentKey, directoryId: nonNullNewParentId }),
        )
        const newParentPath = localBackend.extractTypeAndId(nonNullNewParentId).id
        let newId = asset.id
        if (!isCloud) {
          const oldPath = localBackend.extractTypeAndId(asset.id).id
          const newPath = path.joinPath(newParentPath, fileInfo.getFileName(oldPath))
          switch (asset.type) {
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
            case backendModule.AssetType.datalink:
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
        // This is SAFE as the type of `newId` is not changed from its original type.
        // eslint-disable-next-line no-restricted-syntax
        const newAsset = object.merge(asset, { id: newId as never, parentId: nonNullNewParentId })
        dispatchAssetListEvent({
          type: AssetListEventType.move,
          newParentKey: nonNullNewParentKey,
          newParentId: nonNullNewParentId,
          key: item.key,
          item: newAsset,
        })
        setAsset(newAsset)
        await updateAsset([
          asset.id,
          { parentDirectoryId: newParentId ?? rootDirectoryId, description: null },
          asset.title,
        ])
      } catch (error) {
        toastAndLog('moveAssetError', error, asset.title)
        setAsset(
          object.merger({
            // This is SAFE as the type of `newId` is not changed from its original type.
            // eslint-disable-next-line no-restricted-syntax
            id: asset.id as never,
            parentId: asset.parentId,
            projectState: asset.projectState,
          }),
        )
        setItem((oldItem) =>
          oldItem.with({ directoryKey: item.directoryKey, directoryId: item.directoryId }),
        )
        // Move the asset back to its original position.
        dispatchAssetListEvent({
          type: AssetListEventType.move,
          newParentKey: item.directoryKey,
          newParentId: item.directoryId,
          key: item.key,
          item: asset,
        })
      }
    },
    [
      isCloud,
      asset,
      rootDirectoryId,
      item.directoryId,
      item.directoryKey,
      item.key,
      toastAndLog,
      updateAsset,
      setAsset,
      dispatchAssetListEvent,
    ],
  )

  React.useEffect(() => {
    if (isSoleSelected) {
      setAssetPanelProps({ backend, item, setItem })
      setIsAssetPanelTemporarilyVisible(false)
    }
  }, [item, isSoleSelected, backend, setAssetPanelProps, setIsAssetPanelTemporarilyVisible])

  const doDelete = React.useCallback(
    async (forever = false) => {
      setInsertionVisibility(Visibility.hidden)
      if (asset.type === backendModule.AssetType.directory) {
        dispatchAssetListEvent({
          type: AssetListEventType.closeFolder,
          id: asset.id,
          // This is SAFE, as this asset is already known to be a directory.
          // eslint-disable-next-line no-restricted-syntax
          key: item.key as backendModule.DirectoryId,
        })
      }
      try {
        dispatchAssetListEvent({ type: AssetListEventType.willDelete, key: item.key })
        if (
          asset.type === backendModule.AssetType.project &&
          backend.type === backendModule.BackendType.local
        ) {
          if (
            asset.projectState.type !== backendModule.ProjectState.placeholder &&
            asset.projectState.type !== backendModule.ProjectState.closed
          ) {
            await openProject([asset.id, null, asset.title])
          }
          try {
            await closeProject([asset.id, asset.title])
          } catch {
            // Ignored. The project was already closed.
          }
        }
        await deleteAsset([asset.id, { force: forever }, asset.title])
        dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
      } catch (error) {
        setInsertionVisibility(Visibility.visible)
        toastAndLog('deleteAssetError', error, asset.title)
      }
    },
    [
      backend,
      dispatchAssetListEvent,
      asset,
      openProject,
      closeProject,
      deleteAsset,
      item.key,
      toastAndLog,
    ],
  )

  const doRestore = React.useCallback(async () => {
    // Visually, the asset is deleted from the Trash view.
    setInsertionVisibility(Visibility.hidden)
    try {
      await undoDeleteAsset([asset.id, asset.title])
      dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
    } catch (error) {
      setInsertionVisibility(Visibility.visible)
      toastAndLog('restoreAssetError', error, asset.title)
    }
  }, [dispatchAssetListEvent, asset, toastAndLog, undoDeleteAsset, item.key])

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

  eventListProvider.useAssetEventListener(async (event) => {
    if (state.category.type === 'trash') {
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
        case AssetEventType.newProject:
        case AssetEventType.newFolder:
        case AssetEventType.uploadFiles:
        case AssetEventType.newDatalink:
        case AssetEventType.newSecret:
        case AssetEventType.updateFiles:
        case AssetEventType.projectClosed: {
          break
        }
        case AssetEventType.copy: {
          if (event.ids.has(item.key)) {
            await doCopyOnBackend(event.newParentId)
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
            await doMove(event.newParentKey, event.newParentId)
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
          if (event.type === AssetEventType.downloadSelected ? selected : event.ids.has(asset.id)) {
            if (isCloud) {
              switch (asset.type) {
                case backendModule.AssetType.project: {
                  try {
                    const details = await getProjectDetailsMutation.mutateAsync([
                      asset.id,
                      asset.parentId,
                      asset.title,
                    ])
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
                    const details = await getFileDetailsMutation.mutateAsync([
                      asset.id,
                      asset.title,
                    ])
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
                    const value = await getDatalinkMutation.mutateAsync([asset.id, asset.title])
                    const fileName = `${asset.title}.datalink`
                    download.download(
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
            setInsertionVisibility(Visibility.hidden)
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
              setInsertionVisibility(Visibility.visible)
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
            // The IIFE is required to prevent TypeScript from narrowing this value.
            let found = (() => false)()
            const labels =
              oldAsset.labels?.filter((label) => {
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
        case AssetEventType.setItem: {
          if (asset.id === event.id) {
            setAsset(event.valueOrUpdater)
          }
          break
        }
      }
    }
  }, item.initialAssetEvents)

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
      if (item.item.type === backendModule.AssetType.directory && state.category.type !== 'trash') {
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
                    doToggleDirectoryExpansion(item.item.id, item.key, asset.title)
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
                      doToggleDirectoryExpansion(item.item.id, item.key, asset.title, true)
                    }, DRAG_EXPAND_DELAY_MS)
                  }
                  // Required because `dragover` does not fire on `mouseenter`.
                  props.onDragOver?.(event)
                  onDragOver(event)
                }}
                onDragOver={(event) => {
                  if (state.category.type === 'trash') {
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
                  if (state.category.type !== 'trash') {
                    props.onDrop?.(event)
                    clearDragState()
                    const [directoryKey, directoryId, directoryTitle] =
                      item.type === backendModule.AssetType.directory ?
                        [item.key, item.item.id, asset.title]
                      : [item.directoryKey, item.directoryId, null]
                    const payload = drag.ASSET_ROWS.lookup(event)
                    if (
                      payload != null &&
                      payload.every((innerItem) => innerItem.key !== directoryKey)
                    ) {
                      event.preventDefault()
                      event.stopPropagation()
                      unsetModal()
                      doToggleDirectoryExpansion(directoryId, directoryKey, directoryTitle, true)
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
                      doToggleDirectoryExpansion(directoryId, directoryKey, directoryTitle, true)
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
