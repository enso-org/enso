/** @file A table row for an arbitrary asset. */
import { Cell, Row } from '#/components/aria'
import {
  useDeleteAssetsMutationState,
  useMoveAssetsMutationState,
  useRestoreAssetsMutationState,
} from '#/hooks/backendBatchedHooks'
import { useBackendMutationState } from '#/hooks/backendHooks'
import { useDraggable } from '#/hooks/dragAndDropHooks'
import AssetContextMenu from '#/layouts/AssetContextMenu'
import type * as assetsTable from '#/layouts/AssetsTable'
import * as columnModule from '#/pages/dashboard/components/column'
import * as columnUtils from '#/pages/dashboard/components/column/columnUtils'
import { useDriveStore, useSetCurrentDirectoryId } from '#/providers/DriveProvider'
import { setModal } from '#/providers/ModalProvider'
import * as backendModule from '#/services/Backend'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Visibility from '#/utilities/Visibility'
import { useStore } from '#/utilities/zustand'
import { useRightPanelData } from '$/providers/react'
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
  readonly state: assetsTable.AssetsTableState
  readonly columns: readonly columnUtils.Column[]
  readonly select: (item: backendModule.AnyAsset) => void
  readonly tableRootRef: React.MutableRefObject<HTMLElement | null> | undefined
}

/** A row containing an {@link backendModule.AnyAsset}. */

export const AssetRow = React.memo(function AssetRow(props: AssetRowProps) {
  const { item, columns } = props

  switch (item.type) {
    case backendModule.AssetType.specialUp: {
      return <AssetSpecialRow columnsLength={columns.length} type={item.type} />
    }
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.secret:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.directory:
    default: {
      return <RealAssetRow {...props} />
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
  const { type } = props

  switch (type) {
    case backendModule.AssetType.specialUp: {
      // TODO: Implement this.
      // @MrFlashAccount [Cloud v2 #1810](https://github.com/enso-org/cloud-v2/issues/1810)
      return null
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
  const { select, state, columns, item, tableRootRef } = props
  const { backend, currentDirectoryId, doCopy, doCut } = state

  const [isNavigating, startNavigation] = useTransition()

  const setCurrentDirectoryId = useSetCurrentDirectoryId()
  const driveStore = useDriveStore()
  const rightPanel = useRightPanelData()
  const { isSelected, isSoleSelected, isMultiSelected } = useStore(
    driveStore,
    ({ visuallySelectedKeys, selectedIds }) => {
      const selection = visuallySelectedKeys ?? selectedIds
      const selected = selection.has(item.id)

      return {
        isSelected: selected,
        isSoleSelected: selected && selection.size === 1,
        isMultiSelected: selection.size > 1,
      }
    },
    { areEqual: 'shallow', unsafeEnableTransition: true },
  )

  const draggableProps = useDraggable({ isDisabled: !isSelected })
  const rootRef = React.useRef<HTMLElement | null>(null)

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

  const insertionVisibility = useStore(driveStore, (driveState) => {
    return (
        driveState.pasteData?.type === 'move' &&
          driveState.pasteData.data.assets.some((asset) => asset.id === item.id)
      ) ?
        'opacity-50'
      : Visibility.visible
  })
  const visibility = isDeleting || isRestoring || isUpdating ? 'opacity-50' : insertionVisibility

  switch (item.type) {
    case backendModule.AssetType.directory:
    case backendModule.AssetType.project:
    case backendModule.AssetType.file:
    case backendModule.AssetType.datalink:
    case backendModule.AssetType.secret: {
      const innerProps: AssetRowInnerProps = { asset: item, state }

      return (
        <>
          <Row
            data-testid="asset-row"
            data-selected={isSelected}
            data-id={item.id}
            id={item.id}
            // Required so that name updates properly when edited.
            dependencies={[item]}
            ref={(element) => {
              rootRef.current = element
              if (!element) {
                return
              }
              element.ondblclick = () => {
                if (item.type === backendModule.AssetType.directory) {
                  startNavigation(() => {
                    setCurrentDirectoryId(item.id)
                  })
                }
              }
              element.oncontextmenu = (event) => {
                // We show the table-wide context menu if the asset is included in the selection.
                if (isSelected && isMultiSelected) {
                  return
                }

                event.preventDefault()
                event.stopPropagation()

                // If we click on an asset row outside of the selection, reset the selection
                // to the clicked asset.
                if (!isSelected) {
                  select(item)
                }

                setModal(
                  <AssetContextMenu
                    rootRef={tableRootRef}
                    innerProps={innerProps}
                    currentDirectoryId={currentDirectoryId}
                    triggerRef={rootRef}
                    event={event}
                    eventTarget={
                      event.target instanceof HTMLElement ?
                        event.target
                        // eslint-disable-next-line no-restricted-syntax
                      : (event.currentTarget as HTMLElement)
                    }
                    doCopy={doCopy}
                    doCut={doCut}
                    rightPanel={rightPanel}
                  />,
                )
              }
              element.onfocus = draggableProps.onFocus
              element.blur = draggableProps.onBlur
              element.draggable = true
            }}
            className={tailwindMerge.twMerge(
              'AssetRow h-table-row rounded-full transition-all ease-in-out rounded-rows-child selected:selected',
              visibility,
            )}
            columns={columns.map((column) => ({ id: column }))}
          >
            {({ id: column }) => {
              const Render = columnModule.COLUMN_RENDERER[column]
              return (
                <Cell key={column} className={columnUtils.COLUMN_CSS_CLASS[column]}>
                  <Render
                    isNavigating={isNavigating}
                    backendType={backend.type}
                    item={item}
                    state={state}
                    isEditable={state.category.type !== 'trash'}
                  />
                </Cell>
              )
            }}
          </Row>

          {isSoleSelected && (
            // This is a copy of the context menu, since the context menu registers keyboard
            // shortcut handlers. This is a bit of a hack, however it is preferable to duplicating
            // the entire context menu (once for the keyboard actions, once for the JSX).
            <AssetContextMenu
              hidden
              rootRef={tableRootRef}
              innerProps={innerProps}
              currentDirectoryId={currentDirectoryId}
              triggerRef={rootRef}
              event={{ pageX: 0, pageY: 0 }}
              eventTarget={null}
              doCopy={doCopy}
              doCut={doCut}
              rightPanel={rightPanel}
            />
          )}
        </>
      )
    }
    case backendModule.AssetType.specialUp:
    default: {
      invariant(
        false,
        'Unsupported asset type, expected one of: directory, project, file, datalink, secret, but got: ' +
          item.type,
      )
    }
  }
}
