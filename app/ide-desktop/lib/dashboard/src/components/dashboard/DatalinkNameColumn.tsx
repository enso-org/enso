/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import DatalinkIcon from 'enso-assets/datalink.svg'

import * as store from '#/store'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'

// ==========================
// === DatalinkNameColumn ===
// ==========================

/** Props for a {@link DatalinkNameColumn}. */
export interface DatalinkNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.DatalinkAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DatalinkAsset}.
 * This should never happen. */
export default function DatalinkNameColumn(props: DatalinkNameColumnProps) {
  const { item, depth, state, isEditable } = props
  const { backend, setIsAssetPanelTemporarilyVisible } = state
  const inputBindings = inputBindingsProvider.useInputBindings()
  const isEditingName = store.useStore(
    storeState => storeState.getAssetState(backend.type, item.id).isEditingName
  )
  const setIsAssetEditingName = store.useStore(storeState => storeState.setIsAssetEditingName)
  if (item.type !== backendModule.AssetType.datalink) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DatalinkNameColumn` can only display Datalinks.')
  }

  const setIsEditing = (editing: boolean) => {
    if (isEditable) {
      setIsAssetEditingName(backend.type, item.id, editing)
    }
  }

  // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
  // context menu entry should be re-added.
  // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
  const doRename = (newTitle: string) => {
    setIsEditing(false)

    if (newTitle !== item.title) {
      // Do nothing - the corresponding backend endpoint does not yet exist.
    }
  }

  const handleClick = inputBindings.handler({
    editName: () => {
      setIsEditing(true)
    },
  })

  return (
    <div
      className={tailwindMerge.twMerge(
        'flex h-full min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y',
        indent.indentClass(depth)
      )}
      onKeyDown={event => {
        if (isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={event => {
        if (handleClick(event)) {
          // Already handled.
        } else if (
          eventModule.isSingleClick(event) &&
          store.useStore.getState().getIsAssetSelected(item.id)
        ) {
          setIsEditing(true)
        } else if (eventModule.isDoubleClick(event)) {
          event.stopPropagation()
          setIsAssetPanelTemporarilyVisible(true)
        }
      }}
    >
      <img src={DatalinkIcon} className="m-name-column-icon size-icon" />
      <EditableSpan
        editable={false}
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        className="text grow bg-transparent"
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
