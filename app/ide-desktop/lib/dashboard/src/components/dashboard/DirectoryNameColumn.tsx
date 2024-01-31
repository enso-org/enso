/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'

import FolderIcon from 'enso-assets/folder.svg'
import TriangleDownIcon from 'enso-assets/triangle_down.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import * as backendProvider from '#/providers/BackendProvider'
import * as shortcutsProvider from '#/providers/ShortcutsProvider'
import * as backendModule from '#/services/backend'
import * as assetTreeNode from '#/utilities/assetTreeNode'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as shortcutsModule from '#/utilities/shortcuts'
import * as string from '#/utilities/string'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.DirectoryAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.DirectoryAsset}.
 * This should never happen. */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
  const { item, setItem, selected, setSelected, state, rowState, setRowState } = props
  const { numberOfSelectedItems, nodeMap } = state
  const { doToggleDirectoryExpansion } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { backend } = backendProvider.useBackend()
  const { shortcuts } = shortcutsProvider.useShortcuts()
  const smartAsset = item.item
  if (smartAsset.type !== backendModule.AssetType.directory) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`DirectoryNameColumn` can only display directory assets.')
  }
  const asset = smartAsset.value
  const setAsset = assetTreeNode.useSetAsset(asset, setItem)
  const isCloud = backend.type === backendModule.BackendType.remote

  const doRename = async (newTitle: string) => {
    setRowState(object.merger({ isEditingName: false }))
    if (string.isWhitespaceOnly(newTitle)) {
      // Do nothing.
    } else if (isCloud && newTitle !== asset.title) {
      const oldTitle = asset.title
      setAsset(object.merger({ title: newTitle }))
      try {
        await backend.updateDirectory(asset.id, { title: newTitle }, asset.title)
      } catch (error) {
        toastAndLog('Could not rename folder', error)
        setAsset(object.merger({ title: oldTitle }))
      }
    }
  }

  return (
    <div
      className={`group flex text-left items-center whitespace-nowrap rounded-l-full gap-1 px-1.5 py-1 min-w-max ${indent.indentClass(
        item.depth
      )}`}
      onKeyDown={event => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={event => {
        if (
          eventModule.isSingleClick(event) &&
          ((selected && numberOfSelectedItems === 1) ||
            shortcuts.matchesMouseAction(shortcutsModule.MouseAction.editName, event))
        ) {
          setRowState(object.merger({ isEditingName: true }))
        } else if (eventModule.isDoubleClick(event)) {
          if (!rowState.isEditingName) {
            // This must be processed on the next tick, otherwise it will be overridden
            // by the default click handler.
            window.setTimeout(() => {
              setSelected(false)
            }, 0)
            doToggleDirectoryExpansion(smartAsset, item.key)
          }
        }
      }}
    >
      <SvgMask
        src={TriangleDownIcon}
        className={`hidden group-hover:inline-block cursor-pointer h-4 w-4 m-1 transition-transform duration-300 ${
          item.children != null ? '' : '-rotate-90'
        }`}
        onClick={event => {
          event.stopPropagation()
          doToggleDirectoryExpansion(smartAsset, item.key)
        }}
      />
      <SvgMask src={FolderIcon} className="group-hover:hidden h-4 w-4 m-1" />
      <EditableSpan
        editable={rowState.isEditingName}
        checkSubmittable={newTitle =>
          (nodeMap.current.get(item.directoryKey)?.children ?? []).every(
            child =>
              // All siblings,
              child.key === item.key ||
              // that are directories,
              !backendModule.assetIsDirectory(child.item.value) ||
              // must have a different name.
              child.item.value.title !== newTitle
          )
        }
        onSubmit={doRename}
        onCancel={() => {
          setRowState(object.merger({ isEditingName: false }))
        }}
        className={`cursor-pointer bg-transparent grow leading-170 h-6 py-px ${
          rowState.isEditingName ? 'cursor-text' : 'cursor-pointer'
        }`}
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
