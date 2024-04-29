/** @file The icon and name of a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import NetworkIcon from 'enso-assets/network.svg'

import * as eventHooks from '#/hooks/eventHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import AssetEventType from '#/events/AssetEventType'

import type * as column from '#/components/dashboard/column'
import ProjectIcon from '#/components/dashboard/ProjectIcon'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as string from '#/utilities/string'
import Visibility from '#/utilities/Visibility'

// ===================
// === ProjectName ===
// ===================

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.ProjectAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.ProjectAsset}.
 * This should never happen. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
  const { item, setItem, selected, rowState, setRowState, state, isEditable } = props
  const { isCloud, selectedKeys, assetEvents, nodeMap } = state
  const { dispatchAssetEvent, doOpenManually, doOpenEditor, doCloseEditor } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useNonPartialUserSession()
  const inputBindings = inputBindingsProvider.useInputBindings()
  if (item.type !== backendModule.AssetType.project) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`ProjectNameColumn` can only display projects.')
  }
  const smartAsset = item.item
  const smartAssetWithProjectState = React.useMemo(() => {
    // This is a workaround for a temporary bad state in the backend causing the
    // `projectState` key to be absent.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    if (smartAsset.value.projectState != null) {
      return smartAsset
    } else {
      return smartAsset.withValue(
        object.merge(smartAsset.value, {
          projectState: { type: backendModule.ProjectState.closed, volumeId: '' },
        })
      )
    }
  }, [smartAsset])
  const asset = smartAssetWithProjectState.value
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const ownPermission =
    asset.permissions?.find(permission => permission.user.userId === user?.value.userId) ?? null
  // This is a workaround for a temporary bad state in the backend causing the `projectState` key
  // to be absent.
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = asset.projectState ?? {
    type: backendModule.ProjectState.closed,
  }
  const isRunning = backendModule.IS_OPENING_OR_OPENED[projectState.type]
  const canExecute =
    !isCloud ||
    (ownPermission != null && permissions.PERMISSION_ACTION_CAN_EXECUTE[ownPermission.permission])
  const isOtherUserUsingProject =
    isCloud && projectState.openedBy != null && projectState.openedBy !== user?.value.email

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  const doRename = async (newTitle: string) => {
    setIsEditing(false)
    if (string.isWhitespaceOnly(newTitle)) {
      // Do nothing.
    } else if (newTitle !== asset.title) {
      const oldTitle = asset.title
      setAsset(object.merger({ title: newTitle }))
      try {
        await smartAsset.update({
          ami: null,
          ideVersion: null,
          projectName: newTitle,
          parentId: asset.parentId,
        })
      } catch (error) {
        toastAndLog('renameProjectError', error)
        setAsset(object.merger({ title: oldTitle }))
      }
    }
  }

  eventHooks.useEventHandler(assetEvents, async event => {
    switch (event.type) {
      case AssetEventType.updateFiles: {
        const file = event.files.get(item.item.value.id)
        if (file != null) {
          rowState.setVisibility(Visibility.faded)
          const title = backendModule.stripProjectExtension(asset.title)
          setAsset(object.merge(asset, { title }))
          try {
            const newSmartAsset = await smartAsset.update({ file })
            rowState.setVisibility(Visibility.visible)
            setAsset(newSmartAsset.value)
          } catch (error) {
            switch (event.type) {
              case AssetEventType.updateFiles: {
                toastAndLog('updateProjectError', error)
                break
              }
            }
            break
          }
        }
        break
      }
      default: {
        break
      }
    }
  })

  const handleClick = inputBindings.handler({
    open: () => {
      dispatchAssetEvent({
        type: AssetEventType.openProject,
        id: asset.id,
        shouldAutomaticallySwitchPage: true,
        runInBackground: false,
      })
    },
    run: () => {
      dispatchAssetEvent({
        type: AssetEventType.openProject,
        id: asset.id,
        shouldAutomaticallySwitchPage: false,
        runInBackground: true,
      })
    },
    editName: () => {
      setIsEditing(true)
    },
  })

  return (
    <div
      className={`flex h-full min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y ${indent.indentClass(
        item.depth
      )}`}
      onKeyDown={event => {
        if (rowState.isEditingName && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={event => {
        if (rowState.isEditingName || isOtherUserUsingProject) {
          // The project should neither be edited nor opened in these cases.
        } else if (handleClick(event)) {
          // Already handled.
        } else if (
          !isRunning &&
          eventModule.isSingleClick(event) &&
          selected &&
          selectedKeys.current.size === 1
        ) {
          setIsEditing(true)
        }
      }}
    >
      {!canExecute ? (
        <SvgMask src={NetworkIcon} className="m-name-column-icon size-icon" />
      ) : (
        <ProjectIcon
          smartAsset={smartAssetWithProjectState}
          setItem={setAsset}
          assetEvents={assetEvents}
          doOpenManually={doOpenManually}
          doOpenEditor={switchPage => {
            doOpenEditor(smartAsset, setAsset, switchPage)
          }}
          doCloseEditor={() => {
            doCloseEditor(asset)
          }}
          state={state}
        />
      )}
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={`text grow bg-transparent ${
          rowState.isEditingName
            ? 'cursor-text'
            : canExecute && !isOtherUserUsingProject
              ? 'cursor-pointer'
              : ''
        }`}
        checkSubmittable={newTitle =>
          newTitle !== asset.title &&
          newTitle !== '' &&
          (nodeMap.current.get(item.directoryKey)?.children ?? []).every(
            child =>
              // All siblings,
              child.key === item.key ||
              // that are not directories,
              backendModule.assetIsDirectory(child.item.value) ||
              // must have a different name.
              child.item.value.title !== newTitle
          )
        }
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
