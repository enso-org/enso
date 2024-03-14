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
  const { item, setItem, selected, rowState, setRowState, state } = props
  const { isCloud, selectedKeys, assetEvents, nodeMap } = state
  const { dispatchAssetEvent, doOpenManually, doOpenEditor, doCloseEditor } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useNonPartialUserSession()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const smartAsset = item.item
  if (smartAsset.type !== backendModule.AssetType.project) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`ProjectNameColumn` can only display projects.')
  }
  const smartAssetWithProjectState = React.useMemo(() => {
    // This is a workaround for a temporary bad state in the backend causing the
    // `projectState` key to be absent.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    if (smartAsset.value.projectState != null) {
      return smartAsset
    } else {
      return smartAsset.withValue(
        object.merge(smartAsset.value, {
          // eslint-disable-next-line @typescript-eslint/naming-convention
          projectState: { type: backendModule.ProjectState.closed, volume_id: '' },
        })
      )
    }
  }, [smartAsset])
  const asset = smartAssetWithProjectState.value
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const ownPermission =
    asset.permissions?.find(permission => permission.user.user_email === user?.value.email) ?? null
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
    isCloud && projectState.opened_by != null && projectState.opened_by !== user?.value.email

  const doRename = async (newTitle: string) => {
    setRowState(object.merger({ isEditingName: false }))
    if (string.isWhitespaceOnly(newTitle)) {
      // Do nothing.
    } else if (newTitle !== asset.title) {
      const oldTitle = asset.title
      setAsset(object.merger({ title: newTitle }))
      try {
        await smartAsset.update({ projectName: newTitle })
      } catch (error) {
        toastAndLog('Could not rename project', error)
        setAsset(object.merger({ title: oldTitle }))
      }
    }
  }

  eventHooks.useEventHandler(assetEvents, async event => {
    switch (event.type) {
      case AssetEventType.openProject:
      case AssetEventType.closeProject:
      case AssetEventType.copy:
      case AssetEventType.cut:
      case AssetEventType.cancelCut:
      case AssetEventType.move:
      case AssetEventType.delete:
      case AssetEventType.deleteForever:
      case AssetEventType.restore:
      case AssetEventType.download:
      case AssetEventType.downloadSelected:
      case AssetEventType.removeSelf:
      case AssetEventType.temporarilyAddLabels:
      case AssetEventType.temporarilyRemoveLabels:
      case AssetEventType.addLabels:
      case AssetEventType.removeLabels:
      case AssetEventType.deleteLabel: {
        // Ignored. Any missing project-related events should be handled by `ProjectIcon`.
        // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
        // are handled by`AssetRow`.
        break
      }
      case AssetEventType.updateFiles: {
        const file = event.files.get(item.item.value.id)
        if (file != null) {
          rowState.setVisibility(Visibility.faded)
          const title = backendModule.stripProjectExtension(asset.title)
          setAsset(object.merge(asset, { title }))
          try {
            if (isCloud) {
              try {
                const newSmartAsset = await smartAsset.update({ file })
                rowState.setVisibility(Visibility.visible)
                setAsset(newSmartAsset.value)
              } catch (error) {
                toastAndLog(null, error)
              }
            } else {
              let id: string
              if (
                'backendApi' in window &&
                // This non-standard property is defined in Electron.
                'path' in file &&
                typeof file.path === 'string'
              ) {
                id = await window.backendApi.importProjectFromPath(file.path)
              } else {
                const response = await fetch('./api/upload-project', {
                  method: 'POST',
                  // Ideally this would use `file.stream()`, to minimize RAM
                  // requirements. for uploading large projects. Unfortunately,
                  // this requires HTTP/2, which is HTTPS-only, so it will not
                  // work on `http://localhost`.
                  body: await file.arrayBuffer(),
                })
                id = await response.text()
              }
              const listedProject = await smartAsset.getDetails()
              rowState.setVisibility(Visibility.visible)
              setAsset(
                object.merge(asset, {
                  title: listedProject.packageName,
                  id: backendModule.ProjectId(id),
                })
              )
            }
          } catch (error) {
            toastAndLog('Could not update project', error)
            break
          }
        }
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
      setRowState(object.merger({ isEditingName: true }))
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
          setRowState(object.merger({ isEditingName: true }))
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
          setRowState(object.merger({ isEditingName: false }))
        }}
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
