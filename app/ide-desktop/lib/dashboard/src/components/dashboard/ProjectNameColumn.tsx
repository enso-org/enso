/** @file The icon and name of a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import NetworkIcon from 'enso-assets/network.svg'

import * as eventHooks from '#/hooks/eventHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import type * as column from '#/components/dashboard/column'
import ProjectIcon from '#/components/dashboard/ProjectIcon'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as shortcutManagerModule from '#/utilities/ShortcutManager'
import * as string from '#/utilities/string'
import * as validation from '#/utilities/validation'
import Visibility from '#/utilities/visibility'

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
  const { numberOfSelectedItems, assetEvents, dispatchAssetEvent, dispatchAssetListEvent } = state
  const { nodeMap, doOpenManually, doOpenIde, doCloseIde } = state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { backend } = backendProvider.useBackend()
  const { organization } = authProvider.useNonPartialUserSession()
  const { shortcutManager } = shortcutManagerProvider.useShortcutManager()
  const asset = item.item
  if (asset.type !== backendModule.AssetType.project) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`ProjectNameColumn` can only display projects.')
  }
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const ownPermission =
    asset.permissions?.find(permission => permission.user.user_email === organization?.email) ??
    null
  // This is a workaround for a temporary bad state in the backend causing the `projectState` key
  // to be absent.
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = asset.projectState ?? {
    type: backendModule.ProjectState.closed,
  }
  const isRunning = backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[projectState.type]
  const canExecute =
    backend.type === backendModule.BackendType.local ||
    (ownPermission != null && permissions.PERMISSION_ACTION_CAN_EXECUTE[ownPermission.permission])
  const isOtherUserUsingProject =
    backend.type !== backendModule.BackendType.local &&
    projectState.opened_by != null &&
    projectState.opened_by !== organization?.email

  const doRename = async (newTitle: string) => {
    setRowState(object.merger({ isEditingName: false }))
    if (string.isWhitespaceOnly(newTitle)) {
      // Do nothing.
    } else if (newTitle !== asset.title) {
      const oldTitle = asset.title
      setAsset(object.merger({ title: newTitle }))
      try {
        await backend.updateProject(
          asset.id,
          { ami: null, ideVersion: null, projectName: newTitle },
          asset.title
        )
      } catch (error) {
        toastAndLog('Could not rename project', error)
        setAsset(object.merger({ title: oldTitle }))
      }
    }
  }

  eventHooks.useEventHandler(assetEvents, async event => {
    switch (event.type) {
      case AssetEventType.newFolder:
      case AssetEventType.newSecret:
      case AssetEventType.openProject:
      case AssetEventType.closeProject:
      case AssetEventType.cancelOpeningAllProjects:
      case AssetEventType.copy:
      case AssetEventType.cut:
      case AssetEventType.cancelCut:
      case AssetEventType.move:
      case AssetEventType.delete:
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
        // `deleteMultiple`, `restoreMultiple`, `download`, and `downloadSelected`
        // are handled by `AssetRow`.
        break
      }
      case AssetEventType.newProject: {
        // This should only run before this project gets replaced with the actual project
        // by this event handler. In both cases `key` will match, so using `key` here
        // is a mistake.
        if (asset.id === event.placeholderId) {
          rowState.setVisibility(Visibility.faded)
          try {
            const createdProject = await backend.createProject({
              parentDirectoryId: asset.parentId,
              projectName: asset.title,
              projectTemplateName: event.templateId,
            })
            rowState.setVisibility(Visibility.visible)
            setAsset(
              object.merge(asset, {
                id: createdProject.projectId,
                projectState: object.merge(projectState, {
                  type: backendModule.ProjectState.placeholder,
                }),
              })
            )
            dispatchAssetEvent({
              type: AssetEventType.openProject,
              id: createdProject.projectId,
              shouldAutomaticallySwitchPage: true,
              runInBackground: false,
            })
          } catch (error) {
            dispatchAssetListEvent({
              type: AssetListEventType.delete,
              key: item.key,
            })
            toastAndLog('Error creating new project', error)
          }
        }
        break
      }
      case AssetEventType.updateFiles:
      case AssetEventType.uploadFiles: {
        const file = event.files.get(item.key)
        if (file != null) {
          const fileId = event.type !== AssetEventType.updateFiles ? null : asset.id
          rowState.setVisibility(Visibility.faded)
          const { extension } = backendModule.extractProjectExtension(file.name)
          const title = backendModule.stripProjectExtension(asset.title)
          setAsset(object.merge(asset, { title }))
          try {
            if (backend.type === backendModule.BackendType.local) {
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
              const listedProject = await backend.getProjectDetails(
                backendModule.ProjectId(id),
                null
              )
              rowState.setVisibility(Visibility.visible)
              setAsset(
                object.merge(asset, {
                  title: listedProject.packageName,
                  id: backendModule.ProjectId(id),
                })
              )
            } else {
              const createdFile = await backend.uploadFile(
                {
                  fileId,
                  fileName: `${title}.${extension}`,
                  parentDirectoryId: asset.parentId,
                },
                file
              )
              const project = createdFile.project
              if (project == null) {
                throw new Error('The uploaded file was not a project.')
              } else {
                rowState.setVisibility(Visibility.visible)
                setAsset(
                  object.merge(asset, {
                    title,
                    id: project.projectId,
                    projectState: project.state,
                  })
                )
                return
              }
            }
          } catch (error) {
            switch (event.type) {
              case AssetEventType.uploadFiles: {
                dispatchAssetListEvent({
                  type: AssetListEventType.delete,
                  key: item.key,
                })
                toastAndLog('Could not upload project', error)
                break
              }
              case AssetEventType.updateFiles: {
                toastAndLog('Could not update project', error)
                break
              }
            }
          }
        }
        break
      }
    }
  })

  return (
    <div
      className={`flex text-left items-center whitespace-nowrap rounded-l-full gap-1 px-1.5 py-1 min-w-max ${indent.indentClass(
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
        } else if (
          shortcutManager.matchesMouseAction(shortcutManagerModule.MouseAction.open, event)
        ) {
          // It is a double click; open the project.
          dispatchAssetEvent({
            type: AssetEventType.openProject,
            id: asset.id,
            shouldAutomaticallySwitchPage: true,
            runInBackground: false,
          })
        } else if (
          shortcutManager.matchesMouseAction(shortcutManagerModule.MouseAction.run, event)
        ) {
          dispatchAssetEvent({
            type: AssetEventType.openProject,
            id: asset.id,
            shouldAutomaticallySwitchPage: false,
            runInBackground: true,
          })
        } else if (
          !isRunning &&
          eventModule.isSingleClick(event) &&
          ((selected && numberOfSelectedItems === 1) ||
            shortcutManager.matchesMouseAction(shortcutManagerModule.MouseAction.editName, event))
        ) {
          setRowState(object.merger({ isEditingName: true }))
        }
      }}
    >
      {!canExecute ? (
        <SvgMask src={NetworkIcon} className="m-1" />
      ) : (
        <ProjectIcon
          keyProp={item.key}
          // This is a workaround for a temporary bad state in the backend causing the
          // `projectState` key to be absent.
          item={object.merge(asset, { projectState })}
          setItem={setAsset}
          assetEvents={assetEvents}
          doOpenManually={doOpenManually}
          openIde={switchPage => {
            doOpenIde(asset, setAsset, switchPage)
          }}
          onClose={() => {
            doCloseIde(asset)
          }}
        />
      )}
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={`bg-transparent grow leading-170 h-6 py-px ${
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
              backendModule.assetIsDirectory(child.item) ||
              // must have a different name.
              child.item.title !== newTitle
          )
        }
        onSubmit={doRename}
        onCancel={() => {
          setRowState(object.merger({ isEditingName: false }))
        }}
        {...(backend.type === backendModule.BackendType.local
          ? {
              inputPattern: validation.LOCAL_PROJECT_NAME_PATTERN,
              inputTitle: validation.LOCAL_PROJECT_NAME_TITLE,
            }
          : {})}
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
