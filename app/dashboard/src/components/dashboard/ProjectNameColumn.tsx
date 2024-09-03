/** @file The icon and name of a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import { useMutation, useQueryClient } from '@tanstack/react-query'

import NetworkIcon from '#/assets/network.svg'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import { useDriveStore } from '#/providers/DriveProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import type * as column from '#/components/dashboard/column'
import ProjectIcon from '#/components/dashboard/ProjectIcon'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'
import * as localBackend from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'

import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as string from '#/utilities/string'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import * as validation from '#/utilities/validation'
import Visibility from '#/utilities/Visibility'
import { isOnMacOS } from 'enso-common/src/detect'

// ===================
// === ProjectName ===
// ===================

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.ProjectAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.ProjectAsset}.
 * This should never happen. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
  const {
    item,
    setItem,
    selected,
    rowState,
    setRowState,
    state,
    isEditable,
    backendType,
    isOpened,
  } = props
  const { backend, nodeMap } = state
  const client = useQueryClient()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const driveStore = useDriveStore()
  const doOpenProject = projectHooks.useOpenProject()

  if (item.type !== backendModule.AssetType.project) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`ProjectNameColumn` can only display projects.')
  }
  const asset = item.item
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const ownPermission =
    asset.permissions?.find(
      backendModule.isUserPermissionAnd((permission) => permission.user.userId === user.userId),
    ) ?? null
  // This is a workaround for a temporary bad state in the backend causing the `projectState` key
  // to be absent.
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = asset.projectState ?? {
    type: backendModule.ProjectState.closed,
  }
  const isRunning = backendModule.IS_OPENING_OR_OPENED[projectState.type]
  const canExecute =
    isEditable &&
    (backend.type === backendModule.BackendType.local ||
      (ownPermission != null &&
        permissions.PERMISSION_ACTION_CAN_EXECUTE[ownPermission.permission]))
  const isCloud = backend.type === backendModule.BackendType.remote
  const isOtherUserUsingProject =
    isCloud && projectState.openedBy != null && projectState.openedBy !== user.email

  const createProjectMutation = useMutation(backendMutationOptions(backend, 'createProject'))
  const updateProjectMutation = useMutation(backendMutationOptions(backend, 'updateProject'))
  const duplicateProjectMutation = useMutation(backendMutationOptions(backend, 'duplicateProject'))
  const getProjectDetailsMutation = useMutation(
    backendMutationOptions(backend, 'getProjectDetails'),
  )
  const uploadFileMutation = useMutation(
    backendMutationOptions(backend, 'uploadFile', {
      meta: {
        invalidates: [['assetVersions', item.item.id, item.item.title]],
        awaitInvalidates: true,
      },
    }),
  )

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
        await updateProjectMutation.mutateAsync([
          asset.id,
          { ami: null, ideVersion: null, projectName: newTitle },
          asset.title,
        ])
        await client.invalidateQueries({
          queryKey: projectHooks.createGetProjectDetailsQuery.getQueryKey(asset.id),
        })
      } catch (error) {
        toastAndLog('renameProjectError', error)
        setAsset(object.merger({ title: oldTitle }))
      }
    }
  }

  eventListProvider.useAssetEventListener(async (event) => {
    if (isEditable) {
      switch (event.type) {
        case AssetEventType.newFolder:
        case AssetEventType.newDatalink:
        case AssetEventType.newSecret:
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
        case AssetEventType.deleteLabel:
        case AssetEventType.setItem:
        case AssetEventType.projectClosed: {
          // Ignored. Any missing project-related events should be handled by `ProjectIcon`.
          // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
          // are handled by`AssetRow`.
          break
        }
        case AssetEventType.newProject: {
          // This should only run before this project gets replaced with the actual project
          // by this event handler. In both cases `key` will match, so using `key` here
          // is a mistake.
          if (asset.id === event.placeholderId) {
            rowState.setVisibility(Visibility.faded)
            try {
              const createdProject =
                event.originalId == null || event.versionId == null ?
                  await createProjectMutation.mutateAsync([
                    {
                      parentDirectoryId: asset.parentId,
                      projectName: asset.title,
                      ...(event.templateId == null ?
                        {}
                      : { projectTemplateName: event.templateId }),
                      ...(event.datalinkId == null ? {} : { datalinkId: event.datalinkId }),
                    },
                  ])
                : await duplicateProjectMutation.mutateAsync([
                    event.originalId,
                    event.versionId,
                    asset.title,
                  ])
              event.onCreated?.(createdProject)
              rowState.setVisibility(Visibility.visible)
              setAsset(
                object.merge(asset, {
                  id: createdProject.projectId,
                  projectState: object.merge(projectState, {
                    type: backendModule.ProjectState.placeholder,
                  }),
                }),
              )
              doOpenProject({
                id: createdProject.projectId,
                type: backendType,
                parentId: asset.parentId,
                title: asset.title,
              })
            } catch (error) {
              event.onError?.()
              dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
              toastAndLog('createProjectError', error)
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
                const directory = localBackend.extractTypeAndId(item.directoryId).id
                let id: string
                if (
                  'backendApi' in window &&
                  // This non-standard property is defined in Electron.
                  'path' in file &&
                  typeof file.path === 'string'
                ) {
                  id = await window.backendApi.importProjectFromPath(file.path, directory, title)
                } else {
                  const searchParams = new URLSearchParams({ directory, name: title }).toString()
                  // Ideally this would use `file.stream()`, to minimize RAM
                  // requirements. for uploading large projects. Unfortunately,
                  // this requires HTTP/2, which is HTTPS-only, so it will not
                  // work on `http://localhost`.
                  const body =
                    window.location.protocol === 'https:' ? file.stream() : await file.arrayBuffer()
                  const path = `./api/upload-project?${searchParams}`
                  const response = await fetch(path, { method: 'POST', body })
                  id = await response.text()
                }
                const projectId = localBackend.newProjectId(projectManager.UUID(id))
                const listedProject = await getProjectDetailsMutation.mutateAsync([
                  projectId,
                  asset.parentId,
                  file.name,
                ])
                rowState.setVisibility(Visibility.visible)
                setAsset(object.merge(asset, { title: listedProject.packageName, id: projectId }))
              } else {
                const createdFile = await uploadFileMutation.mutateAsync([
                  {
                    fileId,
                    fileName: `${title}.${extension}`,
                    parentDirectoryId: asset.parentId,
                  },
                  file,
                ])
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
                    }),
                  )
                  return
                }
              }
            } catch (error) {
              switch (event.type) {
                case AssetEventType.uploadFiles: {
                  dispatchAssetListEvent({ type: AssetListEventType.delete, key: item.key })
                  toastAndLog('uploadProjectError', error)
                  break
                }
                case AssetEventType.updateFiles: {
                  toastAndLog('updateProjectError', error)
                  break
                }
              }
            }
          }
          break
        }
      }
    }
  }, item.initialAssetEvents)

  const handleClick = inputBindings.handler({
    editName: () => {
      setIsEditing(true)
    },
  })

  return (
    <div
      className={tailwindMerge.twMerge(
        'flex h-table-row min-w-max items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y',
        indent.indentClass(item.depth),
      )}
      onKeyDown={(event) => {
        if (rowState.isEditingName && isOnMacOS() && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={(event) => {
        if (rowState.isEditingName || isOtherUserUsingProject) {
          // The project should neither be edited nor opened in these cases.
        } else if (handleClick(event)) {
          // Already handled.
        } else if (
          !isRunning &&
          eventModule.isSingleClick(event) &&
          selected &&
          driveStore.getState().selectedKeys.size === 1
        ) {
          setIsEditing(true)
        } else if (eventModule.isDoubleClick(event)) {
          doOpenProject({
            id: asset.id,
            type: backendType,
            parentId: asset.parentId,
            title: asset.title,
          })
        }
      }}
    >
      {!canExecute ?
        <SvgMask src={NetworkIcon} className="m-name-column-icon size-4" />
      : <ProjectIcon
          isOpened={isOpened}
          backend={backend}
          // This is a workaround for a temporary bad state in the backend causing the
          // `projectState` key to be absent.
          item={object.merge(asset, { projectState })}
        />
      }
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={tailwindMerge.twMerge(
          'text grow bg-transparent font-naming',
          canExecute && !isOtherUserUsingProject && 'cursor-pointer',
          rowState.isEditingName && 'cursor-text',
        )}
        checkSubmittable={(newTitle) =>
          item.isNewTitleValid(newTitle, nodeMap.current.get(item.directoryKey)?.children)
        }
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        {...(backend.type === backendModule.BackendType.local ?
          {
            inputPattern: validation.LOCAL_PROJECT_NAME_PATTERN,
            inputTitle: getText('projectNameCannotBeEmpty'),
          }
        : {})}
      >
        {asset.title}
      </EditableSpan>
    </div>
  )
}
