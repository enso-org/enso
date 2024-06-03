/** @file The icon and name of a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import NetworkIcon from 'enso-assets/network.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

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
import * as validation from '#/utilities/validation'

// ===================
// === ProjectName ===
// ===================

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.ProjectAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.ProjectAsset}.
 * This should never happen. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
  const { item, setItem: setItemRaw, depth, selected, rowState, setRowState, state } = props
  const { isEditable } = props
  const { backend, selectedKeys, nodeMap, setProjectStartupInfo, doOpenEditor, doCloseEditor } =
    state
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useNonPartialUserSession()
  const { session } = sessionProvider.useSession()
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  if (item.type !== backendModule.AssetType.project) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('`ProjectNameColumn` can only display projects.')
  }
  const setItem = setAssetHooks.useSetAsset(item, setItemRaw)
  const ownPermission =
    item.permissions?.find(
      backendModule.isUserPermissionAnd(permission => permission.user.userId === user?.userId)
    ) ?? null
  // This is a workaround for a temporary bad state in the backend causing the `projectState` key
  // to be absent.
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = item.projectState ?? {
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
    isCloud && projectState.openedBy != null && projectState.openedBy !== user?.email

  const openProjectMutation = backendHooks.useBackendMutation(backend, 'openProject')
  const updateProjectMutation = backendHooks.useBackendMutation(backend, 'updateProject')

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(object.merger({ isEditingName }))
    }
  }

  const doRename = async (newTitle: string) => {
    setIsEditing(false)

    if (string.isWhitespaceOnly(newTitle)) {
      // Do nothing.
    } else if (newTitle !== item.title) {
      const oldTitle = item.title
      setItem(object.merger({ title: newTitle }))
      try {
        await updateProjectMutation.mutateAsync([
          item.id,
          { ami: null, ideVersion: null, projectName: newTitle, parentId: item.parentId },
          item.title,
        ])
      } catch (error) {
        toastAndLog('renameProjectError', error)
        setItem(object.merger({ title: oldTitle }))
      }
    }
  }

  const handleClick = inputBindings.handler({
    open: () => {
      openProjectMutation.mutate([
        item.id,
        {
          executeAsync: false,
          parentId: item.parentId,
          cognitoCredentials: session,
        },
        item.title,
      ])
    },
    run: () => {
      openProjectMutation.mutate([
        item.id,
        {
          executeAsync: true,
          parentId: item.parentId,
          cognitoCredentials: session,
        },
        item.title,
      ])
    },
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
          backend={backend}
          // This is a workaround for a temporary bad state in the backend causing the
          // `projectState` key to be absent.
          item={object.merge(item, { projectState })}
          setItem={setItem}
          setProjectStartupInfo={setProjectStartupInfo}
          doOpenEditor={switchPage => {
            doOpenEditor(item, setItem, switchPage)
          }}
          doCloseEditor={() => {
            doCloseEditor(item)
          }}
        />
      )}
      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={tailwindMerge.twMerge(
          'text grow bg-transparent',
          canExecute && !isOtherUserUsingProject && 'cursor-pointer',
          rowState.isEditingName && 'cursor-text'
        )}
        checkSubmittable={newTitle =>
          newTitle !== item.title &&
          (nodeMap.current.get(item.parentId)?.children ?? []).every(
            child =>
              // All siblings,
              child.key === item.id ||
              // that are not directories,
              backendModule.assetIsDirectory(child.item) ||
              // must have a different name.
              child.item.title !== newTitle
          )
        }
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        {...(backend.type === backendModule.BackendType.local
          ? {
              inputPattern: validation.LOCAL_PROJECT_NAME_PATTERN,
              inputTitle: getText('projectNameCannotBeEmpty'),
            }
          : {})}
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
