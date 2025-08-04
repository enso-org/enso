/** @file The icon and name of a {@link ProjectAsset}. */
import EditableSpan from '#/components/EditableSpan'
import { useRenameAsset } from '#/hooks/backendHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import type { AssetNameColumnProps } from '#/pages/dashboard/components/column'
import ProjectIcon, { CLOSED_PROJECT_STATE } from '#/pages/dashboard/components/ProjectIcon'
import { BackendType, titleSchema, type ProjectAsset } from '#/services/Backend'
import { isDoubleClick } from '#/utilities/event'
import { merger } from '#/utilities/object'
import { PERMISSION_ACTION_CAN_EXECUTE, tryFindSelfPermission } from '#/utilities/permissions'
import { twMerge } from '#/utilities/tailwindMerge'
import { useFullUserSession } from '$/providers/react'
import { isOnMacOS } from 'enso-common/src/detect'

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends AssetNameColumnProps {
  readonly item: ProjectAsset
}

/** The icon and name of a {@link ProjectAsset}. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
  const {
    item,
    rowState,
    setRowState,
    state,
    isEditable,
    isOpened,
    isPlaceholder,
    closeProject,
    openProject,
  } = props
  const { backend } = state

  const { user } = useFullUserSession()
  const getAssetChildren = useGetAssetChildren()
  const renameAsset = useRenameAsset(backend)

  const ownPermission = tryFindSelfPermission(user, item.permissions)
  // This is a workaround for a temporary bad state in the backend causing the `projectState` key
  // to be absent.
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = item.projectState ?? CLOSED_PROJECT_STATE
  const canExecute =
    isEditable &&
    (backend.type === BackendType.local ||
      (ownPermission != null && PERMISSION_ACTION_CAN_EXECUTE[ownPermission.permission]))
  const isCloud = backend.type === BackendType.remote
  const isOtherUserUsingProject =
    isCloud && projectState.openedBy != null && projectState.openedBy !== user.email

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(merger({ isEditingName }))
    }
  }

  const doRename = async (newTitle: string) => {
    await renameAsset(item.id, newTitle)
    setIsEditing(false)
  }

  return (
    <div
      className="flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (rowState.isEditingName && isOnMacOS() && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={async (event) => {
        if (rowState.isEditingName || isOtherUserUsingProject) {
          // The project should neither be edited nor opened in these cases.
        } else if (isDoubleClick(event) && canExecute) {
          await openProject(item.id)
        }
      }}
    >
      <ProjectIcon
        isDisabled={!canExecute}
        isOpened={isOpened}
        backend={backend}
        item={item}
        isPlaceholder={isPlaceholder}
        closeProject={closeProject}
        openProject={openProject}
      />

      <EditableSpan
        data-testid="asset-row-name"
        editable={rowState.isEditingName}
        className={twMerge(
          'grow bg-transparent font-naming',
          canExecute && !isOtherUserUsingProject && 'cursor-pointer',
          rowState.isEditingName && 'cursor-text',
        )}
        onSubmit={doRename}
        onCancel={() => {
          setIsEditing(false)
        }}
        schema={() =>
          titleSchema({
            id: item.id,
            siblings: getAssetChildren(item.parentId),
          })
        }
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
