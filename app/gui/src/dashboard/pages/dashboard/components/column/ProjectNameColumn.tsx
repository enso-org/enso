/** @file The icon and name of a {@link ProjectAsset}. */
import EditableSpan from '#/components/EditableSpan'
import { useRenameAsset } from '#/hooks/backendHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import type { AssetNameColumnProps } from '#/pages/dashboard/components/column'
import ProjectIcon, { CLOSED_PROJECT_STATE } from '#/pages/dashboard/components/ProjectIcon'
import { useDriveStore } from '#/providers/DriveProvider'
import { isDoubleClick } from '#/utilities/event'
import { twMerge } from '#/utilities/tailwindMerge'
import { useFullUserSession } from '$/providers/react'
import { useOpenedProjects } from '$/providers/react/openedProjects'
import { BackendType, titleSchema, type ProjectAsset } from 'enso-common/src/services/Backend'
import { isOnMacOS } from 'enso-common/src/utilities/detect'
import {
  PERMISSION_ACTION_CAN_EXECUTE,
  tryFindSelfPermission,
} from 'enso-common/src/utilities/permissions'
import { useStore } from 'zustand'

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends AssetNameColumnProps {
  readonly item: ProjectAsset
}

/** The icon and name of a {@link ProjectAsset}. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
  const { item, isEditable, isPlaceholder } = props

  const { associatedBackend: backend } = useCategoriesAPI()
  const { user } = useFullUserSession()
  const getAssetChildren = useGetAssetChildren()
  const renameAsset = useRenameAsset(backend)
  const driveStore = useDriveStore()
  const openedProjects = useOpenedProjects()

  const isEditingName = useStore(driveStore, ({ assetToRename }) => assetToRename === item.id)
  const setIsEditing = (isEditing: boolean) => {
    if (isEditing) {
      if (isEditable) {
        driveStore.setState({ assetToRename: item.id })
      }
    } else {
      driveStore.setState({ assetToRename: null })
    }
  }

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

  const doRename = async (newTitle: string) => {
    await renameAsset(item.id, newTitle)
    setIsEditing(false)
  }

  return (
    <div
      className="flex h-table-row w-auto min-w-48 max-w-full items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (isEditingName && isOnMacOS() && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={(event) => {
        if (isEditingName || isOtherUserUsingProject) {
          // The project should neither be edited nor opened in these cases.
        } else if (isDoubleClick(event) && canExecute) {
          openedProjects.openProjectLocally(item, backend.type)
        }
      }}
    >
      <ProjectIcon
        isDisabled={!canExecute}
        backend={backend}
        item={item}
        isPlaceholder={isPlaceholder}
      />

      <EditableSpan
        data-testid="asset-row-name"
        editable={isEditingName}
        className={twMerge(
          'grow bg-transparent font-naming',
          canExecute && !isOtherUserUsingProject && 'cursor-pointer',
          isEditingName && 'cursor-text',
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
