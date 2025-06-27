/** @file The icon and name of a {@link ProjectAsset}. */
import EditableSpan from '#/components/EditableSpan'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useOpenProjectLocally } from '#/hooks/projectHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import ProjectIcon, { CLOSED_PROJECT_STATE } from '#/pages/dashboard/components/ProjectIcon'
import { useIsEditingName, useSetEditingNameAssetId } from '#/providers/DriveProvider'
import { BackendType, titleSchema, type ProjectAsset } from '#/services/Backend'
import { isDoubleClick } from '#/utilities/event'
import { PERMISSION_ACTION_CAN_EXECUTE, tryFindSelfPermission } from '#/utilities/permissions'
import { twMerge } from '#/utilities/tailwindMerge'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useFullUserSession } from '$/providers/react'
import { isOnMacOS } from 'enso-common/src/detect'

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends AssetColumnProps {
  readonly item: ProjectAsset
}

/** The icon and name of a {@link ProjectAsset}. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
  const { item, state, isEditable } = props
  const { backend } = state

  const isEditing = useIsEditingName(item.id) && isEditable
  const setEditingNameAssetId = useSetEditingNameAssetId()
  const { user } = useFullUserSession()
  const getAssetChildren = useGetAssetChildren()
  const openProjectLocally = useOpenProjectLocally()

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

  const updateAsset = useMutationCallback(
    backendMutationOptions(backend, 'updateAsset', {
      onSuccess: () => {
        setEditingNameAssetId(null)
      },
    }),
  )

  return (
    <div
      className="flex h-table-row items-center gap-name-column-icon whitespace-nowrap rounded-l-full px-name-column-x py-name-column-y rounded-rows-child"
      onKeyDown={(event) => {
        if (isEditing && isOnMacOS() && event.key === 'Enter') {
          event.stopPropagation()
        }
      }}
      onClick={async (event) => {
        if (isEditing || isOtherUserUsingProject) {
          // The project should neither be edited nor opened in these cases.
        } else if (isDoubleClick(event) && canExecute) {
          await openProjectLocally(item, backend.type)
        }
      }}
    >
      <ProjectIcon isDisabled={!canExecute} backend={backend} item={item} />

      <EditableSpan
        data-testid="asset-row-name"
        editable={isEditing}
        schema={() => titleSchema({ asset: item, siblings: getAssetChildren(item.parentId) })}
        className={twMerge(
          'grow bg-transparent font-naming',
          isEditing ? 'cursor-text' : canExecute && !isOtherUserUsingProject && 'cursor-pointer',
        )}
        onSubmit={(title) => updateAsset([item.id, { title }, item.title])}
        onCancel={() => {
          setEditingNameAssetId(null)
        }}
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
