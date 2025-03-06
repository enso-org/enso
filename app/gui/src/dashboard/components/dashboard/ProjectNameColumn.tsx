/** @file The icon and name of a {@link ProjectAsset}. */
import type { AssetColumnProps } from '#/components/dashboard/column'
import ProjectIcon, { CLOSED_PROJECT_STATE } from '#/components/dashboard/ProjectIcon'
import EditableSpan from '#/components/EditableSpan'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useOpenProject } from '#/hooks/projectHooks'
import { useGetAssetChildren } from '#/layouts/Drive/assetsTableItemsHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'
import { BackendType, isNewTitleUnique, type ProjectAsset } from '#/services/Backend'
import { isDoubleClick } from '#/utilities/event'
import { merger } from '#/utilities/object'
import { PERMISSION_ACTION_CAN_EXECUTE, tryFindSelfPermission } from '#/utilities/permissions'
import { twMerge } from '#/utilities/tailwindMerge'
import { useMutation } from '@tanstack/react-query'
import { isOnMacOS } from 'enso-common/src/detect'

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends AssetColumnProps {
  readonly item: ProjectAsset
}

/** The icon and name of a {@link ProjectAsset}. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
  const { item, rowState, setRowState, state, isEditable, backendType, isOpened, isPlaceholder } =
    props
  const { backend } = state

  const { user } = useFullUserSession()
  const { getText } = useText()
  const getAssetChildren = useGetAssetChildren()

  const doOpenProject = useOpenProject()
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

  const updateProjectMutation = useMutation(backendMutationOptions(backend, 'updateProject'))

  const setIsEditing = (isEditingName: boolean) => {
    if (isEditable) {
      setRowState(merger({ isEditingName }))
    }
  }

  const doRename = async (newTitle: string) => {
    await updateProjectMutation.mutateAsync([
      item.id,
      { ami: null, ideVersion: null, projectName: newTitle },
      item.title,
    ])
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
      onClick={(event) => {
        if (rowState.isEditingName || isOtherUserUsingProject) {
          // The project should neither be edited nor opened in these cases.
        } else if (isDoubleClick(event) && canExecute) {
          doOpenProject({
            id: item.id,
            type: backendType,
            parentId: item.parentId,
            title: item.title,
          })
        }
      }}
    >
      <ProjectIcon
        isDisabled={!canExecute}
        isOpened={isOpened}
        backend={backend}
        item={item}
        isPlaceholder={isPlaceholder}
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
        schema={(z) =>
          z.refine((value) => isNewTitleUnique(item, value, getAssetChildren(item.parentId)), {
            message: getText('nameShouldBeUnique'),
          })
        }
      >
        {item.title}
      </EditableSpan>
    </div>
  )
}
