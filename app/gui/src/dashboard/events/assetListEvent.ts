/** @file Events related to changes in the asset list. */
import AssetListEventType from '#/events/AssetListEventType'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useDispatchAssetListEvent } from '#/layouts/AssetsTable/EventListProvider'
import { useTransferBetweenCategories, type Category } from '#/layouts/CategorySwitcher/Category'
import type { DrivePastePayload } from '#/providers/DriveProvider'

import type * as backend from '#/services/Backend'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import { isTeamPath, isUserPath } from '#/utilities/permissions'

// ======================
// === AssetListEvent ===
// ======================

/** Properties common to all asset list events. */
interface AssetListBaseEvent<Type extends AssetListEventType> {
  readonly type: Type
}

/** All possible events. */
interface AssetListEvents {
  readonly newFolder: AssetListNewFolderEvent
  readonly newProject: AssetListNewProjectEvent
  readonly uploadFiles: AssetListUploadFilesEvent
  readonly newSecret: AssetListNewSecretEvent
  readonly newDatalink: AssetListNewDatalinkEvent
  readonly duplicateProject: AssetListDuplicateProjectEvent
  readonly closeFolder: AssetListCloseFolderEvent
  readonly copy: AssetListCopyEvent
  readonly move: AssetListMoveEvent
  readonly delete: AssetListDeleteEvent
  readonly emptyTrash: AssetListEmptyTrashEvent
  readonly removeSelf: AssetListRemoveSelfEvent
}

/** A type to ensure that {@link AssetListEvents} contains every {@link AssetListEventType}. */
// This is meant only as a sanity check, so it is allowed to break lint rules.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
type SanityCheck<
  T extends {
    readonly [Type in keyof typeof AssetListEventType]: AssetListBaseEvent<
      (typeof AssetListEventType)[Type]
    >
  } = AssetListEvents,
> = [T]

/** A signal to create a new directory. */
interface AssetListNewFolderEvent extends AssetListBaseEvent<AssetListEventType.newFolder> {
  readonly parentKey: backend.DirectoryId
  readonly parentId: backend.DirectoryId
}

/** A signal to create a new project. */
interface AssetListNewProjectEvent extends AssetListBaseEvent<AssetListEventType.newProject> {
  readonly parentKey: backend.DirectoryId
  readonly parentId: backend.DirectoryId
  readonly templateId: string | null
  readonly datalinkId: backend.DatalinkId | null
  readonly preferredName: string | null
  readonly onCreated?: (project: backend.CreatedProject) => void
  readonly onError?: () => void
}

/** A signal to upload files. */
interface AssetListUploadFilesEvent extends AssetListBaseEvent<AssetListEventType.uploadFiles> {
  readonly parentKey: backend.DirectoryId
  readonly parentId: backend.DirectoryId
  readonly files: File[]
}

/** A signal to create a new secret. */
interface AssetListNewDatalinkEvent extends AssetListBaseEvent<AssetListEventType.newDatalink> {
  readonly parentKey: backend.DirectoryId
  readonly parentId: backend.DirectoryId
  readonly name: string
  readonly value: unknown
}

/** A signal to create a new secret. */
interface AssetListNewSecretEvent extends AssetListBaseEvent<AssetListEventType.newSecret> {
  readonly parentKey: backend.DirectoryId
  readonly parentId: backend.DirectoryId
  readonly name: string
  readonly value: string
}

/** A signal to duplicate a project. */
interface AssetListDuplicateProjectEvent
  extends AssetListBaseEvent<AssetListEventType.duplicateProject> {
  readonly parentKey: backend.DirectoryId
  readonly parentId: backend.DirectoryId
  readonly original: backend.ProjectAsset
  readonly versionId: backend.S3ObjectVersionId
}

/** A signal to close (collapse) a folder. */
interface AssetListCloseFolderEvent extends AssetListBaseEvent<AssetListEventType.closeFolder> {
  readonly id: backend.DirectoryId
  readonly key: backend.DirectoryId
}

/** A signal that files should be copied. */
interface AssetListCopyEvent extends AssetListBaseEvent<AssetListEventType.copy> {
  readonly newParentKey: backend.DirectoryId
  readonly newParentId: backend.DirectoryId
  readonly items: backend.AnyAsset[]
}

/** A signal that a file has been moved. */
interface AssetListMoveEvent extends AssetListBaseEvent<AssetListEventType.move> {
  readonly key: backend.AssetId
  readonly newParentKey: backend.DirectoryId
  readonly newParentId: backend.DirectoryId
  readonly items: backend.AnyAsset[]
}

/**
 * A signal that a file has been deleted. This must not be called before the request is
 * finished.
 */
interface AssetListDeleteEvent extends AssetListBaseEvent<AssetListEventType.delete> {
  readonly key: backend.AssetId
}

/** A signal to permanently delete all files in Trash. */
type AssetListEmptyTrashEvent = AssetListBaseEvent<AssetListEventType.emptyTrash>

/** A signal for a file to remove itself from the asset list, without being deleted. */
interface AssetListRemoveSelfEvent extends AssetListBaseEvent<AssetListEventType.removeSelf> {
  readonly id: backend.AssetId
}

/** Every possible type of asset list event. */
export type AssetListEvent = AssetListEvents[keyof AssetListEvents]

/**
 * A hook to copy or move assets as appropriate. Assets are moved, except when performing
 * a cut and paste between the Team Space and the User Space, in which case the asset is copied.
 */
export function useCutAndPaste(category: Category) {
  const transferBetweenCategories = useTransferBetweenCategories(category)
  const dispatchAssetListEvent = useDispatchAssetListEvent()
  return useEventCallback(
    (
      newParentKey: backend.DirectoryId,
      newParentId: backend.DirectoryId,
      pasteData: DrivePastePayload,
      nodeMap: ReadonlyMap<backend.AssetId, AnyAssetTreeNode>,
    ) => {
      const ids = Array.from(pasteData.ids)
      const nodes = ids.flatMap((id) => {
        const item = nodeMap.get(id)
        return item == null ? [] : [item]
      })
      const newParent = nodeMap.get(newParentKey)
      const isMovingToUserSpace = newParent?.path != null && isUserPath(newParent.path)
      const teamToUserItems =
        isMovingToUserSpace ?
          nodes.filter((node) => isTeamPath(node.path)).map((otherItem) => otherItem.item)
        : []
      const nonTeamToUserIds =
        isMovingToUserSpace ?
          nodes.filter((node) => !isTeamPath(node.path)).map((otherItem) => otherItem.item.id)
        : ids
      if (teamToUserItems.length !== 0) {
        dispatchAssetListEvent({
          type: AssetListEventType.copy,
          newParentKey,
          newParentId,
          items: teamToUserItems,
        })
      }
      if (nonTeamToUserIds.length !== 0) {
        transferBetweenCategories(
          pasteData.category,
          category,
          pasteData.ids,
          newParentKey,
          newParentId,
        )
      }
    },
  )
}
