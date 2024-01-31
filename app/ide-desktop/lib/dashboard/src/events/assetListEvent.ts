/** @file Events related to changes in the asset list. */
import type AssetListEventType from '#/events/AssetListEventType'

import type * as spinner from '#/components/Spinner'

import type * as backend from '#/services/Backend'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '#/hooks/eventHooks' {
  /** A map containing all known event types. */
  export interface KnownEventsMap {
    assetListEvent: AssetListEvent
  }
}

// ======================
// === AssetListEvent ===
// ======================

/** Properties common to all asset list events. */
interface AssetListBaseEvent<Type extends AssetListEventType> {
  type: Type
}

/** All possible events. */
interface AssetListEvents {
  newFolder: AssetListNewFolderEvent
  newProject: AssetListNewProjectEvent
  uploadFiles: AssetListUploadFilesEvent
  newSecret: AssetListNewSecretEvent
  insertAssets: AssetListInsertAssetsEvent
  closeFolder: AssetListCloseFolderEvent
  copy: AssetListCopyEvent
  move: AssetListMoveEvent
  willDelete: AssetListWillDeleteEvent
  delete: AssetListDeleteEvent
  removeSelf: AssetListRemoveSelfEvent
}

/** A type to ensure that {@link AssetListEvents} contains every {@link AssetListEventType}. */
// This is meant only as a sanity check, so it is allowed to break lint rules.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
type SanityCheck<
  T extends {
    [Type in keyof typeof AssetListEventType]: AssetListBaseEvent<(typeof AssetListEventType)[Type]>
  } = AssetListEvents,
> = [T]

/** A signal to create a new directory. */
interface AssetListNewFolderEvent extends AssetListBaseEvent<AssetListEventType.newFolder> {
  parentKey: backend.DirectoryId
  parentId: backend.DirectoryId
}

/** A signal to create a new project. */
interface AssetListNewProjectEvent extends AssetListBaseEvent<AssetListEventType.newProject> {
  parentKey: backend.DirectoryId
  parentId: backend.DirectoryId
  templateId: string | null
  templateName: string | null
  onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null
}

/** A signal to upload files. */
interface AssetListUploadFilesEvent extends AssetListBaseEvent<AssetListEventType.uploadFiles> {
  parentKey: backend.DirectoryId
  parentId: backend.DirectoryId
  files: File[]
}

/** A signal to create a new secret. */
interface AssetListNewSecretEvent extends AssetListBaseEvent<AssetListEventType.newSecret> {
  parentKey: backend.DirectoryId
  parentId: backend.DirectoryId
  name: string
  value: string
}

/** A signal to insert new assets. The assets themselves need to be created by the caller. */
interface AssetListInsertAssetsEvent extends AssetListBaseEvent<AssetListEventType.insertAssets> {
  parentKey: backend.DirectoryId
  parentId: backend.DirectoryId
  assets: backend.AnyAsset[]
}

/** A signal to close (collapse) a folder. */
interface AssetListCloseFolderEvent extends AssetListBaseEvent<AssetListEventType.closeFolder> {
  id: backend.DirectoryId
  key: backend.DirectoryId
}

/** A signal that files should be copied. */
interface AssetListCopyEvent extends AssetListBaseEvent<AssetListEventType.copy> {
  newParentKey: backend.AssetId
  newParentId: backend.DirectoryId
  items: backend.AnyAsset[]
}

/** A signal that a file has been moved. */
interface AssetListMoveEvent extends AssetListBaseEvent<AssetListEventType.move> {
  key: backend.AssetId
  newParentKey: backend.AssetId
  newParentId: backend.DirectoryId
  item: backend.AnyAsset
}

/** A signal that a file has been deleted. */
interface AssetListWillDeleteEvent extends AssetListBaseEvent<AssetListEventType.willDelete> {
  key: backend.AssetId
}

/** A signal that a file has been deleted. This must not be called before the request is
 * finished. */
interface AssetListDeleteEvent extends AssetListBaseEvent<AssetListEventType.delete> {
  key: backend.AssetId
}

/** A signal for a file to remove itself from the asset list, without being deleted. */
interface AssetListRemoveSelfEvent extends AssetListBaseEvent<AssetListEventType.removeSelf> {
  id: backend.AssetId
}

/** Every possible type of asset list event. */
export type AssetListEvent = AssetListEvents[keyof AssetListEvents]
