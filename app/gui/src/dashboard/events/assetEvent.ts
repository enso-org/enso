/** @file Events related to changes in asset state. */
import type AssetEventType from '#/events/AssetEventType'

import type * as backend from '#/services/Backend'

// ==================
// === AssetEvent ===
// ==================

/** Properties common to all asset state change events. */
interface AssetBaseEvent<Type extends AssetEventType> {
  readonly type: Type
}

/** All possible events. */
interface AssetEvents {
  readonly newProject: AssetNewProjectEvent
  readonly newFolder: AssetNewFolderEvent
  readonly uploadFiles: AssetUploadFilesEvent
  readonly updateFiles: AssetUpdateFilesEvent
  readonly newDatalink: AssetNewDatalinkEvent
  readonly newSecret: AssetNewSecretEvent
  readonly copy: AssetCopyEvent
  readonly cut: AssetCutEvent
  readonly cancelCut: AssetCancelCutEvent
  readonly move: AssetMoveEvent
  readonly delete: AssetDeleteEvent
  readonly deleteForever: AssetDeleteForeverEvent
  readonly restore: AssetRestoreEvent
  readonly download: AssetDownloadEvent
  readonly downloadSelected: AssetDownloadSelectedEvent
  readonly removeSelf: AssetRemoveSelfEvent
  readonly temporarilyAddLabels: AssetTemporarilyAddLabelsEvent
  readonly temporarilyRemoveLabels: AssetTemporarilyRemoveLabelsEvent
  readonly addLabels: AssetAddLabelsEvent
  readonly removeLabels: AssetRemoveLabelsEvent
  readonly deleteLabel: AssetDeleteLabelEvent
  readonly setItem: AssetSetItemEvent
  readonly projectClosed: AssetProjectClosedEvent
}

/** A type to ensure that {@link AssetEvents} contains every {@link AssetEventType}. */
// This is meant only as a sanity check, so it is allowed to break lint rules.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
type SanityCheck<
  T extends {
    readonly [Type in keyof typeof AssetEventType]: AssetBaseEvent<(typeof AssetEventType)[Type]>
  } = AssetEvents,
> = [T]

/** A signal to create a project. */
export interface AssetNewProjectEvent extends AssetBaseEvent<AssetEventType.newProject> {
  readonly placeholderId: backend.ProjectId
  readonly templateId: string | null
  readonly datalinkId: backend.DatalinkId | null
  readonly originalId: backend.ProjectId | null
  readonly versionId: backend.S3ObjectVersionId | null
  readonly onCreated?: (project: backend.CreatedProject) => void
  readonly onError?: () => void
}

/** A signal to create a directory. */
export interface AssetNewFolderEvent extends AssetBaseEvent<AssetEventType.newFolder> {
  readonly placeholderId: backend.DirectoryId
}

/** A signal to upload files. */
export interface AssetUploadFilesEvent extends AssetBaseEvent<AssetEventType.uploadFiles> {
  readonly files: ReadonlyMap<backend.AssetId, File>
}

/** A signal to update files with new versions. */
export interface AssetUpdateFilesEvent extends AssetBaseEvent<AssetEventType.updateFiles> {
  readonly files: ReadonlyMap<backend.AssetId, File>
}

/** A signal to create a Datalink. */
export interface AssetNewDatalinkEvent extends AssetBaseEvent<AssetEventType.newDatalink> {
  readonly placeholderId: backend.DatalinkId
  readonly value: unknown
}

/** A signal to create a secret. */
export interface AssetNewSecretEvent extends AssetBaseEvent<AssetEventType.newSecret> {
  readonly placeholderId: backend.SecretId
  readonly value: string
}

/**
 * A signal that multiple assets should be copied. `ids` are the `Id`s of the newly created
 * placeholder items.
 */
export interface AssetCopyEvent extends AssetBaseEvent<AssetEventType.copy> {
  readonly ids: ReadonlySet<backend.AssetId>
  readonly newParentKey: backend.AssetId
  readonly newParentId: backend.DirectoryId
}

/** A signal to cut multiple assets. */
export interface AssetCutEvent extends AssetBaseEvent<AssetEventType.cut> {
  readonly ids: ReadonlySet<backend.AssetId>
}

/** A signal that a cut operation has been cancelled. */
export interface AssetCancelCutEvent extends AssetBaseEvent<AssetEventType.cancelCut> {
  readonly ids: ReadonlySet<backend.AssetId>
}

/** A signal to move multiple assets. */
export interface AssetMoveEvent extends AssetBaseEvent<AssetEventType.move> {
  readonly ids: ReadonlySet<backend.AssetId>
  readonly newParentKey: backend.DirectoryId
  readonly newParentId: backend.DirectoryId
}

/** A signal to delete assets. */
export interface AssetDeleteEvent extends AssetBaseEvent<AssetEventType.delete> {
  readonly ids: ReadonlySet<backend.AssetId>
}

/** A signal to delete assets forever. */
export interface AssetDeleteForeverEvent extends AssetBaseEvent<AssetEventType.deleteForever> {
  readonly ids: ReadonlySet<backend.AssetId>
}

/** A signal to restore assets from trash. */
export interface AssetRestoreEvent extends AssetBaseEvent<AssetEventType.restore> {
  readonly ids: ReadonlySet<backend.AssetId>
}

/** A signal to download assets. */
export interface AssetDownloadEvent extends AssetBaseEvent<AssetEventType.download> {
  readonly ids: ReadonlySet<backend.AssetId>
}

/** A signal to download the currently selected assets. */
export type AssetDownloadSelectedEvent = AssetBaseEvent<AssetEventType.downloadSelected>

/** A signal to remove the current user's permissions for an asset. */
export interface AssetRemoveSelfEvent extends AssetBaseEvent<AssetEventType.removeSelf> {
  readonly id: backend.AssetId
}

/** A signal to temporarily add labels to the selected assets. */
export interface AssetTemporarilyAddLabelsEvent
  extends AssetBaseEvent<AssetEventType.temporarilyAddLabels> {
  readonly ids: ReadonlySet<backend.AssetId>
  readonly labelNames: ReadonlySet<backend.LabelName>
}

/** A signal to temporarily remove labels from the selected assets. */
export interface AssetTemporarilyRemoveLabelsEvent
  extends AssetBaseEvent<AssetEventType.temporarilyRemoveLabels> {
  readonly ids: ReadonlySet<backend.AssetId>
  readonly labelNames: ReadonlySet<backend.LabelName>
}

/** A signal to add labels to the selected assets. */
export interface AssetAddLabelsEvent extends AssetBaseEvent<AssetEventType.addLabels> {
  readonly ids: ReadonlySet<backend.AssetId>
  readonly labelNames: ReadonlySet<backend.LabelName>
}

/** A signal to remove labels from the selected assets. */
export interface AssetRemoveLabelsEvent extends AssetBaseEvent<AssetEventType.removeLabels> {
  readonly ids: ReadonlySet<backend.AssetId>
  readonly labelNames: ReadonlySet<backend.LabelName>
}

/** A signal to remove a label from all assets. */
export interface AssetDeleteLabelEvent extends AssetBaseEvent<AssetEventType.deleteLabel> {
  readonly labelName: backend.LabelName
}

/** A signal to update the value of an item. */
export interface AssetSetItemEvent extends AssetBaseEvent<AssetEventType.setItem> {
  readonly id: backend.AssetId
  readonly valueOrUpdater: React.SetStateAction<backend.AnyAsset>
}

/**
 * A signal that a project was closed. In this case, the consumer should not fire a
 * "close project" request to the backend.
 */
export interface AssetProjectClosedEvent extends AssetBaseEvent<AssetEventType.projectClosed> {
  readonly id: backend.AssetId
}

/** Every possible type of asset event. */
export type AssetEvent = AssetEvents[keyof AssetEvents]
