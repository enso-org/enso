/** @file Events related to changes in asset state. */
import * as backendModule from '../backend'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        assetEvent: AssetEvent
    }
}

// ==================
// === AssetEvent ===
// ==================

/** Possible types of asset state change. */
export enum AssetEventType {
    createProject = 'create-project',
    uploadFiles = 'upload-files',
    openProject = 'open-project',
    cancelOpeningAllProjects = 'cancel-opening-all-projects',
    deleteMultiple = 'delete-multiple',
}

/** Properties common to all asset state change events. */
interface AssetBaseEvent<Type extends AssetEventType> {
    type: Type
}

/** A signal to create a project. */
export interface AssetCreateProjectEvent extends AssetBaseEvent<AssetEventType.createProject> {
    placeholderId: backendModule.AssetId
    templateId: string | null
}

/** A signal to create multiple files. */
export interface AssetUploadFilesEvent extends AssetBaseEvent<AssetEventType.uploadFiles> {
    files: Map<backendModule.FileId, File>
}

/** A signal to open the specified project. */
export interface AssetOpenProjectEvent extends AssetBaseEvent<AssetEventType.openProject> {
    id: backendModule.AssetId
}

/** A signal to cancel automatically opening any project that is currently opening. */
export interface AssetCancelOpeningAllProjectsEvent
    extends AssetBaseEvent<AssetEventType.cancelOpeningAllProjects> {}

/** A signal to delete multiple assets. */
export interface AssetDeleteMultipleEvent extends AssetBaseEvent<AssetEventType.deleteMultiple> {
    ids: Set<backendModule.AssetId>
}

/** Every possible type of asset event. */
export type AssetEvent =
    | AssetCancelOpeningAllProjectsEvent
    | AssetCreateProjectEvent
    | AssetDeleteMultipleEvent
    | AssetOpenProjectEvent
    | AssetUploadFilesEvent
