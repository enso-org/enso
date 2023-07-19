/** @file Events related to changes in the asset list. */
import * as backend from '../backend'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        assetListEvent: AssetListEvent
    }
}

/** Possible changes to the file list. */
export enum AssetListEventType {
    createProject = 'create-project',
    uploadFiles = 'upload-files',
    delete = 'delete',
}

/** Properties common to all asset list events. */
interface AssetListBaseEvent<Type extends AssetListEventType> {
    type: Type
}

/** A signal to create a new project. */
interface AssetListCreateProjectEvent extends AssetListBaseEvent<AssetListEventType.createProject> {
    templateId: string | null
}

/** A signal to upload multiple files. */
interface AssetListUploadFilesEvent extends AssetListBaseEvent<AssetListEventType.uploadFiles> {
    files: FileList
}

/** A signal to upload multiple files. */
interface AssetListDeleteEvent extends AssetListBaseEvent<AssetListEventType.delete> {
    id: backend.AssetId
}

/** Every possible type of asset list event. */
export type AssetListEvent =
    | AssetListCreateProjectEvent
    | AssetListDeleteEvent
    | AssetListUploadFilesEvent
