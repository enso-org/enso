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
    createDirectory = 'create-directory',
    createProject = 'create-project',
    uploadFiles = 'upload-files',
    createSecret = 'create-secret',
    delete = 'delete',
}

/** Properties common to all asset list events. */
interface AssetListBaseEvent<Type extends AssetListEventType> {
    type: Type
}

/** All possible events. */
interface AssetListEvents {
    createDirectory: AssetListCreateDirectoryEvent
    createProject: AssetListCreateProjectEvent
    uploadFiles: AssetListUploadFilesEvent
    createSecret: AssetListCreateSecretEvent
    delete: AssetListDeleteEvent
}

/** A type to ensure that {@link AssetListEvents} contains every {@link AssetListEventType}. */
// This is meant only as a sanity check, so it is allowed to break lint rules.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
type SanityCheck<
    T extends {
        [Type in keyof typeof AssetListEventType]: AssetListBaseEvent<
            (typeof AssetListEventType)[Type]
        >
    } = AssetListEvents
    // eslint-disable-next-line no-restricted-syntax
> = T

/** A signal to create a new directory. */
interface AssetListCreateDirectoryEvent
    extends AssetListBaseEvent<AssetListEventType.createDirectory> {
    parentId: backend.DirectoryId | null
}

/** A signal to create a new project. */
interface AssetListCreateProjectEvent extends AssetListBaseEvent<AssetListEventType.createProject> {
    parentId: backend.DirectoryId | null
    templateId: string | null
}

/** A signal to upload files. */
interface AssetListUploadFilesEvent extends AssetListBaseEvent<AssetListEventType.uploadFiles> {
    parentId: backend.DirectoryId | null
    files: FileList
}

/** A signal to create a new secret. */
interface AssetListCreateSecretEvent extends AssetListBaseEvent<AssetListEventType.createSecret> {
    parentId: backend.DirectoryId | null
    name: string
    value: string
}

/** A signal to delete a file. */
interface AssetListDeleteEvent extends AssetListBaseEvent<AssetListEventType.delete> {
    id: backend.AssetId
}

/** Every possible type of asset list event. */
export type AssetListEvent = AssetListEvents[keyof AssetListEvents]
