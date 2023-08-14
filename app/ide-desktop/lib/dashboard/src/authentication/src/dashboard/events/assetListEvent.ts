/** @file Events related to changes in the asset list. */
import * as backend from '../backend'

import * as spinner from '../components/spinner'

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
    newFolder = 'new-folder',
    newProject = 'new-project',
    uploadFiles = 'upload-files',
    newSecret = 'new-secret',
    delete = 'delete',
}

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
interface AssetListNewFolderEvent extends AssetListBaseEvent<AssetListEventType.newFolder> {
    parentKey: backend.DirectoryId | null
    parentId: backend.DirectoryId | null
}

/** A signal to create a new project. */
interface AssetListNewProjectEvent extends AssetListBaseEvent<AssetListEventType.newProject> {
    parentKey: backend.DirectoryId | null
    parentId: backend.DirectoryId | null
    templateId: string | null
    onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null
}

/** A signal to upload files. */
interface AssetListUploadFilesEvent extends AssetListBaseEvent<AssetListEventType.uploadFiles> {
    parentKey: backend.DirectoryId | null
    parentId: backend.DirectoryId | null
    files: File[]
}

/** A signal to create a new secret. */
interface AssetListNewSecretEvent extends AssetListBaseEvent<AssetListEventType.newSecret> {
    parentKey: backend.DirectoryId | null
    parentId: backend.DirectoryId | null
    name: string
    value: string
}

/** A signal that a file has been deleted. This must not be called before the request is
 * finished. */
interface AssetListDeleteEvent extends AssetListBaseEvent<AssetListEventType.delete> {
    id: backend.AssetId
}

/** Every possible type of asset list event. */
export type AssetListEvent = AssetListEvents[keyof AssetListEvents]
