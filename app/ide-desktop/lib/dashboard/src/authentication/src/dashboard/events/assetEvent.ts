/** @file Events related to changes in asset state. */
import * as backendModule from '../backend'

import * as spinner from '../components/spinner'

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
    createDirectory = 'create-directory',
    uploadFiles = 'upload-files',
    createSecret = 'create-secret',
    openProject = 'open-project',
    cancelOpeningAllProjects = 'cancel-opening-all-projects',
    deleteMultiple = 'delete-multiple',
    downloadSelected = 'download-selected',
    removeSelf = 'remove-self',
}

/** Properties common to all asset state change events. */
interface AssetBaseEvent<Type extends AssetEventType> {
    type: Type
}

/** All possible events. */
interface AssetEvents {
    createProject: AssetCreateProjectEvent
    createDirectory: AssetCreateDirectoryEvent
    uploadFiles: AssetUploadFilesEvent
    createSecret: AssetCreateSecretEvent
    openProject: AssetOpenProjectEvent
    cancelOpeningAllProjects: AssetCancelOpeningAllProjectsEvent
    deleteMultiple: AssetDeleteMultipleEvent
    downloadSelected: AssetDownloadSelectedEvent
    removeSelf: AssetRemoveSelfEvent
}

/** A type to ensure that {@link AssetEvents} contains every {@link AssetLEventType}. */
// This is meant only as a sanity check, so it is allowed to break lint rules.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
type SanityCheck<
    T extends {
        [Type in keyof typeof AssetEventType]: AssetBaseEvent<(typeof AssetEventType)[Type]>
    } = AssetEvents
    // eslint-disable-next-line no-restricted-syntax
> = T

/** A signal to create a project. */
export interface AssetCreateProjectEvent extends AssetBaseEvent<AssetEventType.createProject> {
    placeholderId: backendModule.ProjectId
    templateId: string | null
    onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null
}

/** A signal to create a directory. */
export interface AssetCreateDirectoryEvent extends AssetBaseEvent<AssetEventType.createDirectory> {
    placeholderId: backendModule.DirectoryId
}

/** A signal to upload files. */
export interface AssetUploadFilesEvent extends AssetBaseEvent<AssetEventType.uploadFiles> {
    files: Map<backendModule.FileId | backendModule.ProjectId, File>
}

/** A signal to create a secret. */
export interface AssetCreateSecretEvent extends AssetBaseEvent<AssetEventType.createSecret> {
    placeholderId: backendModule.SecretId
    value: string
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

/** A signal to download the currently selected assets. */
export interface AssetDownloadSelectedEvent
    extends AssetBaseEvent<AssetEventType.downloadSelected> {}

/** A signal to remove the current user's permissions for an asset.. */
export interface AssetRemoveSelfEvent extends AssetBaseEvent<AssetEventType.removeSelf> {
    id: backendModule.AssetId
}

/** Every possible type of asset event. */
export type AssetEvent = AssetEvents[keyof AssetEvents]
