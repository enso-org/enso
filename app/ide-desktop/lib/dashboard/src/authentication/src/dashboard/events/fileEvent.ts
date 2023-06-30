/** @file Events related to changes in file state. */
import * as backendModule from '../backend'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        fileEvent: FileEvent
    }
}

// =================
// === FileEvent ===
// =================

/** Possible types of file state change. */
export enum FileEventType {
    create = 'create',
    deleteMultiple = 'delete-multiple',
}

/** Properties common to all file state change events. */
interface FileBaseEvent<Type extends FileEventType> {
    type: Type
}

/** A signal to create a file. */
export interface FileCreateEvent extends FileBaseEvent<FileEventType.create> {
    placeholderId: backendModule.FileId
    file: File
}

/** A signal to delete multiple files. */
export interface FileDeleteMultipleEvent extends FileBaseEvent<FileEventType.deleteMultiple> {
    fileIds: Set<backendModule.FileId>
}

/** Every possible type of file event. */
export type FileEvent = FileCreateEvent | FileDeleteMultipleEvent
