/** @file Events related to changes in the file list. */

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        fileListEvent: FileListEvent
    }
}

// =====================
// === FileListEvent ===
// =====================

/** Possible changes to the file list. */
export enum FileListEventType {
    create = 'create',
    delete = 'delete',
}

/** Properties common to all file list change events. */
interface FileListBaseEvent<Type extends FileListEventType> {
    type: Type
}

/** A signal to create a new file. */
interface FileListCreateEvent extends FileListBaseEvent<FileListEventType.create> {}

/** A signal to delete a file. */
interface FileListDeleteEvent extends FileListBaseEvent<FileListEventType.delete> {
    fileId: string
}

/** Every possible type of file list event. */
export type FileListEvent = FileListCreateEvent | FileListDeleteEvent
