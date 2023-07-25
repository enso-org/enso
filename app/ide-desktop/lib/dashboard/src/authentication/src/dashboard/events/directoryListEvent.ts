/** @file Events related to changes in the directory list. */

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        directoryListEvent: DirectoryListEvent
    }
}

// ==========================
// === DirectoryListEvent ===
// ==========================

/** Possible changes to the directory list. */
export enum DirectoryListEventType {
    create = 'create',
    delete = 'delete',
}

/** Properties common to all directory list change events. */
interface DirectoryListBaseEvent<Type extends DirectoryListEventType> {
    type: Type
}

/** A signal to create a new directory. */
interface DirectoryListCreateEvent extends DirectoryListBaseEvent<DirectoryListEventType.create> {}

/** A signal to delete a directory. */
interface DirectoryListDeleteEvent extends DirectoryListBaseEvent<DirectoryListEventType.delete> {
    directoryId: string
}

/** Every possible type of directory list event. */
export type DirectoryListEvent = DirectoryListCreateEvent | DirectoryListDeleteEvent
