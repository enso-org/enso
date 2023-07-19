/** @file Events related to changes in directory state. */
import * as backendModule from '../backend'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        directoryEvent: DirectoryEvent
    }
}

// ======================
// === DirectoryEvent ===
// ======================

/** Possible types of directory state change. */
export enum DirectoryEventType {
    create = 'create',
    deleteMultiple = 'delete-multiple',
}

/** Properties common to all directory state change events. */
interface DirectoryBaseEvent<Type extends DirectoryEventType> {
    type: Type
}

/** A signal to create a directory. */
export interface DirectoryCreateEvent extends DirectoryBaseEvent<DirectoryEventType.create> {
    placeholderId: backendModule.DirectoryId
}

/** A signal to delete multiple directories. */
export interface DirectoryDeleteMultipleEvent
    extends DirectoryBaseEvent<DirectoryEventType.deleteMultiple> {
    directoryIds: Set<backendModule.DirectoryId>
}

/** Every possible type of directory event. */
export type DirectoryEvent = DirectoryCreateEvent | DirectoryDeleteMultipleEvent
