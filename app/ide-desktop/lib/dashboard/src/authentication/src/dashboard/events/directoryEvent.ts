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
    deleteMultiple = 'delete-multiple',
}

/** Properties common to all directory state change events. */
interface DirectoryBaseEvent<Type extends DirectoryEventType> {
    type: Type
}

/** A signal to delete multiple directories. */
export interface DirectoryDeleteMultipleEvent
    extends DirectoryBaseEvent<DirectoryEventType.deleteMultiple> {
    directoryIds: Set<backendModule.DirectoryId>
}

/** Every possible type of directory event. */
// This renamed type is intentional. Semantically, it is a union type with one member.
// eslint-disable-next-line no-restricted-syntax
export type DirectoryEvent = DirectoryDeleteMultipleEvent
