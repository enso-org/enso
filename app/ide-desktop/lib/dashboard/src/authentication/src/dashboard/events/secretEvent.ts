/** @file Events related to changes in secret state. */
import * as backendModule from '../backend'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        secretEvent: SecretEvent
    }
}

// ===================
// === SecretEvent ===
// ===================

/** Possible types of secret state change. */
export enum SecretEventType {
    create = 'create',
    deleteMultiple = 'delete-multiple',
}

/** Properties common to all secret state change events. */
interface SecretBaseEvent<Type extends SecretEventType> {
    type: Type
}

/** A signal to create a secret. */
export interface SecretCreateEvent extends SecretBaseEvent<SecretEventType.create> {
    placeholderId: backendModule.SecretId
    value: string
}

/** A signal to delete multiple secrets. */
export interface SecretDeleteMultipleEvent extends SecretBaseEvent<SecretEventType.deleteMultiple> {
    secretIds: Set<backendModule.SecretId>
}

/** Every possible type of secret event. */
export type SecretEvent = SecretCreateEvent | SecretDeleteMultipleEvent
