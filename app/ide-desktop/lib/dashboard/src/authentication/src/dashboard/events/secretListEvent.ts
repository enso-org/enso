/** @file Events related to changes in the secret list. */

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        secretListEvent: SecretListEvent
    }
}

// =======================
// === SecretListEvent ===
// =======================

/** Possible changes to the secret list. */
export enum SecretListEventType {
    create = 'create',
    delete = 'delete',
}

/** Properties common to all secret list change events. */
interface SecretListBaseEvent<Type extends SecretListEventType> {
    type: Type
}

/** A signal to create a new secret. */
interface SecretListCreateEvent extends SecretListBaseEvent<SecretListEventType.create> {
    name: string
    value: string
}

/** A signal to delete a secret. */
interface SecretListDeleteEvent extends SecretListBaseEvent<SecretListEventType.delete> {
    secretId: string
}

/** Every possible type of secret list event. */
export type SecretListEvent = SecretListCreateEvent | SecretListDeleteEvent
