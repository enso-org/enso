/** @file Types and functions for representing optimistic state.
 * Optimistic UI is when UI updates are applied immediately with the expected result, instead of
 * waiting for a server response, based on the assumption that virtually all requests succeed.
 * In our case it is MANDATORY because immediate user feedback is important. */

// These enum members may shadow variables, because the enum members are not being used in the
// definitions of other enum members - e.g. `deleting = present`.
/* eslint-disable @typescript-eslint/no-shadow */
/** The state of an item being synced to the backend. */
export enum OptimisticState {
    /** The item is present. */
    present = 'present',
    /** The item will be inserted, but the backend request has not yet finished. */
    inserting = 'inserting',
    /** The item will be deleted, but the backend request has not yet finished. */
    deleting = 'deleting',
}
/* eslint-enable @typescript-eslint/no-shadow */

/** Properties common to all {@link Optimistic} values. */
interface OptimisticBase<T, State extends OptimisticState> {
    key: string
    value: T
    state: State
}

/** An {@link Optimistic} value which is present. */
export interface OptimisticPresent<T> extends OptimisticBase<T, OptimisticState.present> {}

/** An {@link Optimistic} value which will be inserted, but is waiting for the backend request to
 * finish. */
export interface OptimisticInserting<T> extends OptimisticBase<T, OptimisticState.inserting> {
    newValuePromise: Promise<T>
}

/** An {@link Optimistic} value which will be deleted, but is waiting for the backend request to
 * finish. */
export interface OptimisticDeleting<T> extends OptimisticBase<T, OptimisticState.deleting> {}

/** A value and the state of state of it being synced to the backend. */
export type Optimistic<T> = OptimisticDeleting<T> | OptimisticInserting<T> | OptimisticPresent<T>

/** Gets the key of any {@link Optimistic}. This is useful for passing to React as it is not an
 * inline function, so it does not get re-created with a different reference on every re-render. */
export function getKey<T>(optimistic: Optimistic<T>) {
    const { key } = optimistic
    return key
}

/** A helper function to create an {@link OptimisticPresent}. */
export function present<T>(key: string, value: T): OptimisticPresent<T> {
    return {
        key,
        value,
        state: OptimisticState.present,
    }
}

/** A helper function to create an {@link OptimisticPresent} from an existing
 * {@link Optimistic}. */
export function load<T>(optimistic: Optimistic<T>): OptimisticPresent<T> {
    const { key, value } = optimistic
    return present(key, value)
}

/** A helper function to create an {@link OptimisticInserting}. */
export function inserting<T>(
    key: string,
    value: T,
    newValuePromise: Promise<T>
): OptimisticInserting<T> {
    return {
        key,
        value,
        state: OptimisticState.inserting,
        newValuePromise,
    }
}

/** A helper function to create an {@link OptimisticInserting} from an existing
 * {@link Optimistic}. */
export function insert<T>(
    optimistic: Optimistic<T>,
    newValuePromise: Promise<T>
): OptimisticInserting<T> {
    const { key, value } = optimistic
    return inserting(key, value, newValuePromise)
}

/** A helper function to create an {@link OptimisticDeleting}. */
export function deleting<T>(key: string, value: T): OptimisticDeleting<T> {
    return {
        key,
        value,
        state: OptimisticState.deleting,
    }
}

/** A helper function to create an {@link OptimisticDeleting} from an existing
 * {@link Optimistic}. */
function deleteFn<T>(optimistic: Optimistic<T>): OptimisticDeleting<T> {
    const { key, value } = optimistic
    return deleting(key, value)
}

// The renaming is required, because `delete` is not allowed as an identifier.
// eslint-disable-next-line no-restricted-syntax
export { deleteFn as delete }
