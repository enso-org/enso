/** @file A wrapper around `toast.promise` that takes feature flags into account. */
import toast, * as toastModule from 'react-hot-toast'

import * as featureFlags from './featureFlags'

/** Messages passed to {@link toast.promise}. */
interface ToastPromiseMessages {
    loading: toastModule.Renderable
    success: toastModule.ValueOrFunction<toastModule.Renderable, unknown>
    // The types come from a third-party API and cannot be changed.
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    error: toastModule.ValueOrFunction<toastModule.Renderable, any>
}

/** A wrapper around {@link toast.promise} that takes feature flags into account. */
export async function toastPromise<T>(
    promise: Promise<T>,
    msgs: ToastPromiseMessages,
    opts?: toastModule.DefaultToastOptions
) {
    if (featureFlags.FEATURE_FLAGS.moreToasts) {
        return toast.promise(promise, msgs, opts)
    } else {
        try {
            return await promise
        } catch (e) {
            const errorMessage = typeof msgs.error === 'function' ? msgs.error(e) : msgs.error
            toast.error(errorMessage, opts)
            throw e
        }
    }
}
