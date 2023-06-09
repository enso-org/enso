/** @file Helper function to upload multiple files,
 * with progress being reported by a continually updating toast notification. */
import toast from 'react-hot-toast'

// ============================
// === toastPromiseMultiple ===
// ============================

/** Functions to generate messages depending on the current status of the {@link Promise}s. */
export interface ToastPromiseMultipleMessages<T> {
    begin: (expectedCount: number) => string
    inProgress: (successCount: number, expectedCount: number) => string
    end: (successCount: number, expectedCount: number) => string
    error: (item: T) => string
}

/** Displays progress  multiple files to the backend, showing a continuously updated toast notification. */
export async function toastPromiseMultiple<T>(
    items: T[],
    map: (item: T) => Promise<void>,
    messages: ToastPromiseMultipleMessages<T>
) {
    const expectedCount = items.length
    if (expectedCount === 0) {
        toast.error('No files were dropped.')
        return []
    } else {
        let successCount = 0
        /** The total count of completed {@link Promise}s, no matter whether they are
         * fulfilled or rejected. */
        let finishedCount = 0
        const toastId = toast.loading(messages.begin(expectedCount))
        return await Promise.allSettled(
            items.map(async item => {
                try {
                    await map(item)
                    successCount += 1
                } catch {
                    toast.error(messages.error(item))
                } finally {
                    finishedCount += 1
                    if (finishedCount === expectedCount) {
                        toast.success(messages.end(successCount, expectedCount), {
                            id: toastId,
                        })
                    } else {
                        toast.loading(messages.inProgress(successCount, expectedCount), {
                            id: toastId,
                        })
                    }
                }
            })
        )
    }
}
