/** @file Helper function to upload multiple files,
 * with progress being reported by a continually updating toast notification. */
import * as toastify from 'react-toastify'

import * as backend from './dashboard/backend'
import * as remoteBackend from './dashboard/remoteBackend'

// ===========================
// === uploadMultipleFiles ===
// ===========================

/** Uploads multiple files to the backend, showing a continuously updated toast notification. */
export async function uploadMultipleFiles(
    backendService: remoteBackend.RemoteBackend,
    directoryId: backend.DirectoryId,
    files: File[]
) {
    const fileCount = files.length
    if (fileCount === 0) {
        toastify.toast.error('No files were dropped.')
        return []
    } else {
        let successfulUploadCount = 0
        let completedUploads = 0
        /** "file" or "files", whicheven is appropriate. */
        const filesWord = fileCount === 1 ? 'file' : 'files'
        const toastId = toastify.toast.loading(`Uploading ${fileCount} ${filesWord}.`)
        return await Promise.allSettled(
            files.map(file =>
                backendService
                    .uploadFile(
                        {
                            fileName: file.name,
                            parentDirectoryId: directoryId,
                        },
                        file
                    )
                    .then(() => {
                        successfulUploadCount += 1
                    })
                    .catch(() => {
                        toastify.toast.error(`Could not upload file '${file.name}'.`)
                    })
                    .finally(() => {
                        completedUploads += 1
                        if (completedUploads === fileCount) {
                            const progress =
                                successfulUploadCount === fileCount
                                    ? fileCount
                                    : `${successfulUploadCount}/${fileCount}`
                            toastify.toast.update(toastId, {
                                render: `${progress} ${filesWord} uploaded.`,
                                type: "success",
                                progress: 1.0,
                            })
                        } else {
                            toastify.toast.update(toastId, {
                                render: `${successfulUploadCount}/${fileCount} ${filesWord} uploaded.`,
                                progress: successfulUploadCount / fileCount,
                            })
                        }
                    })
            )
        )
    }
}
