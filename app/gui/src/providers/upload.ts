import { ConditionVariable } from '$/utils/ConditionVariable'
import { backendMutationOptions } from '@/composables/backend'
import * as vueQuery from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import type { Backend, HttpsUrl, UploadFileRequestParams } from 'enso-common/src/services/Backend'
import { reactive } from 'vue'
import { useBackends } from './backends'
import { useFeatureFlag } from './featureFlags'

/** The delay, in milliseconds, before query data for a file being uploaded is cleared. */
const CLEAR_PROGRESS_DELAY_MS = 5_000
const RETRIES = 3

export type UploadKind = 'requestedByUser' | 'hybridSync'

export interface OngoingUpload {
  kind?: UploadKind | undefined
  sentBytes: number
  totalBytes: number
  finished: boolean
  abortController: AbortController
}

export type UploadsToCloudStore = ReturnType<typeof createUploadsStore>

/** Constructor of UploadsToCloudStore. see {@link useUploadsToCloudStore}  docs. */
export function createUploadsStore(backend: Backend) {
  const uploads = reactive(new Map<string, OngoingUpload>())
  const chunkUploadPoolSize = useFeatureFlag('fileChunkUploadPoolSize')
  let chunksBeingUploaded = 0
  const chunkUploadCondVar = new ConditionVariable()

  const uploadFileStart = vueQuery.useMutation(backendMutationOptions('uploadFileStart', backend))
  const uploadFileChunk = vueQuery.useMutation(
    backendMutationOptions('uploadFileChunk', backend, { retry: RETRIES }),
  )
  const uploadFileEnd = vueQuery.useMutation(
    backendMutationOptions('uploadFileEnd', backend, { retry: RETRIES }),
  )

  async function uploadChunk(url: HttpsUrl, file: File, index: number, abort: AbortSignal) {
    while (chunkUploadPoolSize.value > 0 && chunksBeingUploaded >= chunkUploadPoolSize.value) {
      await chunkUploadCondVar.wait()
      abort.throwIfAborted()
    }
    chunksBeingUploaded += 1
    return uploadFileChunk.mutateAsync([url, file, index, abort]).finally(() => {
      chunksBeingUploaded -= 1
      chunkUploadCondVar.notifyOne()
    })
  }

  async function uploadFile(file: File, params: UploadFileRequestParams, kind?: UploadKind) {
    const abortController = new AbortController()
    const { sourcePath, uploadId, presignedUrls } = await uploadFileStart.mutateAsync([
      params,
      file,
      abortController.signal,
    ])

    const data: OngoingUpload = reactive({
      kind,
      sentBytes: 0,
      totalBytes: file.size,
      finished: false,
      abortController,
    })
    uploads.set(uploadId, data)

    const parts = await Promise.all(
      presignedUrls.map((url, i) =>
        uploadChunk(url, file, i, abortController.signal).then(({ part, size }) => {
          data.sentBytes += size
          return part
        }),
      ),
    )
    const result = await uploadFileEnd.mutateAsync([
      {
        parentDirectoryId: params.parentDirectoryId,
        parts,
        sourcePath: sourcePath,
        uploadId: uploadId,
        assetId: params.fileId,
        fileName: params.fileName,
      },
      abortController.signal,
    ])
    data.finished = true
    setTimeout(() => {
      uploads.delete(uploadId)
    }, CLEAR_PROGRESS_DELAY_MS)
    return result
  }

  return { uploads, uploadFile }
}

/**
 * Uploads to Cloud Store.
 *
 * This store handles and keeps track of multipart file upload to Remote Backend.
 * The number of chunks uploaded at once is throttled by 'fileChunkUploadPoolSize'
 * feature flag. `uploads` map contains all uploads with their progress, including
 * the uploads finished no longer than {@link CLEAR_PROGRESS_DELAY_MS} ago.
 */
export const useUploadsToCloudStore = createGlobalState(() => {
  const { remoteBackend } = useBackends()
  return createUploadsStore(remoteBackend)
})
