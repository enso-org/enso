import type Backend from '#/services/Backend'
import type { HttpsUrl, UploadFileRequestParams } from '#/services/Backend'
import { createGlobalState } from '@vueuse/core'
import { ConditionVariable } from 'enso-common/src/utilities/ConditionVariable'
import { reactive, watchEffect } from 'vue'
import { useBackends } from './backends'
import { useFeatureFlag } from './featureFlags'

/** The delay, in milliseconds, before query data for a file being uploaded is cleared. */
const CLEAR_PROGRESS_DELAY_MS = 5_000

export type UploadKind = 'requestedByUser' | 'hybridSync'

export interface OngoingUpload {
  kind?: UploadKind | undefined
  sentBytes: number
  totalBytes: number
  finished: boolean
  abortController: AbortController
}

export type UploadsToCloudStore = ReturnType<typeof createUploadsToCloudStore>

export function createUploadsToCloudStore(backend: Backend) {
  const uploads = reactive(new Map<string, OngoingUpload>())
  const chunkUploadPoolSize = useFeatureFlag('fileChunkUploadPoolSize')
  let chunksBeingUploaded = 0
  const chunkUploadCondVar = new ConditionVariable()

  watchEffect(() => console.debug(uploads))

  async function uploadChunk(url: HttpsUrl, file: File, index: number, abort: AbortSignal) {
    console.debug('CHUNK', index, 'waiting for pool')
    while (chunkUploadPoolSize.value > 0 && chunksBeingUploaded >= chunkUploadPoolSize.value) {
      await chunkUploadCondVar.wait()
      abort.throwIfAborted()
    }
    console.debug('CHUNK', index, 'uploading')
    chunksBeingUploaded += 1
    return backend.uploadFileChunk(url, file, index, abort).finally(() => {
      chunksBeingUploaded -= 1
      chunkUploadCondVar.notifyOne()
      console.debug('CHUNK', index, 'finished')
    })
  }

  async function uploadFile(file: File, params: UploadFileRequestParams, kind?: UploadKind) {
    console.debug('Start upload')
    const abortController = new AbortController()
    const { sourcePath, uploadId, presignedUrls } = await backend.uploadFileStart(
      params,
      file,
      abortController.signal,
    )

    console.debug('Upload started', presignedUrls)
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
        uploadChunk(url, file, i, abortController.signal).then((part) => {
          data.sentBytes += part.size
          return part
        }),
      ),
    )
    console.debug('Parts uploaded')
    const result = await backend.uploadFileEnd(
      {
        parentDirectoryId: params.parentDirectoryId,
        parts,
        sourcePath: sourcePath,
        uploadId: uploadId,
        assetId: params.fileId,
        fileName: params.fileName,
      },
      abortController.signal,
    )
    console.debug('Finished')
    data.finished = true
    setTimeout(() => {
      console.debug('cleared')
      uploads.delete(uploadId)
    }, CLEAR_PROGRESS_DELAY_MS)
    return result
  }

  return { uploads, uploadFile }
}

export const useUploadsToCloudStore = createGlobalState(() => {
  const { remoteBackend } = useBackends()
  return createUploadsToCloudStore(remoteBackend)
})
