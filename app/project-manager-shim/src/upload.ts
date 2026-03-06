/**
 * @file Standalone file upload functionality based on app/gui/src/providers/upload.ts but without the Vue dependencies.
 * TODO (#14361) investigate if the two implementations can be merged.
 */

import type {
  Backend,
  S3MultipartPart,
  UploadedAsset,
  UploadFileEndRequestBody,
  UploadFileRequestParams,
} from 'enso-common/src/services/Backend'
import { S3_CHUNK_SIZE_BYTES } from 'enso-common/src/services/Backend'

/** Upload progress event types. */
export type UploadProgressEvent = 'begin' | 'chunk' | 'end'

/** Upload progress information. */
export interface UploadProgress {
  readonly event: UploadProgressEvent
  readonly sentBytes: number
  readonly totalBytes: number
}

/** Options for file upload. */
export interface UploadFileOptions {
  /** Number of retries for chunk uploads. Defaults to 3. */
  readonly chunkRetries?: number
  /** Number of retries for finalization. Defaults to 3. */
  readonly endRetries?: number
  /** Progress callback. */
  readonly onProgress?: (progress: UploadProgress) => void
  /** Called before upload starts. */
  readonly onBegin?: (progress: UploadProgress) => void
  /** Called after each successful chunk upload. */
  readonly onChunkSuccess?: (progress: UploadProgress) => void
  /** Called after successful upload completion. */
  readonly onSuccess?: (progress: UploadProgress) => void
  /** Called on error. */
  readonly onError?: (error: unknown) => void
  /** Called after completion (success or error). */
  readonly onSettled?: (progress: UploadProgress | null, error: unknown) => void
  /** Maximum number of parallel chunk uploads. Defaults to 8. */
  readonly maxParallelChunks?: number
}

/**
 * Retry a function with exponential backoff.
 */
async function retryWithBackoff<T>(
  fn: () => Promise<T>,
  retries: number,
  delayMs = 1000,
): Promise<T> {
  let lastError: unknown
  for (let attempt = 0; attempt <= retries; attempt++) {
    try {
      return await fn()
    } catch (error) {
      lastError = error
      if (attempt < retries) {
        await new Promise((resolve) => setTimeout(resolve, delayMs * Math.pow(2, attempt)))
      }
    }
  }
  throw lastError
}

/**
 * Upload a single chunk with retry logic.
 */
async function uploadChunk(
  url: string,
  file: File,
  chunkIndex: number,
  retries: number,
): Promise<S3MultipartPart> {
  return retryWithBackoff(async () => {
    const start = chunkIndex * S3_CHUNK_SIZE_BYTES
    const end = Math.min(start + S3_CHUNK_SIZE_BYTES, file.size)
    const chunk = file.slice(start, end)

    const response = await fetch(url, {
      method: 'PUT',
      body: chunk,
      headers: {
        'Content-Type': 'application/octet-stream',
      },
    })

    if (!response.ok) {
      throw new Error(`Chunk upload failed: ${response.statusText}`)
    }

    const eTag = response.headers.get('ETag')
    if (!eTag) {
      throw new Error('Missing ETag in response')
    }

    return {
      eTag: eTag.replace(/"/g, ''),
      partNumber: chunkIndex + 1,
    }
  }, retries)
}

/**
 * Upload chunks with controlled parallelism.
 */
async function uploadChunksWithParallelism(
  presignedUrls: readonly string[],
  file: File,
  chunkRetries: number,
  maxParallel: number,
  onChunkComplete: (completedCount: number) => void,
): Promise<S3MultipartPart[]> {
  const parts: S3MultipartPart[] = new Array(presignedUrls.length)
  let completedCount = 0
  let nextIndex = 0

  const uploadNext = async (): Promise<void> => {
    while (nextIndex < presignedUrls.length) {
      const currentIndex = nextIndex++
      const url = presignedUrls[currentIndex]!
      const part = await uploadChunk(url, file, currentIndex, chunkRetries)
      parts[currentIndex] = part
      completedCount++
      onChunkComplete(completedCount)
    }
  }

  const workers = Array.from({ length: Math.min(maxParallel, presignedUrls.length) }, () =>
    uploadNext(),
  )
  await Promise.all(workers)

  return parts
}

/**
 * Upload a file to the backend using multipart upload.
 * This is a standalone implementation that doesn't depend on React hooks or toast notifications.
 * @param backend - The backend instance to use for upload operations
 * @param body - Upload request parameters
 * @param file - The file to upload
 * @param options - Upload options
 * @returns The uploaded asset information
 */
export async function uploadFile(
  backend: Backend,
  body: UploadFileRequestParams,
  file: File,
  options: UploadFileOptions = {},
): Promise<UploadedAsset> {
  const {
    chunkRetries = 3,
    endRetries = 3,
    maxParallelChunks = 8,
    onBegin,
    onChunkSuccess,
    onSuccess,
    onError,
    onSettled,
    onProgress,
  } = options

  const fileSizeBytes = file.size
  const beginProgress: UploadProgress = {
    event: 'begin',
    sentBytes: 0,
    totalBytes: fileSizeBytes,
  }

  try {
    // Notify upload start
    onBegin?.(beginProgress)
    onProgress?.(beginProgress)

    // Start multipart upload
    const { sourcePath, uploadId, presignedUrls } = await backend.uploadFileStart(body, file)

    // Upload chunks with controlled parallelism
    const parts = await uploadChunksWithParallelism(
      presignedUrls,
      file,
      chunkRetries,
      maxParallelChunks,
      (completedCount) => {
        const newSentBytes = Math.min(completedCount * S3_CHUNK_SIZE_BYTES, fileSizeBytes)
        const chunkProgress: UploadProgress = {
          event: 'chunk',
          sentBytes: newSentBytes,
          totalBytes: fileSizeBytes,
        }
        onChunkSuccess?.(chunkProgress)
        onProgress?.(chunkProgress)
      },
    )

    // Finalize upload with retry
    const result = await retryWithBackoff(async () => {
      const endParams: UploadFileEndRequestBody = {
        parentDirectoryId: body.parentDirectoryId,
        parts,
        sourcePath,
        uploadId,
        assetId: body.fileId,
        fileName: body.fileName,
        ...(body.overwrite && { overwrite: body.overwrite }),
      }
      return await backend.uploadFileEnd(endParams)
    }, endRetries)

    // Notify completion
    const endProgress: UploadProgress = {
      event: 'end',
      sentBytes: fileSizeBytes,
      totalBytes: fileSizeBytes,
    }
    onSuccess?.(endProgress)
    onProgress?.(endProgress)
    onSettled?.(endProgress, null)

    return result
  } catch (error) {
    onError?.(error)
    onSettled?.(null, error)
    throw error
  }
}
