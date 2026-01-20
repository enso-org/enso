import { withSetup } from '@/util/testing'
import { flushPromises } from '@vue/test-utils'
import {
  DirectoryId,
  HttpsUrl,
  S3FilePath,
  type S3MultipartPart,
  type UploadedAsset,
  type UploadFileEndRequestBody,
  type UploadFileRequestParams,
  type UploadLargeFileMetadata,
} from 'enso-common/src/services/Backend'
import { assert, expect, test, vi } from 'vitest'
import { setFeatureFlag } from '../featureFlags'
import { createUploadsStore } from '../upload'

const CHUNK_SIZE = 10

function fixture(files: { fileName: string; partsCount: number }[]) {
  const partsInProgress = new Map<[string, number], () => void>()

  const filesMap = new Map(
    files.map((f) => [
      f.fileName,
      {
        ...f,
        file: new File(Array(f.partsCount).fill(Array(CHUNK_SIZE).fill(0)), 'test-path'),
        params: {
          fileId: null,
          fileName: f.fileName,
          parentDirectoryId: DirectoryId('directory-testdir'),
        },
      },
    ]),
  )

  const backend = {
    uploadFileStart: vi.fn(
      (params: UploadFileRequestParams, file: File): Promise<UploadLargeFileMetadata> => {
        const f = filesMap.get(params.fileName)
        assert(f != null)
        expect(params).toEqual(f.params)
        expect(file).toBe(f.file)
        return Promise.resolve({
          presignedUrls: [...Array(f.partsCount).keys()].map((i) =>
            HttpsUrl(`url-${f.fileName}-${i}`),
          ),
          uploadId: `upload-${f.fileName}`,
          sourcePath: S3FilePath('s2path'),
        })
      },
    ),
    uploadFileChunk: vi.fn(
      (
        url: HttpsUrl,
        file: Blob,
        index: number,
      ): Promise<{ part: S3MultipartPart; size: number }> => {
        const fileName = url.split('-')[1]
        assert(fileName != null)
        const f = filesMap.get(fileName)
        assert(f != null)
        expect([...partsInProgress.keys()]).not.toContain([fileName, index])
        expect(file).toBe(f.file)
        return new Promise((resolve) => {
          partsInProgress.set([fileName, index], () =>
            resolve({ part: { eTag: '', partNumber: index + 1 }, size: CHUNK_SIZE }),
          )
        })
      },
    ),
    uploadFileEnd: vi.fn((params: UploadFileEndRequestBody): Promise<UploadedAsset> => {
      const f = filesMap.get(params.fileName)
      assert(f != null)
      expect(params).toEqual({
        parentDirectoryId: f.params.parentDirectoryId,
        parts: [...Array(f.partsCount).keys()].map((i) => ({ eTag: '', partNumber: i + 1 })),
        sourcePath: S3FilePath('s2path'),
        uploadId: `upload-${f.fileName}`,
        assetId: f.params.fileId,
        fileName: f.fileName,
      })
      return Promise.resolve({} as UploadedAsset)
    }),
  }

  const store = createUploadsStore(backend as any)

  return { partsInProgress, store, filesMap }
}

test('Pooling single multipart file upload', () =>
  withSetup(async () => {
    setFeatureFlag('fileChunkUploadPoolSize', 2)
    const { partsInProgress, store, filesMap } = fixture([{ fileName: 'file', partsCount: 5 }])
    const file = filesMap.get('file')!
    const uploadResult = store.uploadFile(file.file, file.params).catch(assert.fail)

    await flushPromises()
    expect(partsInProgress.size).toBe(2)
    expect(store.uploads.size).toBe(1)
    expect(store.uploads.values().next().value?.sentBytes).toBe(0)

    // Resolve single part
    const firstPart = partsInProgress.keys().next().value!
    partsInProgress.get(firstPart)?.()
    partsInProgress.delete(firstPart)
    await flushPromises()
    expect(partsInProgress.size).toBe(2)
    expect(store.uploads.values().next().value?.sentBytes).toBe(CHUNK_SIZE)

    // Resovle two parts at once
    partsInProgress.forEach((resolve) => resolve())
    partsInProgress.clear()
    await flushPromises()
    expect(partsInProgress.size).toBe(2)
    expect(store.uploads.values().next().value?.sentBytes).toBe(3 * CHUNK_SIZE)

    // Resolve last parts
    partsInProgress.forEach((resolve) => resolve())
    await uploadResult
    expect(store.uploads.values().next().value?.sentBytes).toBe(5 * CHUNK_SIZE)
  }))

test('Pooling multiple files upload', () =>
  withSetup(async () => {
    setFeatureFlag('fileChunkUploadPoolSize', 2)
    const { partsInProgress, store, filesMap } = fixture([
      { fileName: 'file1', partsCount: 1 },
      { fileName: 'file2', partsCount: 1 },
      { fileName: 'file3', partsCount: 1 },
    ])
    const results = Promise.all(
      [...filesMap.entries()].map(([, file]) => store.uploadFile(file.file, file.params)),
    ).catch(assert.fail)

    await flushPromises()
    expect(partsInProgress.size).toBe(2)
    expect(store.uploads.size).toBe(3)

    // Resolve single part
    const firstPart = partsInProgress.keys().next().value!
    partsInProgress.get(firstPart)?.()
    partsInProgress.delete(firstPart)
    await flushPromises()
    expect(partsInProgress.size).toBe(2)
    expect([...store.uploads.values()].map(({ sentBytes }) => sentBytes).sort()).toEqual([
      0,
      0,
      CHUNK_SIZE,
    ])

    // Resovle rest of the parts
    partsInProgress.forEach((resolve) => resolve())
    partsInProgress.clear()
    await flushPromises()
    expect(partsInProgress.size).toBe(0)
    expect([...store.uploads.values()].map(({ sentBytes }) => sentBytes).sort()).toEqual([
      CHUNK_SIZE,
      CHUNK_SIZE,
      CHUNK_SIZE,
    ])

    // Resolve last parts
    partsInProgress.forEach((resolve) => resolve())
    await results
  }))
