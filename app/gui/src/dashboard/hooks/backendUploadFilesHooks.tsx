/** @file Hooks for uploading files. */

import {
  backendMutationOptions,
  useBackendQuery,
  useEnsureListDirectory,
} from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useToastAndLog, useToastAndLogWithId } from '#/hooks/toastAndLogHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import DuplicateAssetsModal from '#/modals/DuplicateAssetsModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import {
  useSetSelectedAssets,
  useToggleDirectoryExpansion,
  type SelectedAssetInfo,
} from '#/providers/DriveProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import LocalBackend from '#/services/LocalBackend'
import { tryCreateOwnerPermission } from '#/utilities/permissions'
import { usePreventNavigation } from '#/utilities/preventNavigation'
import { useMutation, type UseMutationResult } from '@tanstack/react-query'
import {
  assetIsFile,
  assetIsProject,
  AssetType,
  createPlaceholderFileAsset,
  createPlaceholderProjectAsset,
  escapeSpecialCharacters,
  extractProjectExtension,
  fileIsNotProject,
  fileIsProject,
  S3_CHUNK_SIZE_BYTES,
  stripProjectExtension,
  type AnyAsset,
  type AssetId,
  type default as Backend,
  type DirectoryId,
  type FileId,
  type ProjectId,
  type S3MultipartPart,
  type UploadedLargeAsset,
  type UploadFileRequestParams,
} from 'enso-common/src/services/Backend'
import type { MergeValuesOfObjectUnion } from 'enso-common/src/utilities/data/object'
import { useId, useState } from 'react'
import { toast } from 'react-toastify'

/** The number of bytes in 1 megabyte. */
const MB_BYTES = 1_000_000
const S3_CHUNK_SIZE_MB = Math.round(S3_CHUNK_SIZE_BYTES / MB_BYTES)
/** The maximum number of file chunks to upload at the same time. */
const FILE_UPLOAD_CONCURRENCY = 5

/** A function to upload files. */
export function useUploadFiles(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const toastAndLog = useToastAndLog()
  const toggleDirectoryExpansion = useToggleDirectoryExpansion()
  const { setModal } = useSetModal()
  const { user } = useFullUserSession()
  const { data: users } = useBackendQuery(backend, 'listUsers', [])
  const uploadFileMutation = useUploadFileWithToastMutation(backend)
  const setSelectedAssets = useSetSelectedAssets()

  return useEventCallback(
    async (
      filesToUpload: readonly File[],
      parentId: DirectoryId,
      parentPath: string | null | undefined,
    ) => {
      const localBackend = backend instanceof LocalBackend ? backend : null
      const reversedFiles = Array.from(filesToUpload).reverse()
      const siblings = await ensureListDirectory(parentId)
      const siblingFiles = siblings.filter(assetIsFile)
      const siblingProjects = siblings.filter(assetIsProject)
      const siblingFileTitles = new Set(siblingFiles.map((asset) => asset.title))
      const siblingProjectTitles = new Set(siblingProjects.map((asset) => asset.title))
      const ownerPermission = tryCreateOwnerPermission(
        parentPath ?? '',
        category,
        user,
        users ?? [],
        user.groups ?? [],
      )
      const files = reversedFiles.filter(fileIsNotProject).map((file) => {
        const asset = createPlaceholderFileAsset(
          escapeSpecialCharacters(file.name),
          parentId,
          ownerPermission,
        )
        return { asset, file }
      })
      const projects = reversedFiles.filter(fileIsProject).map((file) => {
        const basename = escapeSpecialCharacters(stripProjectExtension(file.name))
        const asset = createPlaceholderProjectAsset(
          basename,
          parentId,
          ownerPermission,
          user,
          localBackend?.joinPath(parentId, basename) ?? null,
        )
        return { asset, file }
      })
      const duplicateFiles = files.filter((file) => siblingFileTitles.has(file.asset.title))
      const duplicateProjects = projects.filter((project) =>
        siblingProjectTitles.has(stripProjectExtension(project.asset.title)),
      )
      const fileMap = new Map<AssetId, File>([
        ...files.map(({ asset, file }) => [asset.id, file] as const),
        ...projects.map(({ asset, file }) => [asset.id, file] as const),
      ])
      const uploadedFileInfos: SelectedAssetInfo[] = []
      const addToSelection = (info: SelectedAssetInfo) => {
        uploadedFileInfos.push(info)
        setSelectedAssets(uploadedFileInfos)
      }

      const doUploadFile = async (asset: AnyAsset, method: 'new' | 'update') => {
        const file = fileMap.get(asset.id)

        if (file != null) {
          const fileId = method === 'new' ? null : asset.id

          // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
          switch (true) {
            case assetIsProject(asset): {
              const { extension } = extractProjectExtension(file.name)
              const title = escapeSpecialCharacters(stripProjectExtension(asset.title))

              await uploadFileMutation
                .mutateAsync([
                  {
                    fileId,
                    fileName: `${title}.${extension}`,
                    parentDirectoryId: asset.parentId,
                  },
                  file,
                ])
                .then(({ id }) => {
                  addToSelection({
                    type: AssetType.project,
                    // This is SAFE, because it is guarded behind `assetIsProject`.
                    // eslint-disable-next-line no-restricted-syntax
                    id: id as ProjectId,
                    parentId: asset.parentId,
                    title,
                  })
                })
                .catch((error) => {
                  toastAndLog('uploadProjectError', error)
                })

              break
            }
            case assetIsFile(asset): {
              const title = escapeSpecialCharacters(asset.title)
              await uploadFileMutation
                .mutateAsync([{ fileId, fileName: title, parentDirectoryId: asset.parentId }, file])
                .then(({ id }) => {
                  addToSelection({
                    type: AssetType.file,
                    // This is SAFE, because it is guarded behind `assetIsFile`.
                    // eslint-disable-next-line no-restricted-syntax
                    id: id as FileId,
                    parentId: asset.parentId,
                    title,
                  })
                })

              break
            }
            default:
              break
          }
        }
      }

      if (duplicateFiles.length === 0 && duplicateProjects.length === 0) {
        toggleDirectoryExpansion(parentId, true)
        const assets = [...files, ...projects].map(({ asset }) => asset)
        void Promise.all(assets.map((asset) => doUploadFile(asset, 'new')))
      } else {
        const siblingFilesByName = new Map(siblingFiles.map((file) => [file.title, file]))
        const siblingProjectsByName = new Map(
          siblingProjects.map((project) => [project.title, project]),
        )
        const conflictingFiles = duplicateFiles.map((file) => ({
          // This is SAFE, as `duplicateFiles` only contains files that have siblings
          // with the same name.
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          current: siblingFilesByName.get(file.asset.title)!,
          new: createPlaceholderFileAsset(file.asset.title, parentId, ownerPermission),
          file: file.file,
        }))
        const conflictingProjects = duplicateProjects.map((project) => {
          const basename = stripProjectExtension(project.asset.title)
          return {
            // This is SAFE, as `duplicateProjects` only contains projects that have
            // siblings with the same name.
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            current: siblingProjectsByName.get(basename)!,
            new: createPlaceholderProjectAsset(
              basename,
              parentId,
              ownerPermission,
              user,
              localBackend?.joinPath(parentId, basename) ?? null,
            ),
            file: project.file,
          }
        })
        setModal(
          <DuplicateAssetsModal
            parentKey={parentId}
            parentId={parentId}
            conflictingFiles={conflictingFiles}
            conflictingProjects={conflictingProjects}
            siblingFileNames={siblingFilesByName.keys()}
            siblingProjectNames={siblingProjectsByName.keys()}
            nonConflictingFileCount={files.length - conflictingFiles.length}
            nonConflictingProjectCount={projects.length - conflictingProjects.length}
            doUpdateConflicting={async (resolvedConflicts) => {
              toggleDirectoryExpansion(parentId, true)

              await Promise.allSettled(
                resolvedConflicts.map((conflict) => {
                  const isUpdating = conflict.current.title === conflict.new.title
                  const asset = isUpdating ? conflict.current : conflict.new
                  fileMap.set(asset.id, conflict.file)
                  return doUploadFile(asset, isUpdating ? 'update' : 'new')
                }),
              )
            }}
            doUploadNonConflicting={async () => {
              toggleDirectoryExpansion(parentId, true)

              const newFiles = files
                .filter((file) => !siblingFileTitles.has(file.asset.title))
                .map((file) => {
                  const asset = createPlaceholderFileAsset(
                    file.asset.title,
                    parentId,
                    ownerPermission,
                  )
                  fileMap.set(asset.id, file.file)
                  return asset
                })

              const newProjects = projects
                .filter(
                  (project) =>
                    !siblingProjectTitles.has(stripProjectExtension(project.asset.title)),
                )
                .map((project) => {
                  const basename = stripProjectExtension(project.asset.title)
                  const asset = createPlaceholderProjectAsset(
                    basename,
                    parentId,
                    ownerPermission,
                    user,
                    localBackend?.joinPath(parentId, basename) ?? null,
                  )
                  fileMap.set(asset.id, project.file)
                  return asset
                })

              const assets = [...newFiles, ...newProjects]

              await Promise.allSettled(assets.map((asset) => doUploadFile(asset, 'new')))
            }}
          />,
        )
      }
    },
  )
}

/** Upload progress for {@link useUploadFileMutation}. */
export interface UploadFileMutationProgress {
  /**
   * Whether this is the first progress update.
   * Useful to determine whether to create a new toast or to update an existing toast.
   */
  readonly event: 'begin' | 'chunk' | 'end'
  readonly sentMb: number
  readonly totalMb: number
}

/** Options for {@link useUploadFileMutation}. */
export interface UploadFileMutationOptions {
  /**
   * Defaults to 3.
   * Controls the default value of {@link UploadFileMutationOptions['chunkRetries']}
   * and {@link UploadFileMutationOptions['endRetries']}.
   */
  readonly retries?: number
  /** Defaults to {@link UploadFileMutationOptions['retries']}. */
  readonly chunkRetries?: number
  /** Defaults to {@link UploadFileMutationOptions['retries']}. */
  readonly endRetries?: number
  /** Called for all progress updates (`onBegin`, `onChunkSuccess` and `onSuccess`). */
  readonly onProgress?: (progress: UploadFileMutationProgress) => void
  /** Called before any mutations are sent. */
  readonly onBegin?: (progress: UploadFileMutationProgress) => void
  /** Called after each successful chunk upload mutation. */
  readonly onChunkSuccess?: (progress: UploadFileMutationProgress) => void
  /** Called after the entire mutation succeeds. */
  readonly onSuccess?: (progress: UploadFileMutationProgress) => void
  /** Called after any mutations fail. */
  readonly onError?: (error: unknown) => void
  /** Called after `onSuccess` or `onError`, depending on whether the mutation succeeded. */
  readonly onSettled?: (progress: UploadFileMutationProgress | null, error: unknown) => void
}

/** The result of a {@link useUploadFileMutation}. */
export type UploadFileMutationResult = UseMutationResult<
  UploadedLargeAsset,
  Error,
  [body: UploadFileRequestParams, file: File],
  unknown
> & { readonly sentMb: number; readonly totalMb: number }

/**
 * Call "upload file" mutations for a file.
 * Always uses multipart upload for Cloud backend.
 * Shows toasts to update progress.
 */
export function useUploadFileWithToastMutation(
  backend: Backend,
  options: UploadFileMutationOptions = {},
): UploadFileMutationResult {
  const toastId = useId()
  const { getText } = useText()
  const toastAndLog = useToastAndLogWithId()
  const { onBegin, onChunkSuccess, onSuccess, onError } = options

  const mutation = useUploadFileMutation(backend, {
    ...options,
    onBegin: (progress) => {
      onBegin?.(progress)
      const { sentMb, totalMb } = progress
      toast.loading(getText('uploadLargeFileStatus', sentMb, totalMb), {
        toastId,
        position: 'bottom-right',
      })
    },
    onChunkSuccess: (progress) => {
      onChunkSuccess?.(progress)
      const { sentMb, totalMb } = progress
      const text = getText('uploadLargeFileStatus', sentMb, totalMb)
      toast.update(toastId, { render: text })
    },
    onSuccess: (progress) => {
      onSuccess?.(progress)
      toast.update(toastId, {
        type: 'success',
        render: getText('uploadLargeFileSuccess'),
        isLoading: false,
        autoClose: null,
      })
    },
    onError: (error) => {
      onError?.(error)
      toastAndLog(toastId, 'uploadLargeFileError', error)
    },
  })

  usePreventNavigation({ message: getText('anUploadIsInProgress'), isEnabled: mutation.isPending })

  return mutation
}

/**
 * Call "upload file" mutations for a file.
 * Always uses multipart upload for Cloud backend.
 */
export function useUploadFileMutation(
  backend: Backend,
  options: UploadFileMutationOptions = {},
): UploadFileMutationResult {
  const toastAndLog = useToastAndLog()
  const {
    retries = 3,
    chunkRetries = retries,
    endRetries = retries,
    onError = (error) => {
      toastAndLog('uploadLargeFileError', error)
    },
  } = options
  const uploadFileStartMutation = useMutation(backendMutationOptions(backend, 'uploadFileStart'))
  const uploadFileChunkMutation = useMutation(
    backendMutationOptions(backend, 'uploadFileChunk', { retry: chunkRetries }),
  )
  const uploadFileEndMutation = useMutation(
    backendMutationOptions(backend, 'uploadFileEnd', { retry: endRetries }),
  )
  const [variables, setVariables] = useState<[params: UploadFileRequestParams, file: File]>()
  const [sentMb, setSentMb] = useState(0)
  const [totalMb, setTotalMb] = useState(0)
  const mutateAsync = useEventCallback(
    async ([body, file]: [body: UploadFileRequestParams, file: File]) => {
      setVariables([body, file])
      const fileSizeMb = Math.ceil(file.size / MB_BYTES)
      options.onBegin?.({ event: 'begin', sentMb: 0, totalMb: fileSizeMb })
      setSentMb(0)
      setTotalMb(fileSizeMb)
      try {
        const { sourcePath, uploadId, presignedUrls } = await uploadFileStartMutation.mutateAsync([
          body,
          file,
        ])
        let i = 0
        let completedChunkCount = 0
        const parts: S3MultipartPart[] = []
        const uploadNextChunk = async (): Promise<void> => {
          const currentI = i
          const url = presignedUrls[i]
          if (url == null) {
            return
          }
          i += 1
          const promise = uploadFileChunkMutation.mutateAsync([url, file, currentI])
          // Queue the next chunk to be uploaded after this one.
          const fullPromise = promise.then(uploadNextChunk)
          parts[currentI] = await promise
          completedChunkCount += 1
          const newSentMb = Math.min(completedChunkCount * S3_CHUNK_SIZE_MB, fileSizeMb)
          setSentMb(newSentMb)
          options.onChunkSuccess?.({
            event: 'chunk',
            sentMb: newSentMb,
            totalMb: fileSizeMb,
          })
          return fullPromise
        }
        await Promise.all(Array.from({ length: FILE_UPLOAD_CONCURRENCY }).map(uploadNextChunk))
        const result = await uploadFileEndMutation.mutateAsync([
          {
            parentDirectoryId: body.parentDirectoryId,
            parts,
            sourcePath: sourcePath,
            uploadId: uploadId,
            assetId: body.fileId,
            fileName: body.fileName,
          },
        ])
        setSentMb(fileSizeMb)
        const progress: UploadFileMutationProgress = {
          event: 'end',
          sentMb: fileSizeMb,
          totalMb: fileSizeMb,
        }
        options.onSuccess?.(progress)
        options.onSettled?.(progress, null)
        return result
      } catch (error) {
        onError(error)
        options.onSettled?.(null, error)
        throw error
      }
    },
  )

  const mutate = useEventCallback(
    ([params, file]: [params: UploadFileRequestParams, file: File]) => {
      void mutateAsync([params, file])
    },
  )

  const reset = useEventCallback(() => {
    uploadFileStartMutation.reset()
    uploadFileChunkMutation.reset()
    uploadFileEndMutation.reset()
  })

  const submittedAt = uploadFileStartMutation.submittedAt

  const isError =
    uploadFileStartMutation.isError ||
    uploadFileChunkMutation.isError ||
    uploadFileEndMutation.isError
  const isSuccess = uploadFileEndMutation.isSuccess
  const isPending =
    uploadFileStartMutation.isPending ||
    uploadFileChunkMutation.isPending ||
    uploadFileEndMutation.isPending
  const isIdle =
    uploadFileStartMutation.isIdle && uploadFileChunkMutation.isIdle && uploadFileEndMutation.isIdle

  const result: MergeValuesOfObjectUnion<UploadFileMutationResult> = {
    sentMb,
    totalMb,
    variables,
    mutate,
    mutateAsync,
    context: uploadFileEndMutation.context,
    data: uploadFileEndMutation.data,
    failureCount:
      uploadFileEndMutation.failureCount +
      uploadFileChunkMutation.failureCount +
      uploadFileStartMutation.failureCount,
    failureReason:
      uploadFileEndMutation.failureReason ??
      uploadFileChunkMutation.failureReason ??
      uploadFileStartMutation.failureReason,
    isError,
    error:
      uploadFileEndMutation.error ?? uploadFileChunkMutation.error ?? uploadFileStartMutation.error,
    isPaused:
      uploadFileStartMutation.isPaused ||
      uploadFileChunkMutation.isPaused ||
      uploadFileEndMutation.isPaused,
    isPending,
    isSuccess,
    isIdle,
    status:
      isPending ? 'pending'
      : isIdle ? 'idle'
      : isSuccess ? 'success'
      : isError ? 'error'
      : 'error',
    reset,
    submittedAt,
  }
  // This is UNSAFE. Care must be taken to ensire all state is merged properly.
  // eslint-disable-next-line no-restricted-syntax
  return result as UploadFileMutationResult
}
