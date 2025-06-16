/** @file Hooks for uploading files. */
import {
  backendMutationOptions,
  listDirectoryQueryOptions,
  useEnsureListDirectory,
} from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import {
  useCategories,
  useCategoriesAPI,
  useTransferBetweenCategories,
} from '#/layouts/Drive/Categories'
import DuplicateAssetsModal, { resolveDuplications } from '#/modals/DuplicateAssetsModal'
import { useSetSelectedAssets, type SelectedAssetInfo } from '#/providers/DriveProvider'
import { setModal } from '#/providers/ModalProvider'
import type LocalBackend from '#/services/LocalBackend'
import { extractTypeAndId } from '#/services/LocalBackend'
import { noop } from '#/utilities/functions'
import { usePreventNavigation } from '#/utilities/preventNavigation'
import { useBackends, useHttpClient, useText } from '$/providers/react'
import {
  queryOptions,
  useMutation,
  useQueryClient,
  type QueryClient,
  type QueryKey,
  type UseMutationResult,
} from '@tanstack/react-query'
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
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { useState } from 'react'
import { toast } from 'react-toastify'
import invariant from 'tiny-invariant'

/** The number of bytes in 1 megabyte. */
export const MB_BYTES = 1_000_000
const S3_CHUNK_SIZE_MB = Math.round(S3_CHUNK_SIZE_BYTES / MB_BYTES)
/** The maximum number of file chunks to upload at the same time. */
const FILE_UPLOAD_CONCURRENCY = 5
/** The delay, in milliseconds, before query data for a file being uploaded is cleared. */
const CLEAR_PROGRESS_DELAY_MS = 5_000
const UPLOADING_FILES_QUERY_KEY = ['uploadingFiles'] satisfies QueryKey

/** A function to upload files. */
export function useUploadFiles(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const toastAndLog = useToastAndLog()
  const uploadFileMutation = useUploadFileMutation(backend)
  const setSelectedAssets = useSetSelectedAssets()

  return useEventCallback(async (filesToUpload: readonly File[], parentId: DirectoryId) => {
    const reversedFiles = Array.from(filesToUpload).reverse()
    const siblings = await ensureListDirectory(parentId)
    const siblingFiles = siblings.filter(assetIsFile)
    const siblingProjects = siblings.filter(assetIsProject)
    const siblingFileTitles = new Set(siblingFiles.map((asset) => asset.title))
    const siblingProjectTitles = new Set(siblingProjects.map((asset) => asset.title))
    const files = reversedFiles.filter(fileIsNotProject).map((file) => {
      const asset = createPlaceholderFileAsset(escapeSpecialCharacters(file.name), parentId)
      return { asset, file }
    })
    const projects = reversedFiles.filter(fileIsProject).map((file) => {
      const basename = escapeSpecialCharacters(stripProjectExtension(file.name))
      const asset = createPlaceholderProjectAsset(basename, parentId)
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
        new: createPlaceholderFileAsset(file.asset.title, parentId),
        file: file.file,
      }))
      const conflictingProjects = duplicateProjects.map((project) => {
        const basename = stripProjectExtension(project.asset.title)
        return {
          // This is SAFE, as `duplicateProjects` only contains projects that have
          // siblings with the same name.
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          current: siblingProjectsByName.get(basename)!,
          new: createPlaceholderProjectAsset(basename, parentId),
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
            const newFiles = files
              .filter((file) => !siblingFileTitles.has(file.asset.title))
              .map((file) => {
                const asset = createPlaceholderFileAsset(file.asset.title, parentId)
                fileMap.set(asset.id, file.file)
                return asset
              })

            const newProjects = projects
              .filter(
                (project) => !siblingProjectTitles.has(stripProjectExtension(project.asset.title)),
              )
              .map((project) => {
                const basename = stripProjectExtension(project.asset.title)
                const asset = createPlaceholderProjectAsset(basename, parentId)
                fileMap.set(asset.id, project.file)
                return asset
              })

            const assets = [...newFiles, ...newProjects]

            await Promise.allSettled(assets.map((asset) => doUploadFile(asset, 'new')))
          }}
        />,
      )
    }
  })
}

/** Upload progress for {@link useUploadFileMutation}. */
export interface UploadFileMutationProgress {
  /**
   * Whether this is the first progress update.
   * Useful to determine whether to create a new toast or to update an existing toast.
   */
  readonly event: 'begin' | 'chunk' | 'end'
  readonly sentBytes: number
  readonly totalBytes: number
}

/** Options for {@link useUploadFileMutation}. */
export interface UploadFileMutationOptions {
  /** Defaults to `true`. */
  readonly updateProgress?: boolean | undefined
  /**
   * Defaults to `3`.
   * Controls the default value of {@link UploadFileMutationOptions['chunkRetries']}
   * and {@link UploadFileMutationOptions['endRetries']}.
   */
  readonly retries?: number | undefined
  /** Defaults to {@link UploadFileMutationOptions['retries']}. */
  readonly chunkRetries?: number | undefined
  /** Defaults to {@link UploadFileMutationOptions['retries']}. */
  readonly endRetries?: number | undefined
  /** Called for all progress updates (`onBegin`, `onChunkSuccess` and `onSuccess`). */
  readonly onProgress?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called before any mutations are sent. */
  readonly onBegin?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called after each successful chunk upload mutation. */
  readonly onChunkSuccess?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called after the entire mutation succeeds. */
  readonly onSuccess?: ((progress: UploadFileMutationProgress) => void) | undefined
  /** Called after any mutations fail. */
  readonly onError?: ((error: unknown) => void) | undefined
  /** Called after `onSuccess` or `onError`, depending on whether the mutation succeeded. */
  readonly onSettled?:
    | ((progress: UploadFileMutationProgress | null, error: unknown) => void)
    | undefined
}

/** The result of a {@link useUploadFileMutation}. */
export type UploadFileMutationResult = UseMutationResult<
  UploadedLargeAsset,
  Error,
  [body: UploadFileRequestParams, file: File],
  unknown
> & { readonly sentBytes: number; readonly totalBytes: number }

/** A key for an "uploading file" computed query. */
export function uploadingFilesQueryKey() {
  return UPLOADING_FILES_QUERY_KEY
}

/** Options for an "uploading file" computed query. */
export function uploadingFileQueryOptions() {
  return queryOptions<Record<string, UploadFileMutationProgress>>({
    queryKey: uploadingFilesQueryKey(),
    initialData: {},
  })
}

/** Set the progress of a file upload. */
function setUploadingFileProgress(
  queryClient: QueryClient,
  id: string,
  progress: UploadFileMutationProgress,
) {
  queryClient.setQueryData<Record<string, UploadFileMutationProgress>>(
    UPLOADING_FILES_QUERY_KEY,
    (data) => ({ ...data, [id]: progress }),
  )
}

/** Clear the progress of file uploads if all current file uploads are done. */
function clearUploadingFileProgressIfDone(queryClient: QueryClient) {
  queryClient.setQueryData<Record<string, UploadFileMutationProgress>>(
    UPLOADING_FILES_QUERY_KEY,
    (data) => {
      if (!data) {
        return
      }
      for (const [, progress] of Object.entries(data)) {
        if (progress.event !== 'end') {
          return
        }
      }
      return {}
    },
  )
}

/**
 * Options for {@link useUploadFileToCloudMutation}.
 */
export interface UploadFileToCloudMutationOptions {
  /** The assets to upload. */
  readonly assets: readonly UploadToCloudAsset<AnyAsset['type']>[]
  /** The directory to upload the assets to. */
  readonly targetDirectoryId: DirectoryId
}

/**
 * Type that represents an asset that can be uploaded to the cloud.
 * From the local backend's perspective, this is any asset that is not a folder.
 * Theoretically, we _could_ upload folders to the cloud, but at this point it is a bit complex to do
 */
export type UploadableAsset =
  | UploadToCloudAsset<AssetType.file>
  | UploadToCloudAsset<AssetType.project>

/** An asset that can be uploaded to the cloud. */
export type UploadToCloudAsset<Type extends AssetType> = Pick<
  AnyAsset,
  'id' | 'parentId' | 'title'
> & {
  readonly type: Type
  readonly newName?: string
  /** The id of an existing cloud asset to replace. */
  readonly cloudId?: AssetId
  /** A list of siblings, if it has been fetched already. */
  readonly siblings?: readonly AnyAsset<AssetType>[]
}

const UPLOADABLE_ASSETS_SET = new Set([AssetType.file, AssetType.project])

/** Whether the asset is uploadable. */
export function isUploadableAsset(asset: UploadToCloudAsset<AssetType>): asset is UploadableAsset {
  return UPLOADABLE_ASSETS_SET.has(asset.type)
}

/** Get both deleted and non-deleted siblings. */
function useGetSiblings() {
  const queryClient = useQueryClient()
  const { cloudCategories } = useCategoriesAPI()
  const cloudHomeCategory = cloudCategories.categories.find((category) => category.type === 'cloud')
  const cloudTrashCategory = cloudCategories.categories.find(
    (category) => category.type === 'trash',
  )

  return useEventCallback(async (backend: Backend, parentId: DirectoryId) => {
    const nonDeletedAssets =
      cloudHomeCategory ?
        await queryClient.fetchQuery(
          listDirectoryQueryOptions({
            backend,
            parentId,
            category: cloudHomeCategory,
            refetchInterval: null,
          }),
        )
      : []
    const deletedAssets =
      cloudTrashCategory ?
        await queryClient.fetchQuery(
          listDirectoryQueryOptions({
            backend,
            parentId,
            category: cloudTrashCategory,
            refetchInterval: null,
          }),
        )
      : []
    return [...nonDeletedAssets, ...deletedAssets] as const
  })
}

/**
 * Packs a project into a file and uploads it to the cloud.
 * Does not work in environments that do not have a local backend.
 */
export function useUploadFileToCloudMutation() {
  const { getText } = useText()
  const httpClient = useHttpClient()
  const toastAndLog = useToastAndLog()
  const { remoteBackend } = useBackends()
  const uploadFileMutation = useUploadFileMutation(remoteBackend)
  const getSiblings = useGetSiblings()
  const { cloudCategories } = useCategoriesAPI()
  const cloudHomeCategory = cloudCategories.categories.find((category) => category.type === 'cloud')

  const upload = useEventCallback(
    /**
     * Upload a file from the Local backend to the Cloud backend.
     * @param localBackend - ignored, only used to double-check that the environment has a local backend
     */
    async (localBackend: LocalBackend, options: UploadFileToCloudMutationOptions) => {
      const { assets, targetDirectoryId } = options
      const siblings = await getSiblings(remoteBackend, targetDirectoryId)
      const assetsMap = new Map(assets.map((asset) => [asset.id, asset]))
      const siblingsMap = new Map(siblings.map((sibling) => [sibling.title, sibling]))

      const { uploadableAssets, conflictingAssets } = await assets.reduce(
        async (accPromise, asset) => {
          const acc = await accPromise
          const isUploadable = isUploadableAsset(asset)

          if (isUploadable) {
            const newName = asset.newName ?? asset.title
            const sibling = asset.cloudId == null ? siblingsMap.get(newName) : null
            if (sibling) {
              acc.conflictingAssets.push({ ...asset, cloudId: sibling.id })
            } else {
              acc.uploadableAssets.push(asset)
            }
          } else {
            acc.nonUploadableAssets.push(asset)
          }

          return acc
        },
        Promise.resolve({
          uploadableAssets: new Array<UploadableAsset>(),
          conflictingAssets: new Array<UploadableAsset>(),
          nonUploadableAssets: new Array<UploadToCloudAsset<AnyAsset['type']>>(),
        }),
      )

      return Promise.all([
        (async () => {
          if (conflictingAssets.length === 0) {
            return
          }

          invariant(
            cloudHomeCategory != null,
            'Cloud home category must exist to upload Local project to Cloud',
          )
          const resolutions = await resolveDuplications({
            canReplace: true,
            targetId: targetDirectoryId,
            conflictingIds: conflictingAssets.map((asset) => asset.id),
            category: cloudHomeCategory,
            backend: remoteBackend,
          })

          const renames = resolutions.flatMap((resolution) => {
            if (resolution.conclusion !== 'rename') {
              return []
            }
            const asset = assetsMap.get(resolution.assetId)
            if (!asset) {
              return []
            }
            return [{ ...resolution, asset }]
          })
          const replaces = resolutions.flatMap((resolution) => {
            if (resolution.conclusion !== 'replace') {
              return []
            }
            const asset = assetsMap.get(resolution.assetId)
            if (!asset) {
              return []
            }
            const sibling = siblingsMap.get(asset.title)
            if (!sibling) {
              return []
            }
            return [{ ...resolution, asset, cloudId: sibling.id }]
          })

          await upload(localBackend, {
            assets: [
              ...renames.map(
                (resolution): UploadToCloudAsset<AssetType> => ({
                  ...resolution.asset,
                  newName: resolution.newName,
                }),
              ),
              ...replaces.map(
                (resolution): UploadToCloudAsset<AssetType> => ({
                  ...resolution.asset,
                  cloudId: resolution.cloudId,
                }),
              ),
            ],
            targetDirectoryId,
          })
        })(),
        ...uploadableAssets.map(async (asset) => {
          try {
            const newName = asset.newName ?? asset.title
            const fileData = await (async () => {
              switch (asset.type) {
                case AssetType.project: {
                  // Folder's id matches the pattern `<type>-<Full Path>`, i.e. `directory-/Users/user/enso/folder 1`
                  const parentDirectoryPath = extractTypeAndId(asset.parentId).id

                  const projectResponse = await httpClient.get(
                    `/api/project-manager/projects/${extractTypeAndId(asset.id).id}/enso-project?projectsDirectory=${parentDirectoryPath}`,
                  )

                  if (!projectResponse.ok) {
                    throw new Error('Something went wrong, please try again')
                  }

                  const fileName = `${newName}.enso-project`

                  return {
                    fileName,
                    file: new File([await projectResponse.blob()], fileName),
                  }
                }
                case AssetType.file: {
                  // TODO: @MrFlashAccount  Implement file upload
                  throw new Error('File upload is not supported yet')
                }
                default:
                  throw new Error('Unknown asset type')
              }
            })()

            await uploadFileMutation.mutateAsync([
              {
                fileName: fileData.fileName,
                fileId: asset.cloudId ?? null,
                parentDirectoryId: targetDirectoryId,
              },
              fileData.file,
            ])

            toast.success(getText('uploadProjectToCloudSuccess'))
          } catch (error) {
            toastAndLog('uploadProjectToCloudError', error)
          }
        }),
      ])
    },
  )

  return upload
}

/**
 * Call "upload file" mutations for a file.
 * Always uses multipart upload for Cloud backend.
 */
export function useUploadFileMutation(
  backend: Backend,
  options: UploadFileMutationOptions = {},
): UploadFileMutationResult {
  const queryClient = useQueryClient()
  const toastAndLog = useToastAndLog()
  const { getText } = useText()
  const {
    retries = 3,
    chunkRetries = retries,
    endRetries = retries,
    updateProgress = true,
    onError = (error) => {
      toastAndLog('uploadLargeFileError', error)
    },
  } = options
  const setProgress: typeof setUploadingFileProgress =
    updateProgress ? setUploadingFileProgress : noop
  const uploadFileStartMutation = useMutation(backendMutationOptions(backend, 'uploadFileStart'))
  const uploadFileChunkMutation = useMutation(
    backendMutationOptions(backend, 'uploadFileChunk', { retry: chunkRetries }),
  )
  const uploadFileEndMutation = useMutation(
    backendMutationOptions(backend, 'uploadFileEnd', { retry: endRetries }),
  )
  const [variables, setVariables] = useState<[params: UploadFileRequestParams, file: File]>()
  const [sentBytes, setSentBytes] = useState(0)
  const [totalBytes, setTotalBytes] = useState(0)
  const mutateAsync = useEventCallback(
    async ([body, file]: [body: UploadFileRequestParams, file: File]) => {
      const progressId = uniqueString()
      setVariables([body, file])
      const fileSizeBytes = file.size
      const beginProgress: UploadFileMutationProgress = {
        event: 'begin',
        sentBytes: 0,
        totalBytes: fileSizeBytes,
      }
      options.onBegin?.(beginProgress)
      setProgress(queryClient, progressId, beginProgress)
      setSentBytes(0)
      setTotalBytes(fileSizeBytes)
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
          const newSentBytes = Math.min(
            completedChunkCount * S3_CHUNK_SIZE_MB * MB_BYTES,
            fileSizeBytes,
          )
          setSentBytes(newSentBytes)
          const chunkProgress: UploadFileMutationProgress = {
            event: 'chunk',
            sentBytes: newSentBytes,
            totalBytes: fileSizeBytes,
          }
          options.onChunkSuccess?.(chunkProgress)
          setProgress(queryClient, progressId, chunkProgress)
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
        setSentBytes(fileSizeBytes)
        const endProgress: UploadFileMutationProgress = {
          event: 'end',
          sentBytes: fileSizeBytes,
          totalBytes: fileSizeBytes,
        }
        options.onSuccess?.(endProgress)
        options.onSettled?.(endProgress, null)
        setProgress(queryClient, progressId, endProgress)
        if (updateProgress) {
          setTimeout(() => {
            clearUploadingFileProgressIfDone(queryClient)
          }, CLEAR_PROGRESS_DELAY_MS)
        }
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

  usePreventNavigation({ message: getText('anUploadIsInProgress'), isEnabled: isPending })

  const result: MergeValuesOfObjectUnion<UploadFileMutationResult> = {
    sentBytes,
    totalBytes,
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

/**
 * Download a file to local.
 * Does not work in environments that do not have a local backend.
 */
export function useUploadFileToLocal(category: Category) {
  const transferBetweenCategories = useTransferBetweenCategories(category)

  const { localCategories } = useCategories()
  const localHomeCategory = localCategories.categories.find(
    (otherCategory) => otherCategory.type === 'local',
  )
  return useEventCallback(async (assets: readonly AnyAsset[]) => {
    invariant(localHomeCategory, 'Local home category must exist to download to local')
    await transferBetweenCategories(category, localHomeCategory, assets)
  })
}
