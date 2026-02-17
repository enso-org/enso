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
import { resolveDuplications } from '#/modals/DuplicateAssetsModal'
import { useSetSelectedAssets, type SelectedAssetInfo } from '#/providers/DriveProvider'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends, useHttpClient, useText } from '$/providers/react'
import { useUploadsToCloudStore } from '$/providers/react/upload'
import { useQueryClient } from '@tanstack/react-query'
import {
  AssetType,
  BackendType,
  escapeSpecialCharacters,
  extractProjectExtension,
  fileIsProject,
  stripProjectExtension,
  type AnyAsset,
  type AssetId,
  type Backend,
  type DirectoryId,
  type FileId,
  type ProjectId,
  type UploadFileRequestParams,
} from 'enso-common/src/services/Backend'
import type { LocalBackend } from 'enso-common/src/services/LocalBackend'
import { toast } from 'react-toastify'
import invariant from 'tiny-invariant'

declare module '$/utils/queryClient' {
  /** */
  interface MutationPools {
    readonly uploadFileChunk: true
  }
}

/**
 * Function for uploading files to Local Backend. It requires less hassle than multipart
 * upload to Cloud.
 */
function useUploadLocally(backend: Backend) {
  const uploadFileStart = useMutationCallback(backendMutationOptions(backend, 'uploadFileStart'))
  const uploadFileEnd = useMutationCallback(backendMutationOptions(backend, 'uploadFileEnd'))

  return async (file: File, params: UploadFileRequestParams) => {
    const { uploadId, sourcePath } = await uploadFileStart([params, file])
    return uploadFileEnd([
      {
        uploadId,
        sourcePath,
        parts: [],
        assetId: params.fileId,
        ...params,
      },
    ])
  }
}

/** A function to upload files. */
export function useUploadFiles(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const uploads = useUploadsToCloudStore()
  const uploadLocally = useUploadLocally(backend)
  const setSelectedAssets = useSetSelectedAssets()
  const uploadFile =
    backend.type === BackendType.local ? uploadLocally : uploads.uploadFile.bind(uploads)

  return useEventCallback(async (filesToUpload: readonly File[], parentId: DirectoryId) => {
    const reversedFiles = Array.from(filesToUpload).reverse()
    const siblings = await ensureListDirectory(parentId)
    const siblingsByTitle = new Map(siblings.map((asset) => [asset.title, asset]))
    const files = reversedFiles.map((file) => {
      if (fileIsProject(file)) {
        const title = escapeSpecialCharacters(stripProjectExtension(file.name))
        return { title, file }
      } else {
        const title = escapeSpecialCharacters(file.name)
        return { title, file }
      }
    })
    const duplicates = new Map(
      files.flatMap((file) => {
        const asset = siblingsByTitle.get(file.title)
        return asset ? [[file.file, { asset, ...file }]] : []
      }),
    )
    const uploadedFileInfos: SelectedAssetInfo[] = []
    const addToSelection = (info: SelectedAssetInfo) => {
      uploadedFileInfos.push(info)
      setSelectedAssets(uploadedFileInfos)
    }

    const doUploadFile = async (file: File, title: string, fileId: AssetId | null = null) => {
      if (fileIsProject(file)) {
        const { extension } = extractProjectExtension(file.name)
        title = escapeSpecialCharacters(stripProjectExtension(title))

        await uploadFile(
          file,
          {
            fileId,
            fileName: `${title}.${extension}`,
            parentDirectoryId: parentId,
          },
          'requestedByUser',
        ).then((result) => {
          if (result.jobId != null) {
            return
          }
          addToSelection({
            type: AssetType.project,
            // This is SAFE, because it is guarded behind `assetIsProject`.
            // eslint-disable-next-line no-restricted-syntax
            id: result.id as ProjectId,
            parentId,
            title,
          })
        })
      } else {
        title = escapeSpecialCharacters(title)
        await uploadFile(
          file,
          { fileId, fileName: title, parentDirectoryId: parentId },
          'requestedByUser',
        ).then((result) => {
          if (result.jobId != null) {
            return
          }
          addToSelection({
            type: AssetType.file,
            // This is SAFE, because it is guarded behind `assetIsFile`.
            // eslint-disable-next-line no-restricted-syntax
            id: result.id as FileId,
            parentId,
            title,
          })
        })
      }
    }

    const resolutions =
      duplicates.size === 0 ?
        []
      : await resolveDuplications({
          targetId: parentId,
          conflictingIds: Array.from(duplicates.values(), ({ asset }) => asset.id),
        })
    const resolutionsById = new Map(
      resolutions.map((resolution) => [resolution.assetId, resolution]),
    )

    await Promise.allSettled(
      files.flatMap(({ file, title }) => {
        const duplicate = duplicates.get(file)
        if (duplicate == null) {
          return [doUploadFile(file, title)]
        }
        const resolution = resolutionsById.get(duplicate.asset.id)
        if (resolution == null) {
          return [doUploadFile(file, title)]
        }
        switch (resolution.conclusion) {
          case 'rename': {
            return [doUploadFile(duplicate.file, resolution.newName)]
          }
          case 'replace': {
            return [doUploadFile(duplicate.file, duplicate.asset.title, duplicate.asset.id)]
          }
          case 'skip': {
            // Ignored.
            return []
          }
        }
      }),
    )
  })
}

/**
 * Options for {@link useUploadFileToCloud}.
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
            labels: null,
            sortExpression: null,
            sortDirection: null,
            refetchInterval: null,
          }),
        )
      : null
    const deletedAssets =
      cloudTrashCategory ?
        await queryClient.fetchQuery(
          listDirectoryQueryOptions({
            backend,
            parentId,
            category: cloudTrashCategory,
            labels: null,
            sortExpression: null,
            sortDirection: null,
            refetchInterval: null,
          }),
        )
      : null
    return [...(nonDeletedAssets?.assets ?? []), ...(deletedAssets?.assets ?? [])] as const
  })
}

/**
 * Packs a project into a file and uploads it to the cloud.
 * Does not work in environments that do not have a local backend.
 */
export function useUploadFileToCloud() {
  const { getText } = useText()
  const httpClient = useHttpClient()
  const toastAndLog = useToastAndLog()
  const { remoteBackend } = useBackends()
  const uploads = useUploadsToCloudStore()
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
                  const projectResponse = await httpClient.get(`/api/projects/${asset.id}/download`)

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

            await uploads.uploadFile(
              fileData.file,
              {
                fileName: fileData.fileName,
                fileId: asset.cloudId ?? null,
                parentDirectoryId: targetDirectoryId,
              },
              'requestedByUser',
            )

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
