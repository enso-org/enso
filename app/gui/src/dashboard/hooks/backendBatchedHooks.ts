/** @file Hooks to do batched backend operations. */
import {
  backendQueryOptions,
  listDirectoryQueryOptions,
  mutationOptions,
} from '#/hooks/backendHooks'
import { useUploadFileWithToastMutation } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type { TrashCategory } from '#/layouts/CategorySwitcher/Category'
import { useCloudCategoryList } from '#/layouts/Drive/Categories'
import { resolveDuplications } from '#/modals/DuplicateAssetsModal'
import { useUser } from '#/providers/AuthProvider'
import { useRemoteBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { extractTypeAndId } from '#/services/LocalBackend'
import { getMessageOrToString } from '#/utilities/error'
import {
  useMutationState,
  useQueryClient,
  type Mutation,
  type QueryClient,
} from '@tanstack/react-query'
import type { AssetType } from 'enso-common/src/services/Backend'
import {
  DuplicateAssetError,
  FilterBy,
  type AnyAsset,
  type AssetId,
  type default as Backend,
  type DirectoryId,
  type LabelName,
} from 'enso-common/src/services/Backend'
import { toast } from 'react-toastify'
import invariant from 'tiny-invariant'

/** Call "delete" mutations for a list of assets. */
export function deleteAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: [backend.type, 'deleteAssets'],
    mutationFn: async ([ids, force]: readonly [ids: readonly AssetId[], force: boolean]) => {
      const results = await Promise.allSettled(
        ids.map((id) => backend.deleteAsset(id, { force }, '(unknown)')),
      )

      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )

      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: ids.length,
        })
      }
      return null
    },
    meta: {
      invalidates: [
        [backend.type, 'listDirectory'],
        [backend.type, 'listAssetVersions'],
      ],
      awaitInvalidates: true,
      refetchType: 'all',
    },
  })
}

/** The type of a "delete assets" mutation. */
type DeleteAssetsMutation = Mutation<
  null,
  Error,
  readonly [ids: readonly AssetId[], force: boolean]
>

/** Return matching in-flight "delete assets" mutations. */
export function useDeleteAssetsMutationState<Result>(
  backend: Backend,
  options: {
    predicate?: (mutation: DeleteAssetsMutation) => boolean
    select?: (mutation: DeleteAssetsMutation) => Result
  } = {},
) {
  const { predicate, select } = options
  return useMutationState({
    filters: {
      ...deleteAssetsMutationOptions(backend),
      predicate: (mutation: DeleteAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true),
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

/** Call "restore" mutations for a list of assets. */
export function restoreAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: [backend.type, 'restoreAssets'],
    mutationFn: async ({
      ids,
      parentId = null,
    }: {
      ids: readonly AssetId[]
      parentId: DirectoryId | null
    }) => {
      const results = await Promise.allSettled(
        ids.map((id) => backend.undoDeleteAsset(id, parentId)),
      )
      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )
      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: ids.length,
        })
      }
      return null
    },
    meta: {
      invalidates: [[backend.type, 'listDirectory']],
      awaitInvalidates: true,
      refetchType: 'all',
    },
  })
}

/** The type of a "restore assets" mutation. */
type RestoreAssetsMutation = Mutation<
  null,
  Error,
  {
    readonly ids: readonly AssetId[]
    readonly parentId: DirectoryId | null
  }
>

/** Return matching in-flight "restore assets" mutations. */
export function useRestoreAssetsMutationState<Result>(
  backend: Backend,
  options: {
    predicate?: (mutation: RestoreAssetsMutation) => boolean
    select?: (mutation: RestoreAssetsMutation) => Result
  } = {},
) {
  const { predicate, select } = options
  return useMutationState({
    filters: {
      ...restoreAssetsMutationOptions(backend),
      predicate: (mutation: RestoreAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true),
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

/** Call "copy" mutations for a list of assets. */
export function copyAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: [backend.type, 'copyAssets'],
    mutationFn: async ([ids, parentId]: [ids: readonly AssetId[], parentId: DirectoryId]) => {
      /**
       * Copy an asset and return a promise that resolves to the asset or an error.
       */
      const copyAsset = async (id: AssetId) => backend.copyAsset(id, parentId)

      const results = await Promise.allSettled(ids.map((id) => copyAsset(id)))

      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )

      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: ids.length,
        })
      }

      return results.flatMap((result) => (result.status === 'fulfilled' ? [result.value] : []))
    },
    meta: {
      invalidates: [[backend.type, 'listDirectory']],
      awaitInvalidates: true,
      refetchType: 'all',
    },
  })
}

/** The type of a "move assets" mutation. */
type CopyAssetsMutation = Mutation<
  null,
  Error,
  readonly [ids: readonly AssetId[], parentId: DirectoryId]
>

/** Return matching in-flight "move assets" mutations. */
export function useCopyAssetsMutationState<Result>(
  backend: Backend,
  options: {
    predicate?: (mutation: CopyAssetsMutation) => boolean
    select?: (mutation: CopyAssetsMutation) => Result
  } = {},
) {
  const { predicate, select } = options
  return useMutationState({
    filters: {
      ...copyAssetsMutationOptions(backend),
      predicate: (mutation: CopyAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true),
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

/** Call "move" mutations for a list of assets. */
export function moveAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: [backend.type, 'moveAssets'],
    mutationFn: async ([ids, parentId]: [ids: readonly AssetId[], parentId: DirectoryId]) => {
      const results = await Promise.allSettled(
        ids.map((id) =>
          backend
            .updateAsset(
              id,
              { description: null, parentDirectoryId: parentId, title: null },
              '(unknown)',
            )
            .catch((error) => {
              if (error instanceof DuplicateAssetError) {
                return { id, error }
              }
              throw error
            }),
        ),
      )

      const duplicateErrors = results
        .filter((result) => result.status === 'fulfilled')
        .map((result) =>
          typeof result.value === 'object' && 'error' in result.value ? result.value : null,
        )
        .filter((error) => error != null)

      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )

      if (duplicateErrors.length !== 0) {
        const resolutions = await resolveDuplications({
          targetId: parentId,
          conflictingIds: duplicateErrors.map((error) => error.id),
        })

        const renames = resolutions.filter((resolution) => resolution.conclusion === 'rename')

        await Promise.allSettled(
          renames.map((resolution) =>
            backend.updateAsset(
              resolution.assetId,
              {
                parentDirectoryId: parentId,
                description: null,
                title: resolution.newName,
              },
              resolution.newName,
            ),
          ),
        )
      }

      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: ids.length,
        })
      }

      return results.flatMap((result) => (result.status === 'fulfilled' ? [result.value] : []))
    },
    meta: {
      invalidates: [
        [backend.type, 'listDirectory'],
        [backend.type, 'listAssetVersions'],
      ],
      awaitInvalidates: true,
    },
  })
}

/** The type of a "move assets" mutation. */
type MoveAssetsMutation = Mutation<
  null,
  Error,
  readonly [ids: readonly AssetId[], parentId: DirectoryId]
>

/** Return matching in-flight "move assets" mutations. */
export function useMoveAssetsMutationState<Result>(
  backend: Backend,
  options: {
    predicate?: (mutation: MoveAssetsMutation) => boolean
    select?: (mutation: MoveAssetsMutation) => Result
  } = {},
) {
  const { predicate, select } = options
  return useMutationState({
    filters: {
      ...moveAssetsMutationOptions(backend),
      predicate: (mutation: MoveAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true),
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

/** Get a list of all items in the trash. */
export async function getAllTrashedItems(
  queryClient: QueryClient,
  backend: Backend,
  category: TrashCategory,
) {
  return await queryClient.ensureQueryData(
    backendQueryOptions(backend, 'listDirectory', [
      {
        parentId: category.homeDirectoryId,
        labels: null,
        filterBy: FilterBy.trashed,
        recentProjects: false,
      },
      '(unknown)',
    ]),
  )
}

/** Call "download" mutations for a list of assets. */
export function downloadAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationFn: async (infos: readonly { id: AssetId; title: string }[]) => {
      const results = await Promise.allSettled(
        infos.map(({ id, title }) => backend.download(id, title)),
      )
      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )
      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: infos.length,
        })
      }
      return null
    },
  })
}

/** Call "add label" mutations for a list of assets. */
export function addAssetsLabelsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationFn: async ([infos, labelNames]: [
      infos: readonly Pick<AnyAsset, 'id' | 'labels'>[],
      labelNames: readonly LabelName[],
    ]) => {
      const results = await Promise.allSettled(
        infos.map(async ({ id, labels }) => {
          const newLabels = [
            ...new Set([
              ...(labels ?? []),
              ...labelNames.filter((label) => labels?.includes(label) !== true),
            ]),
          ]
          if (newLabels.length !== labels?.length) {
            await backend.associateTag(id, newLabels, '(unknown)')
          }
        }),
      )
      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )
      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: infos.length,
        })
      }
      return null
    },
    meta: {
      invalidates: [[backend.type, 'listDirectory']],
      awaitInvalidates: true,
      refetchType: 'all',
    },
  })
}

/** Call "remove label" mutations for a list of assets. */
export function removeAssetsLabelsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationFn: async ([infos, labelNames]: [
      infos: readonly Pick<AnyAsset, 'id' | 'labels'>[],
      labelNames: readonly LabelName[],
    ]) => {
      const results = await Promise.allSettled(
        infos.map(async ({ id, labels }) => {
          const labelNamesSet = new Set(labelNames)
          const newLabels = (labels ?? []).filter((label) => !labelNamesSet.has(label))
          if (labels && newLabels.length !== labels.length) {
            await backend.associateTag(id, newLabels, '(unknown)')
          }
        }),
      )
      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )
      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: infos.length,
        })
      }
      return null
    },
    meta: {
      invalidates: [[backend.type, 'listDirectory']],
      awaitInvalidates: true,
      refetchType: 'all',
    },
  })
}

/** Get both deleted and non-deleted siblings. */
function useGetSiblings() {
  const queryClient = useQueryClient()
  const cloudCategories = useCloudCategoryList()
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
    return [...nonDeletedAssets, ...deletedAssets]
  })
}

/** Return a callback to upload a project to the cloud. */
function useUploadAssetToCloud() {
  const { getText } = useText()
  const user = useUser()
  const toastAndLog = useToastAndLog()
  const remoteBackend = useRemoteBackend()
  const uploadFileMutation = useUploadFileWithToastMutation(remoteBackend)
  const getSiblings = useGetSiblings()

  return useEventCallback(
    async (
      asset: Pick<AnyAsset, 'id' | 'parentId' | 'title'>,
      parentDirectoryId: DirectoryId | null = null,
      newName?: string,
      /** A list of siblings, if it has been fetched already. */
      siblings?: readonly AnyAsset<AssetType>[],
    ) => {
      const { parentId, id, title } = asset
      newName ??= title
      siblings ??= await getSiblings(remoteBackend, parentDirectoryId ?? user.rootDirectoryId)
      const siblingTitles = siblings.map((sibling) => sibling.title)

      if (siblingTitles.includes(newName)) {
        throw new DuplicateAssetError(
          'Could not upload to cloud: A resource with that title already exists.',
        )
      }

      try {
        const parentDirectoryPath = extractTypeAndId(parentId).id

        const projectResponse = await fetch(
          `./api/project-manager/projects/${extractTypeAndId(id).id}/enso-project?projectsDirectory=${parentDirectoryPath}`,
        )

        if (!projectResponse.ok) {
          throw new Error('Something went wrong, please try again')
        }

        const fileName = `${newName}.enso-project`
        await uploadFileMutation
          .mutateAsync([
            { fileName, fileId: null, parentDirectoryId },
            new File([await projectResponse.blob()], fileName),
          ])
          .catch()
        toast.success(getText('uploadProjectToCloudSuccess'))
      } catch (error) {
        toastAndLog('uploadProjectToCloudError', error)
      }
    },
  )
}

/** Return a callback to upload one or more projects to the cloud. */
export function useUploadAssetsToCloud() {
  const user = useUser()
  const uploadAssetToCloud = useUploadAssetToCloud()
  const remoteBackend = useRemoteBackend()
  const getSiblings = useGetSiblings()
  const cloudCategories = useCloudCategoryList()
  const cloudHomeCategory = cloudCategories.categories.find((category) => category.type === 'cloud')

  return useEventCallback(
    async (assets: readonly Pick<AnyAsset, 'id' | 'parentId' | 'title'>[]) => {
      const parentDirectoryId = user.rootDirectoryId
      const siblings = await getSiblings(remoteBackend, parentDirectoryId)

      const results = await Promise.allSettled(
        assets.map((asset) =>
          uploadAssetToCloud(asset, null, undefined, siblings).catch((error) => {
            if (error instanceof DuplicateAssetError) {
              return { id: asset.id, error }
            }
            throw error
          }),
        ),
      )

      const duplicateErrors = results
        .filter((result) => result.status === 'fulfilled')
        .map((result) =>
          typeof result.value === 'object' && 'error' in result.value ? result.value : null,
        )
        .filter((error) => error != null)

      const errors = results.flatMap((result): unknown =>
        result.status === 'rejected' ? [result.reason] : [],
      )

      if (duplicateErrors.length !== 0) {
        invariant(
          cloudHomeCategory != null,
          'Cloud home category must exist to upload Local project to Cloud',
        )

        const resolutions = await resolveDuplications({
          targetId: parentDirectoryId,
          conflictingIds: duplicateErrors.map((error) => error.id),
          category: cloudHomeCategory,
          backend: remoteBackend,
        })

        const assetsMap = new Map(assets.map((asset) => [asset.id, asset]))
        const renames = resolutions.flatMap((resolution) => {
          if (resolution.conclusion !== 'rename') {
            return []
          }
          const asset = assetsMap.get(resolution.assetId)
          return asset ? [{ ...resolution, asset }] : []
        })

        const newSiblings = await getSiblings(remoteBackend, parentDirectoryId)
        await Promise.allSettled(
          renames.map((resolution) =>
            uploadAssetToCloud(resolution.asset, null, resolution.newName, newSiblings),
          ),
        )
      }

      if (errors.length !== 0) {
        throw Object.assign(new Error(errors.map(getMessageOrToString).join('\n')), {
          errors,
          failed: errors.length,
          total: assets.length,
        })
      }

      return results.flatMap((result) => (result.status === 'fulfilled' ? [result.value] : []))
    },
  )
}
