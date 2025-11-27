/** @file Hooks to do batched backend operations. */
import { backendQueryOptions, mutationOptions } from '#/hooks/backendHooks'
import type { TrashCategory } from '#/layouts/CategorySwitcher/Category'
import { resolveDuplications } from '#/modals/DuplicateAssetsModal'
import {
  useMutationState,
  type Mutation,
  type QueryClient,
  type UseMutationOptions,
} from '@tanstack/react-query'
import {
  DuplicateAssetError,
  FilterBy,
  type AnyAsset,
  type AssetId,
  type Backend,
  type BackendType,
  type DirectoryId,
} from 'enso-common/src/services/Backend'
import { LocalBackend } from 'enso-common/src/services/LocalBackend'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { getMessageOrToString } from 'enso-common/src/utilities/errors'

/** Extract the corresponding {@link Mutation} type from a `MutationOptions` function. */
export type MutationFromOptionsFunction<T extends (...args: never) => unknown> =
  ReturnType<T> extends (
    UseMutationOptions<infer TData, infer TError, infer TVariables, infer TContext>
  ) ?
    Mutation<TData, TError, TVariables, TContext>
  : never

export const DELETE_ASSETS_MUTATION_METHOD = 'deleteAssets'

/** A key for {@link deleteAssetsMutationOptions}. */
export function deleteAssetsMutationKey(backendType: BackendType) {
  return [backendType, DELETE_ASSETS_MUTATION_METHOD]
}

/** Call "delete" mutations for a list of assets. */
export function deleteAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: deleteAssetsMutationKey(backend.type),
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
        [backend.type, 'getAssetDetails'],
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
      // We rely on mutation key pointing to properly typed mutation.
      // eslint-disable-next-line no-restricted-syntax
      predicate: ((mutation: DeleteAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true)) as (
        mutation: Mutation,
      ) => boolean,
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

export const RESTORE_ASSETS_MUTATION_METHOD = 'restoreAssets'

/** A key for {@link restoreAssetsMutationOptions}. */
export function restoreAssetsMutationKey(backendType: BackendType) {
  return [backendType, RESTORE_ASSETS_MUTATION_METHOD]
}

/** Call "restore" mutations for a list of assets. */
export function restoreAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: restoreAssetsMutationKey(backend.type),
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
      invalidates: [
        [backend.type, 'listDirectory'],
        [backend.type, 'getAssetDetails'],
      ],
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
      // We rely on mutation key pointing to properly typed mutation.
      // eslint-disable-next-line no-restricted-syntax
      predicate: ((mutation: RestoreAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true)) as (
        mutation: Mutation,
      ) => boolean,
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

export const COPY_ASSETS_MUTATION_METHOD = 'copyAssets'

/** A key for {@link copyAssetsMutationOptions}. */
export function copyAssetsMutationKey(backendType: BackendType) {
  return [backendType, COPY_ASSETS_MUTATION_METHOD]
}

/** Call "copy" mutations for a list of assets. */
export function copyAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: copyAssetsMutationKey(backend.type),
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
      invalidates: [
        [backend.type, 'listDirectory'],
        [backend.type, 'getAssetDetails'],
      ],
      awaitInvalidates: true,
      refetchType: 'all',
    },
  })
}

/** The type of a "copy assets" mutation. */
type CopyAssetsMutation = Mutation<
  null,
  Error,
  readonly [ids: readonly AssetId[], parentId: DirectoryId]
>

/** Return matching in-flight "copy assets" mutations. */
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
      // We rely on mutation key pointing to properly typed mutation.
      // eslint-disable-next-line no-restricted-syntax
      predicate: ((mutation: CopyAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true)) as (
        mutation: Mutation,
      ) => boolean,
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
}

export const MOVE_ASSETS_MUTATION_METHOD = 'moveAssets'

/** A key for {@link moveAssetsMutationOptions}. */
export function moveAssetsMutationKey(backendType: BackendType) {
  return [backendType, MOVE_ASSETS_MUTATION_METHOD]
}

/** Call "move" mutations for a list of assets. */
export function moveAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: moveAssetsMutationKey(backend.type),
    mutationFn: async ([ids, parentId]: [ids: readonly AssetId[], parentId: DirectoryId]) => {
      const results = await Promise.allSettled(
        ids.map((id) =>
          backend
            .updateAsset(
              id,
              { description: null, parentDirectoryId: parentId, title: null, metadataId: null },
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
                metadataId: null,
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
      // We rely on mutation key pointing to properly typed mutation.
      // eslint-disable-next-line no-restricted-syntax
      predicate: ((mutation: MoveAssetsMutation) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true)) as (
        mutation: Mutation,
      ) => boolean,
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
): Promise<readonly AnyAsset[]> {
  return (
    await queryClient.ensureQueryData(
      backendQueryOptions(backend, 'listDirectory', [
        {
          parentId: category.homeDirectoryId,
          labels: null,
          filterBy: FilterBy.trashed,
          recentProjects: false,
          from: null,
          pageSize: null,
          sortExpression: null,
          sortDirection: null,
        },
        '(unknown)',
      ]),
    )
  ).assets
}

/** Options for the "download" mutation. */
export interface DownloadAssetsMutationOptions {
  readonly ids: readonly Pick<AnyAsset, 'id' | 'title'>[]
  readonly targetDirectoryId: DirectoryId | null
  readonly shouldUnpackProject?: boolean
}

/** Call "download" mutations for a list of assets. */
export function downloadAssetsMutationOptions(backend: Backend) {
  return mutationOptions({
    mutationKey: [backend.type, 'downloadAssets'],
    mutationFn: async (options: DownloadAssetsMutationOptions) => {
      const { ids, targetDirectoryId, shouldUnpackProject } = options

      // Downloading assets should be done in order, because we want to avoid potential
      // race conditions.
      const rejects = []
      for (const { id, title } of ids) {
        try {
          await backend.download(id, title, targetDirectoryId, shouldUnpackProject)
        } catch (error) {
          rejects.push(error)
        }
      }

      if (rejects.length !== 0) {
        throw Object.assign(new Error(rejects.map(getMessageOrToString).join('\n')), {
          errors: rejects,
          failed: rejects.length,
          total: ids.length,
        })
      }

      return null
    },
    meta: {
      invalidates: [
        [RemoteBackend.type, 'listDirectory'],
        [LocalBackend.type, 'listDirectory'],
      ],
      awaitInvalidates: true,
    },
  })
}
