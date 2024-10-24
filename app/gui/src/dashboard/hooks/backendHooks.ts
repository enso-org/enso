/** @file Hooks for interacting with the backend. */
import { useId, useMemo, useState } from 'react'

import {
  queryOptions,
  useMutation,
  useMutationState,
  useQuery,
  type Mutation,
  type MutationKey,
  type UseMutationOptions,
  type UseQueryOptions,
  type UseQueryResult,
} from '@tanstack/react-query'
import { toast } from 'react-toastify'
import invariant from 'tiny-invariant'

import {
  backendQueryOptions as backendQueryOptionsBase,
  type BackendMethods,
} from 'enso-common/src/backendQuery'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useToastAndLog, useToastAndLogWithId } from '#/hooks/toastAndLogHooks'
import { CATEGORY_TO_FILTER_BY, type Category } from '#/layouts/CategorySwitcher/Category'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import {
  AssetType,
  BackendType,
  type AnyAsset,
  type AssetId,
  type DirectoryAsset,
  type DirectoryId,
  type User,
  type UserGroupInfo,
} from '#/services/Backend'
import { TEAMS_DIRECTORY_ID, USERS_DIRECTORY_ID } from '#/services/remoteBackendPaths'
import { usePreventNavigation } from '#/utilities/preventNavigation'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'

// The number of bytes in 1 megabyte.
const MB_BYTES = 1_000_000
const S3_CHUNK_SIZE_MB = Math.round(backendModule.S3_CHUNK_SIZE_BYTES / MB_BYTES)

// ============================
// === DefineBackendMethods ===
// ============================

/** Ensure that the given type contains only names of backend methods. */
type DefineBackendMethods<T extends keyof Backend> = T

// ======================
// === MutationMethod ===
// ======================

/** Names of methods corresponding to mutations. */
export type MutationMethod = DefineBackendMethods<
  | 'acceptInvitation'
  | 'associateTag'
  | 'changeUserGroup'
  | 'closeProject'
  | 'copyAsset'
  | 'createCheckoutSession'
  | 'createDatalink'
  | 'createDirectory'
  | 'createPermission'
  | 'createProject'
  | 'createSecret'
  | 'createTag'
  | 'createUser'
  | 'createUserGroup'
  | 'declineInvitation'
  | 'deleteAsset'
  | 'deleteDatalink'
  | 'deleteInvitation'
  | 'deleteTag'
  | 'deleteUser'
  | 'deleteUserGroup'
  | 'duplicateProject'
  | 'inviteUser'
  | 'logEvent'
  | 'openProject'
  | 'removeUser'
  | 'resendInvitation'
  | 'restoreUser'
  | 'undoDeleteAsset'
  | 'updateAsset'
  | 'updateDirectory'
  | 'updateFile'
  | 'updateOrganization'
  | 'updateProject'
  | 'updateSecret'
  | 'updateUser'
  | 'uploadFileChunk'
  | 'uploadFileEnd'
  | 'uploadFileStart'
  | 'uploadOrganizationPicture'
  | 'uploadUserPicture'
>

// =======================
// === useBackendQuery ===
// =======================

export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>
export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryOptions<Awaited<ReturnType<Backend[Method]>> | undefined>
/** Wrap a backend method call in a React Query. */
export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  // @ts-expect-error This call is generic over the presence or absence of `inputData`.
  return queryOptions<Awaited<ReturnType<Backend[Method]>>>({
    ...options,
    ...backendQueryOptionsBase(backend, method, args, options?.queryKey),
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

export function useBackendQuery<Method extends BackendMethods>(
  backend: Backend,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryResult<Awaited<ReturnType<Backend[Method]>>>
export function useBackendQuery<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): UseQueryResult<Awaited<ReturnType<Backend[Method]>> | undefined>
/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryFn' | 'queryKey'> &
    Partial<Pick<UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  return useQuery(backendQueryOptions(backend, method, args, options))
}

// ==========================
// === useBackendMutation ===
// ==========================

const INVALIDATE_ALL_QUERIES = Symbol('invalidate all queries')
const INVALIDATION_MAP: Partial<
  Record<MutationMethod, readonly (BackendMethods | typeof INVALIDATE_ALL_QUERIES)[]>
> = {
  createUser: ['usersMe'],
  updateUser: ['usersMe'],
  deleteUser: ['usersMe'],
  restoreUser: ['usersMe'],
  uploadUserPicture: ['usersMe'],
  updateOrganization: ['getOrganization'],
  uploadOrganizationPicture: ['getOrganization'],
  createUserGroup: ['listUserGroups'],
  deleteUserGroup: ['listUserGroups'],
  changeUserGroup: ['listUsers'],
  createTag: ['listTags'],
  deleteTag: ['listTags'],
  associateTag: ['listDirectory'],
  acceptInvitation: [INVALIDATE_ALL_QUERIES],
  declineInvitation: ['usersMe'],
  createProject: ['listDirectory'],
  duplicateProject: ['listDirectory'],
  createDirectory: ['listDirectory'],
  createSecret: ['listDirectory'],
  updateSecret: ['listDirectory'],
  createDatalink: ['listDirectory', 'getDatalink'],
  uploadFileEnd: ['listDirectory'],
  copyAsset: ['listDirectory', 'listAssetVersions'],
  deleteAsset: ['listDirectory', 'listAssetVersions'],
  undoDeleteAsset: ['listDirectory'],
  updateAsset: ['listDirectory', 'listAssetVersions'],
  closeProject: ['listDirectory', 'listAssetVersions'],
  updateDirectory: ['listDirectory'],
}

/** The type of the corresponding mutation for the given backend method. */
export type BackendMutation<Method extends MutationMethod> = Mutation<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
>

export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  >,
): UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>
export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  >,
): UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>> | undefined,
  Error,
  Parameters<Backend[Method]>
>
/** Wrap a backend method call in a React Query Mutation. */
export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>>,
    'mutationFn'
  >,
): UseMutationOptions<Awaited<ReturnType<Backend[Method]>>, Error, Parameters<Backend[Method]>> {
  return {
    ...options,
    mutationKey: [backend?.type, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: (args) => (backend?.[method] as any)?.(...args),
    networkMode: backend?.type === BackendType.local ? 'always' : 'online',
    meta: {
      invalidates: [
        ...(options?.meta?.invalidates ?? []),
        ...(INVALIDATION_MAP[method]?.map((queryMethod) =>
          queryMethod === INVALIDATE_ALL_QUERIES ? [backend?.type] : [backend?.type, queryMethod],
        ) ?? []),
      ],
      awaitInvalidates: options?.meta?.awaitInvalidates ?? true,
    },
  }
}

// ==================================
// === useListUserGroupsWithUsers ===
// ==================================

/** A user group, as well as the users that are a part of the user group. */
export interface UserGroupInfoWithUsers extends UserGroupInfo {
  readonly users: readonly User[]
}

/** A list of user groups, taking into account optimistic state. */
export function useListUserGroupsWithUsers(
  backend: Backend,
): readonly UserGroupInfoWithUsers[] | null {
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  return useMemo(() => {
    if (listUserGroupsQuery.data == null || listUsersQuery.data == null) {
      return null
    } else {
      const result = listUserGroupsQuery.data.map((userGroup) => {
        const usersInGroup: readonly User[] = listUsersQuery.data.filter((user) =>
          user.userGroups?.includes(userGroup.id),
        )
        return { ...userGroup, users: usersInGroup }
      })
      return result
    }
  }, [listUserGroupsQuery.data, listUsersQuery.data])
}

/**
 * Upload progress for {@link useUploadFileMutation}.
 */
export interface UploadFileMutationProgress {
  /**
   * Whether this is the first progress update.
   * Useful to determine whether to create a new toast or to update an existing toast.
   */
  readonly event: 'begin' | 'chunk' | 'end'
  readonly sentMb: number
  readonly totalMb: number
}

/** Data for a specific asset. */
export function useAssetPassiveListener(
  backendType: BackendType,
  assetId: AssetId | null | undefined,
  parentId: DirectoryId | null | undefined,
  category: Category,
) {
  const listDirectoryQuery = useQuery<readonly AnyAsset<AssetType>[] | undefined>({
    queryKey: [
      backendType,
      'listDirectory',
      parentId,
      {
        labels: null,
        filterBy: CATEGORY_TO_FILTER_BY[category.type],
        recentProjects: category.type === 'recent',
      },
    ],
    initialData: undefined,
  })
  const asset = listDirectoryQuery.data?.find((child) => child.id === assetId)
  if (asset || !assetId || !parentId) {
    return asset
  }
  switch (assetId) {
    case USERS_DIRECTORY_ID: {
      return {
        id: assetId,
        parentId,
        type: AssetType.directory,
        projectState: null,
        title: 'Users',
        description: '',
        modifiedAt: toRfc3339(new Date()),
        permissions: [],
        labels: [],
      } satisfies DirectoryAsset
    }
    case TEAMS_DIRECTORY_ID: {
      return {
        id: assetId,
        parentId,
        type: AssetType.directory,
        projectState: null,
        title: 'Teams',
        description: '',
        modifiedAt: toRfc3339(new Date()),
        permissions: [],
        labels: [],
      } satisfies DirectoryAsset
    }
    default: {
      return
    }
  }
}

/** Data for a specific asset. */
export function useAssetPassiveListenerStrict(
  backendType: BackendType,
  assetId: AssetId | null | undefined,
  parentId: DirectoryId | null | undefined,
  category: Category,
) {
  const asset = useAssetPassiveListener(backendType, assetId, parentId, category)
  invariant(asset, 'Asset not found')
  return asset
}

/** Return matching in-flight mutations */
export function useBackendMutationState<Method extends MutationMethod, Result>(
  backend: Backend,
  method: Method,
  options: {
    mutationKey?: MutationKey
    predicate?: (mutation: BackendMutation<Method>) => boolean
    select?: (mutation: BackendMutation<Method>) => Result
  } = {},
) {
  const { mutationKey, predicate, select } = options
  return useMutationState({
    filters: {
      ...backendMutationOptions(backend, method, mutationKey ? { mutationKey } : {}),
      predicate: (mutation: BackendMutation<Method>) =>
        mutation.state.status === 'pending' && (predicate?.(mutation) ?? true),
    },
    // This is UNSAFE when the `Result` parameter is explicitly specified in the
    // generic parameter list.
    // eslint-disable-next-line no-restricted-syntax
    select: select as (mutation: Mutation<unknown, Error, unknown, unknown>) => Result,
  })
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

/**
 * Call "upload file" mutations for a file.
 * Always uses multipart upload for Cloud backend.
 * Shows toasts to update progress.
 */
export function useUploadFileWithToastMutation(
  backend: Backend,
  options: UploadFileMutationOptions = {},
) {
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
export function useUploadFileMutation(backend: Backend, options: UploadFileMutationOptions = {}) {
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
  const [variables, setVariables] =
    useState<[params: backendModule.UploadFileRequestParams, file: File]>()
  const [sentMb, setSentMb] = useState(0)
  const [totalMb, setTotalMb] = useState(0)
  const mutateAsync = useEventCallback(
    async (body: backendModule.UploadFileRequestParams, file: File) => {
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
        const parts: backendModule.S3MultipartPart[] = []
        for (const [url, i] of Array.from(
          presignedUrls,
          (presignedUrl, index) => [presignedUrl, index] as const,
        )) {
          parts.push(await uploadFileChunkMutation.mutateAsync([url, file, i]))
          const newSentMb = Math.min((i + 1) * S3_CHUNK_SIZE_MB, fileSizeMb)
          setSentMb(newSentMb)
          options.onChunkSuccess?.({
            event: 'chunk',
            sentMb: newSentMb,
            totalMb: fileSizeMb,
          })
        }
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
  const mutate = useEventCallback((params: backendModule.UploadFileRequestParams, file: File) => {
    void mutateAsync(params, file)
  })

  return {
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
    isError:
      uploadFileStartMutation.isError ||
      uploadFileChunkMutation.isError ||
      uploadFileEndMutation.isError,
    error:
      uploadFileEndMutation.error ?? uploadFileChunkMutation.error ?? uploadFileStartMutation.error,
    isPaused:
      uploadFileStartMutation.isPaused ||
      uploadFileChunkMutation.isPaused ||
      uploadFileEndMutation.isPaused,
    isPending:
      uploadFileStartMutation.isPending ||
      uploadFileChunkMutation.isPending ||
      uploadFileEndMutation.isPending,
    isSuccess: uploadFileEndMutation.isSuccess,
  }
}
