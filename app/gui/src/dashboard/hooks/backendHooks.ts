/** @file Hooks for interacting with the backend. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import { toast } from 'react-toastify'

import * as backendQuery from 'enso-common/src/backendQuery'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useToastAndLog, useToastAndLogWithId } from '#/hooks/toastAndLogHooks'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

// The number of bytes in 1 megabyte.
const MB_BYTES = 1_000_000
const S3_CHUNK_SIZE_MB = Math.round(backendModule.S3_CHUNK_SIZE_BYTES / MB_BYTES)
const TOAST_SUCCESS_AUTO_CLOSE_MS = 5_000

// ============================
// === DefineBackendMethods ===
// ============================

/** Ensure that the given type contains only names of backend methods. */
// eslint-disable-next-line no-restricted-syntax
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
  // TODO: `get*` are not mutations, but are currently used in some places.
  | 'getDatalink'
  | 'getFileDetails'
  | 'getProjectDetails'
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
  | 'uploadFile'
  | 'uploadFileChunk'
  | 'uploadFileEnd'
  | 'uploadFileStart'
  | 'uploadOrganizationPicture'
  | 'uploadUserPicture'
>

// =======================
// === useBackendQuery ===
// =======================

export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>,
    'queryFn' | 'queryKey'
  > &
    Partial<Pick<reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): reactQuery.UseQueryResult<Awaited<ReturnType<Backend[Method]>>>
export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>,
    'queryFn' | 'queryKey'
  > &
    Partial<Pick<reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
): reactQuery.UseQueryResult<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Backend[Method]>> | undefined
>
/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends backendQuery.BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
  options?: Omit<
    reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>,
    'queryFn' | 'queryKey'
  > &
    Partial<Pick<reactQuery.UseQueryOptions<Awaited<ReturnType<Backend[Method]>>>, 'queryKey'>>,
) {
  return reactQuery.useQuery<Awaited<ReturnType<Backend[Method]>>>({
    ...options,
    ...backendQuery.backendQueryOptions(backend, method, args, options?.queryKey),
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

// ==========================
// === useBackendMutation ===
// ==========================

const INVALIDATE_ALL_QUERIES = Symbol('invalidate all queries')
const INVALIDATION_MAP: Partial<
  Record<MutationMethod, readonly (backendQuery.BackendMethods | typeof INVALIDATE_ALL_QUERIES)[]>
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
  createDatalink: ['listDirectory'],
  uploadFile: ['listDirectory'],
  copyAsset: ['listDirectory', 'listAssetVersions'],
  deleteAsset: ['listDirectory', 'listAssetVersions'],
  undoDeleteAsset: ['listDirectory'],
  updateAsset: ['listDirectory', 'listAssetVersions'],
  closeProject: ['listDirectory', 'listAssetVersions'],
  updateDirectory: ['listDirectory'],
}

export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >,
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
>
export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >,
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>> | undefined,
  Error,
  Parameters<Backend[Method]>
>
/** Wrap a backend method call in a React Query Mutation. */
export function backendMutationOptions<Method extends MutationMethod>(
  backend: Backend | null,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Backend[Method]>>,
      Error,
      Parameters<Backend[Method]>
    >,
    'mutationFn'
  >,
): reactQuery.UseMutationOptions<
  Awaited<ReturnType<Backend[Method]>>,
  Error,
  Parameters<Backend[Method]>
> {
  return {
    ...options,
    mutationKey: [backend?.type, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: (args) => (backend?.[method] as any)?.(...args),
    networkMode: backend?.type === backendModule.BackendType.local ? 'always' : 'online',
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
export interface UserGroupInfoWithUsers extends backendModule.UserGroupInfo {
  readonly users: readonly backendModule.User[]
}

/** A list of user groups, taking into account optimistic state. */
export function useListUserGroupsWithUsers(
  backend: Backend,
): readonly UserGroupInfoWithUsers[] | null {
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  return React.useMemo(() => {
    if (listUserGroupsQuery.data == null || listUsersQuery.data == null) {
      return null
    } else {
      const result = listUserGroupsQuery.data.map((userGroup) => {
        const usersInGroup: readonly backendModule.User[] = listUsersQuery.data.filter((user) =>
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
  readonly sentMb: number
  readonly totalMb: number
}

/**
 * Options for {@link useUploadFileMutation}.
 */
export interface UploadFileMutationOptions {
  /** Defaults to 3. */
  readonly chunkRetries?: number
  /** Called before any mutations are sent. */
  readonly onBegin?: (progress: UploadFileMutationProgress) => void
  /** Called after each successful chunk upload mutation. */
  readonly onChunkSuccess?: (progress: UploadFileMutationProgress) => void
  /** Called after the entire mutation succeeds. */
  readonly onSuccess?: (progress: UploadFileMutationProgress) => void
  /** Called after any mutations fail. */
  readonly onError?: (error: unknown) => void
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
  const toastId = React.useId()
  const { getText } = useText()
  const toastAndLog = useToastAndLogWithId()
  const { onBegin, onChunkSuccess, onSuccess, onError } = options
  return useUploadFileMutation(backend, {
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
        autoClose: TOAST_SUCCESS_AUTO_CLOSE_MS,
      })
    },
    onError: (error) => {
      onError?.(error)
      toastAndLog(toastId, 'uploadLargeFileError', error)
    },
  })
}

/**
 * Call "upload file" mutations for a file.
 * Always uses multipart upload for Cloud backend.
 */
export function useUploadFileMutation(backend: Backend, options: UploadFileMutationOptions = {}) {
  const toastAndLog = useToastAndLog()
  const {
    chunkRetries = 3,
    onError = (error) => {
      toastAndLog('uploadLargeFileError', error)
    },
  } = options
  const uploadFileMutation = reactQuery.useMutation(backendMutationOptions(backend, 'uploadFile'))
  const uploadFileStartMutation = reactQuery.useMutation(
    backendMutationOptions(backend, 'uploadFileStart'),
  )
  const uploadFileChunkMutation = reactQuery.useMutation({
    ...backendMutationOptions(backend, 'uploadFileChunk'),
    retry: chunkRetries,
  })
  const uploadFileEndMutation = reactQuery.useMutation(
    backendMutationOptions(backend, 'uploadFileEnd'),
  )
  const [variables, setVariables] =
    React.useState<[params: backendModule.UploadFileRequestParams, file: Blob]>()
  const mutateAsync = useEventCallback(
    async (params: backendModule.UploadFileRequestParams, file: Blob) => {
      setVariables([params, file])
      if (backend.type === backendModule.BackendType.local) {
        return await uploadFileMutation.mutateAsync([params, file])
      } else {
        const fileSizeMb = Math.ceil(file.size / MB_BYTES)
        options.onBegin?.({ sentMb: 0, totalMb: fileSizeMb })
        try {
          const { sourcePath, uploadId, presignedUrls } = await uploadFileStartMutation.mutateAsync(
            [{ fileName: params.fileName }, file],
          )
          const parts: backendModule.S3MultipartPart[] = []
          for (const [url, i] of Array.from(
            presignedUrls,
            (presignedUrl, index) => [presignedUrl, index] as const,
          )) {
            parts.push(await uploadFileChunkMutation.mutateAsync([url, file, i]))
            options.onChunkSuccess?.({
              sentMb: Math.min((i + 1) * S3_CHUNK_SIZE_MB, fileSizeMb),
              totalMb: fileSizeMb,
            })
          }
          const result = await uploadFileEndMutation.mutateAsync([
            {
              parentDirectoryId: params.parentDirectoryId,
              parts,
              sourcePath: sourcePath,
              uploadId: uploadId,
              assetId: params.fileId,
              fileName: params.fileName,
            },
          ])
          options.onSuccess?.({ sentMb: fileSizeMb, totalMb: fileSizeMb })
          return result
        } catch (error) {
          onError(error)
          throw error
        }
      }
    },
  )
  const mutate = useEventCallback((params: backendModule.UploadFileRequestParams, file: Blob) => {
    void mutateAsync(params, file)
  })

  return {
    variables,
    mutate,
    mutateAsync,
    context: uploadFileEndMutation.context ?? uploadFileMutation.context,
    data: uploadFileEndMutation.data ?? uploadFileMutation.data,
    failureCount:
      uploadFileEndMutation.failureCount +
      uploadFileChunkMutation.failureCount +
      uploadFileStartMutation.failureCount +
      uploadFileMutation.failureCount,
    failureReason:
      uploadFileEndMutation.failureReason ??
      uploadFileChunkMutation.failureReason ??
      uploadFileStartMutation.failureReason ??
      uploadFileMutation.failureReason,
    isError:
      uploadFileMutation.isError ||
      uploadFileStartMutation.isError ||
      uploadFileChunkMutation.isError ||
      uploadFileEndMutation.isError,
    error:
      uploadFileEndMutation.error ??
      uploadFileChunkMutation.error ??
      uploadFileStartMutation.error ??
      uploadFileMutation.error,
    isPaused:
      uploadFileMutation.isPaused ||
      uploadFileStartMutation.isPaused ||
      uploadFileChunkMutation.isPaused ||
      uploadFileEndMutation.isPaused,
    isPending:
      uploadFileMutation.isPending ||
      uploadFileStartMutation.isPending ||
      uploadFileChunkMutation.isPending ||
      uploadFileEndMutation.isPending,
    isSuccess: uploadFileMutation.isSuccess || uploadFileEndMutation.isSuccess,
  }
}
