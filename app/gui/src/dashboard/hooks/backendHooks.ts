/** @file Hooks for interacting with the backend. */
import { useId, useMemo, useState } from 'react'

import {
  queryOptions,
  useMutation,
  useMutationState,
  useQuery,
  useQueryClient,
  useSuspenseQuery,
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
import { useOpenProject } from '#/hooks/projectHooks'
import { useToastAndLog, useToastAndLogWithId } from '#/hooks/toastAndLogHooks'
import { CATEGORY_TO_FILTER_BY, type Category } from '#/layouts/CategorySwitcher/Category'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import {
  AssetType,
  BackendType,
  DirectoryId,
  type AnyAsset,
  type AssetId,
  type DirectoryAsset,
  type User,
  type UserGroupInfo,
} from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import { TEAMS_DIRECTORY_ID, USERS_DIRECTORY_ID } from '#/services/remoteBackendPaths'
import { tryCreateOwnerPermission } from '#/utilities/permissions'
import { usePreventNavigation } from '#/utilities/preventNavigation'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'

// The number of bytes in 1 megabyte.
const MB_BYTES = 1_000_000
const S3_CHUNK_SIZE_MB = Math.round(backendModule.S3_CHUNK_SIZE_BYTES / MB_BYTES)

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
): UseQueryOptions<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Backend[Method]>> | undefined
>
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
): UseQueryResult<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Backend[Method]>> | undefined
>
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

/** The type of directory listings in the React Query cache. */
type DirectoryQuery = readonly AnyAsset<AssetType>[] | undefined

/** Data for a specific asset. */
export function useAssetPassiveListener(
  backendType: BackendType,
  assetId: AssetId | null | undefined,
  parentId: DirectoryId | null | undefined,
  category: Category,
) {
  const listDirectoryQuery = useQuery<DirectoryQuery>({
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

/** Get the root directory ID given the current backend and category. */
export function useRootDirectoryId(backend: Backend, category: Category) {
  const { user } = useFullUserSession()
  const { data: organization } = useSuspenseQuery({
    queryKey: [backend.type, 'getOrganization'],
    queryFn: () => backend.getOrganization(),
  })
  const [localRootDirectory] = useLocalStorageState('localRootDirectory')

  return useMemo(() => {
    const localRootPath = localRootDirectory != null ? backendModule.Path(localRootDirectory) : null
    const id =
      'homeDirectoryId' in category ?
        category.homeDirectoryId
      : backend.rootDirectoryId(user, organization, localRootPath)
    invariant(id, 'Missing root directory')
    return id
  }, [category, backend, user, organization, localRootDirectory])
}

/** A function to optimistically insert assets into the React Query cache listing for a folder. */
function useInsertAssets(backend: Backend, category: Category) {
  const queryClient = useQueryClient()
  const rootDirectoryId = useRootDirectoryId(backend, category)

  return useEventCallback((assets: readonly AnyAsset[], parentId: DirectoryId | null) => {
    const actualParentId = parentId ?? rootDirectoryId

    const listDirectoryQuery = queryClient.getQueryCache().find<DirectoryQuery>({
      queryKey: [backend.type, 'listDirectory', actualParentId],
      exact: false,
    })

    if (listDirectoryQuery?.state.data) {
      listDirectoryQuery.setData([...listDirectoryQuery.state.data, ...assets])
    }
  })
}

/** Return query data for the children of a directory, fetching it if it does not exist. */
function useEnsureListDirectory(backend: Backend, category: Category) {
  const queryClient = useQueryClient()
  return useEventCallback(async (parentId: DirectoryId) => {
    return await queryClient.ensureQueryData(
      backendQueryOptions(backend, 'listDirectory', [
        {
          parentId,
          labels: null,
          filterBy: CATEGORY_TO_FILTER_BY[category.type],
          recentProjects: category.type === 'recent',
        },
        '(unknown)',
      ]),
    )
  })
}

/** A function to create a new folder. */
export function useNewFolder(backend: Backend, category: Category) {
  const insertAssets = useInsertAssets(backend, category)
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const { user } = useFullUserSession()
  const { data: users } = useBackendQuery(backend, 'listUsers', [])
  const { data: userGroups } = useBackendQuery(backend, 'listUserGroups', [])
  const createDirectoryMutation = useMutation(backendMutationOptions(backend, 'createDirectory'))

  return useEventCallback(async (parentId: DirectoryId, parentPath: string | null | undefined) => {
    const siblings = await ensureListDirectory(parentId)
    const directoryIndices = siblings
      .filter(backendModule.assetIsDirectory)
      .map((item) => /^New Folder (?<directoryIndex>\d+)$/.exec(item.title))
      .map((match) => match?.groups?.directoryIndex)
      .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
    const title = `New Folder ${Math.max(0, ...directoryIndices) + 1}`
    const placeholderItem: DirectoryAsset = {
      type: AssetType.directory,
      id: DirectoryId(uniqueString()),
      title,
      modifiedAt: toRfc3339(new Date()),
      parentId,
      permissions: tryCreateOwnerPermission(
        `${parentPath ?? ''}/${title}`,
        category,
        user,
        users ?? [],
        userGroups ?? [],
      ),
      projectState: null,
      labels: [],
      description: null,
    }

    insertAssets([placeholderItem], parentId)

    return await createDirectoryMutation.mutateAsync([
      { parentId: placeholderItem.parentId, title: placeholderItem.title },
    ])
  })
}

/**
 * Remove an asset from the React Query cache. Should only be called on
 * optimistically inserted assets.
 */
function useDeleteAsset(backend: Backend, category: Category) {
  const queryClient = useQueryClient()
  const ensureListDirectory = useEnsureListDirectory(backend, category)

  return useEventCallback(async (assetId: AssetId, parentId: DirectoryId) => {
    const siblings = await ensureListDirectory(parentId)
    const asset = siblings.find((sibling) => sibling.id === assetId)
    if (!asset) return

    const listDirectoryQuery = queryClient.getQueryCache().find<DirectoryQuery>({
      queryKey: [
        backend.type,
        'listDirectory',
        parentId,
        {
          labels: null,
          filterBy: CATEGORY_TO_FILTER_BY[category.type],
          recentProjects: category.type === 'recent',
        },
      ],
    })

    if (listDirectoryQuery?.state.data) {
      listDirectoryQuery.setData(
        listDirectoryQuery.state.data.filter((child) => child.id !== assetId),
      )
    }
  })
}

/** A function to create a new project. */
export function useNewProject(backend: Backend, category: Category) {
  const insertAssets = useInsertAssets(backend, category)
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const toastAndLog = useToastAndLog()
  const doOpenProject = useOpenProject()
  const deleteAsset = useDeleteAsset(backend, category)
  const { user } = useFullUserSession()
  const { data: users } = useBackendQuery(backend, 'listUsers', [])
  const { data: userGroups } = useBackendQuery(backend, 'listUserGroups', [])
  const createProjectMutation = useMutation(backendMutationOptions(backend, 'createProject'))

  return useEventCallback(
    async (
      {
        templateName,
        templateId,
        datalinkId,
      }: {
        templateName: string | null | undefined
        templateId?: string | null | undefined
        datalinkId?: backendModule.DatalinkId | null | undefined
      },
      parentId: DirectoryId,
      parentPath: string | null | undefined,
    ) => {
      const siblings = await ensureListDirectory(parentId)
      const projectName = (() => {
        const prefix = `${templateName ?? 'New Project'} `
        const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
        const projectIndices = siblings
          .filter(backendModule.assetIsProject)
          .map((item) => projectNameTemplate.exec(item.title)?.groups?.projectIndex)
          .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        return `${prefix}${Math.max(0, ...projectIndices) + 1}`
      })()
      const dummyId = backendModule.ProjectId(uniqueString())
      const path = backend instanceof LocalBackend ? backend.joinPath(parentId, projectName) : null

      const placeholderItem: backendModule.ProjectAsset = {
        type: AssetType.project,
        id: dummyId,
        title: projectName,
        modifiedAt: toRfc3339(new Date()),
        parentId,
        permissions: tryCreateOwnerPermission(
          `${parentPath ?? ''}/${projectName}`,
          category,
          user,
          users ?? [],
          userGroups ?? [],
        ),
        projectState: {
          type: backendModule.ProjectState.placeholder,
          volumeId: '',
          openedBy: user.email,
          ...(path != null ? { path } : {}),
        },
        labels: [],
        description: null,
      }

      insertAssets([placeholderItem], parentId)

      return await createProjectMutation
        .mutateAsync([
          {
            parentDirectoryId: placeholderItem.parentId,
            projectName: placeholderItem.title,
            ...(templateId == null ? {} : { projectTemplateName: templateId }),
            ...(datalinkId == null ? {} : { datalinkId: datalinkId }),
          },
        ])
        .catch((error) => {
          void deleteAsset(placeholderItem.id, parentId)
          toastAndLog('createProjectError', error)
          throw error
        })
        .then((createdProject) => {
          doOpenProject({
            id: createdProject.projectId,
            type: backend.type,
            parentId: placeholderItem.parentId,
            title: placeholderItem.title,
          })
          return createdProject
        })
    },
  )
}

// FIXME: `doToggleDirectoryExpansion` via zustand state

/** A function to create a new secret. */
export function useNewSecret(backend: Backend, category: Category) {
  const insertAssets = useInsertAssets(backend, category)
  const { user } = useFullUserSession()
  const { data: users } = useBackendQuery(backend, 'listUsers', [])
  const { data: userGroups } = useBackendQuery(backend, 'listUserGroups', [])
  const createSecretMutation = useMutation(backendMutationOptions(backend, 'createSecret'))

  return useEventCallback(
    async (
      name: string,
      value: string,
      parentId: DirectoryId,
      parentPath: string | null | undefined,
    ) => {
      const placeholderItem: backendModule.SecretAsset = {
        type: AssetType.secret,
        id: backendModule.SecretId(uniqueString()),
        title: name,
        modifiedAt: toRfc3339(new Date()),
        parentId,
        permissions: tryCreateOwnerPermission(
          `${parentPath ?? ''}/${name}`,
          category,
          user,
          users ?? [],
          userGroups ?? [],
        ),
        projectState: null,
        labels: [],
        description: null,
      }

      insertAssets([placeholderItem], parentId)

      return await createSecretMutation.mutateAsync([
        {
          parentDirectoryId: placeholderItem.parentId,
          name: placeholderItem.title,
          value: value,
        },
      ])
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
