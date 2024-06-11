/** @file Hooks for interacting with the backend. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import * as store from '#/store'

import * as backendMutationListeners from '#/hooks/backendHooks/backendMutationListeners'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import DuplicateAssetsModal from '#/modals/DuplicateAssetsModal'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import LocalBackend, * as localBackendModule from '#/services/LocalBackend'

import * as dateTime from '#/utilities/dateTime'
import * as download from '#/utilities/download'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as uniqueString from '#/utilities/uniqueString'

// FIXME: Listeners and optimistic state for duplicateProjectMutation, copyAssetMutation,
// updateAssetMutation (e.g. move), and associateTagMutation

// FIXME: Keep track of the current project to open somewhere, and open project when ready when
// the corresponding query returns state: Opened

// ===============================
// === DefineBackendMethodKeys ===
// ===============================

/** Ensure that the given type contains only names of backend methods. */
// eslint-disable-next-line no-restricted-syntax
type DefineBackendMethods<T extends keyof Backend> = T

// =======================
// === MutationMethods ===
// =======================

/** Names of methods corresponding to mutations. */
export type MutationMethodInternal = DefineBackendMethods<
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
  | 'undoDeleteAsset'
  | 'updateAsset'
  | 'updateDirectory'
  | 'updateFile'
  | 'updateOrganization'
  | 'updateProject'
  | 'updateSecret'
  | 'updateUser'
  | 'uploadFile'
  | 'uploadOrganizationPicture'
  | 'uploadUserPicture'
  | 'waitUntilProjectIsReady'
>

/** Names of methods corresponding to mutations.
 *
 * Some methods have been omitted:
 * - `uploadFile` - use `useBackendUploadFiles` instead
 * - `createProject` - use `useBackendCreateProject` instead */
export type MutationMethod = Exclude<
  MutationMethodInternal,
  DefineBackendMethods<'createProject' | 'uploadFile'>
>

// ===================
// === QueryMethod ===
// ===================

/** Names of methods corresponding to queries. */
export type QueryMethod = DefineBackendMethods<
  | 'checkResources'
  | 'getCheckoutSession'
  | 'getDatalink'
  | 'getFileContent'
  | 'getFileDetails'
  | 'getLogEvents'
  | 'getOrganization'
  | 'getProjectDetails'
  | 'getSecret'
  | 'listAssetVersions'
  | 'listDirectory'
  | 'listFiles'
  | 'listInvitations'
  | 'listProjects'
  | 'listSecrets'
  | 'listTags'
  | 'listUserGroups'
  | 'listUsers'
  | 'listVersions'
  | 'usersMe'
>

// ============================
// === revokeUserPictureUrl ===
// ============================

const USER_PICTURE_URL_REVOKERS = new WeakMap<Backend, () => void>()

/** Create the corresponding "user picture" URL for the given backend. */
function createUserPictureUrl(backend: Backend | null, picture: Blob) {
  if (backend != null) {
    USER_PICTURE_URL_REVOKERS.get(backend)?.()
    const url = URL.createObjectURL(picture)
    USER_PICTURE_URL_REVOKERS.set(backend, () => {
      URL.revokeObjectURL(url)
    })
    return url
  } else {
    // This should never happen, so use an arbitrary URL.
    return location.href
  }
}

/** Revoke the corresponding "user picture" URL for the given backend. */
export function revokeUserPictureUrl(backend: Backend | null) {
  if (backend != null) {
    USER_PICTURE_URL_REVOKERS.get(backend)?.()
  }
}

// ====================================
// === revokeOrganizationPictureUrl ===
// ====================================

const ORGANIZATION_PICTURE_URL_REVOKERS = new WeakMap<Backend, () => void>()

/** Create the corresponding "organization picture" URL for the given backend. */
function createOrganizationPictureUrl(backend: Backend | null, picture: Blob) {
  if (backend != null) {
    ORGANIZATION_PICTURE_URL_REVOKERS.get(backend)?.()
    const url = URL.createObjectURL(picture)
    ORGANIZATION_PICTURE_URL_REVOKERS.set(backend, () => {
      URL.revokeObjectURL(url)
    })
    return url
  } else {
    // This should never happen, so use an arbitrary URL.
    return location.href
  }
}

/** Revoke the corresponding "organization picture" URL for the given backend. */
export function revokeOrganizationPictureUrl(backend: Backend | null) {
  if (backend != null) {
    ORGANIZATION_PICTURE_URL_REVOKERS.get(backend)?.()
  }
}

// =======================
// === useBackendQuery ===
// =======================

/** Options for {@link useBackendQuery}. */
interface UseBackendQueryOptions<
  TQueryFnData = unknown,
  TError = reactQuery.DefaultError,
  TData = TQueryFnData,
  TQueryKey extends reactQuery.QueryKey = reactQuery.QueryKey,
> extends Omit<
      reactQuery.UseQueryOptions<TQueryFnData, TError, TData, TQueryKey>,
      'queryFn' | 'queryKey'
    >,
    Partial<Pick<reactQuery.UseQueryOptions<TQueryFnData, TError, TData, TQueryKey>, 'queryKey'>> {}

export function useBackendQuery<Method extends QueryMethod>(
  backend: Backend,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: UseBackendQueryOptions<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  >
): reactQuery.UseQueryResult<
  Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
>
export function useBackendQuery<Method extends QueryMethod>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: UseBackendQueryOptions<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  >
): reactQuery.UseQueryResult<
  // eslint-disable-next-line no-restricted-syntax
  Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>> | undefined
>
/** Wrap a backend method call in a React Query. */
export function useBackendQuery<Method extends QueryMethod>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: UseBackendQueryOptions<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  >
) {
  return reactQuery.useQuery<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  >({
    ...options,
    queryKey: [backend, method, ...args, ...(options?.queryKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

// ==============================
// === invalidateBackendQuery ===
// ==============================

/** Wrap a backend method call in a React Query. */
export function invalidateBackendQuery<Method extends QueryMethod>(
  queryClient: reactQuery.QueryClient,
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Pick<
    reactQuery.EnsureQueryDataOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >,
    'queryKey'
  >
) {
  return queryClient.invalidateQueries({
    queryKey: [backend, method, ...args, ...(options?.queryKey ?? [])],
  })
}

// ===========================
// === getBackendQueryData ===
// ===========================

/** Get cached data for a backend query, or `undefined` if no data was cached. */
export function getBackendQueryData<Method extends QueryMethod>(
  queryClient: reactQuery.QueryClient,
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Pick<
    reactQuery.EnsureQueryDataOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >,
    'queryKey'
  >
) {
  return queryClient.getQueryData<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  >([backend, method, ...args, ...(options?.queryKey ?? [])])
}

// ==============================
// === ensureBackendQueryData ===
// ==============================

export function ensureBackendQueryData<Method extends QueryMethod>(
  queryClient: reactQuery.QueryClient,
  backend: Backend,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Omit<
    reactQuery.EnsureQueryDataOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >,
    'queryFn'
  >
): Promise<Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>>
export function ensureBackendQueryData<Method extends QueryMethod>(
  queryClient: reactQuery.QueryClient,
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Omit<
    reactQuery.EnsureQueryDataOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >,
    'queryFn'
  >
  // eslint-disable-next-line no-restricted-syntax
): Promise<Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>> | undefined>
/** Get cached data for a backend query, or fetch the data if it is not yet cached. */
export function ensureBackendQueryData<Method extends QueryMethod>(
  queryClient: reactQuery.QueryClient,
  backend: Backend | null,
  method: Method,
  args: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  options?: Omit<
    reactQuery.EnsureQueryDataOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >,
    'queryFn'
  >
) {
  return queryClient.ensureQueryData<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  >({
    ...options,
    queryKey: [backend, method, ...args, ...(options?.queryKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    queryFn: () => (backend?.[method] as any)?.(...args),
  })
}

// ==========================
// === useBackendMutation ===
// ==========================

/** Wrap a backend method call in a React Query Mutation.
 * @deprecated This is an internal function. */
export function useBackendMutationInternal<Method extends MutationMethodInternal>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
      unknown
    >,
    'mutationFn'
  >
) {
  const queryClient = reactQuery.useQueryClient()
  const session = authProvider.useNonPartialUserSession()
  return reactQuery.useMutation<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
    Error,
    Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
    unknown
  >({
    ...options,
    mutationKey: [backend, method, ...(options?.mutationKey ?? [])],
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-return
    mutationFn: args => (backend[method] as any)(...args),
    onSuccess: (data, variables, context) => {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax
      const listener = backendMutationListeners.BACKEND_MUTATION_LISTENERS[
        // eslint-disable-next-line no-restricted-syntax
        method as keyof typeof backendMutationListeners.BACKEND_MUTATION_LISTENERS
      ] as backendMutationListeners.MutationListener<Method> | null
      listener?.(
        data,
        variables,
        (queryMethod, key, updater) => {
          queryClient.setQueryData<
            Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
            // @ts-expect-error This code is correct - it is just too dynamic for TypeScript to
            // typecheck.
          >([backend, queryMethod, ...key], queryData =>
            // eslint-disable-next-line no-restricted-syntax
            queryData == null ? queryData : updater(queryData as never)
          )
        },
        backend,
        session
      )
      options?.onSuccess?.(data, variables, context)
    },
  })
}

/** Wrap a backend method call in a React Query Mutation. */
// eslint-disable-next-line no-restricted-syntax
export const useBackendMutation: <Method extends MutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
      unknown
    >,
    'mutationFn'
  >
) => ReturnType<typeof useBackendMutationInternal<Method>> = useBackendMutationInternal

// ===================================
// === useBackendMutationVariables ===
// ===================================

/** Access mutation variables from a React Query Mutation. */
export function useBackendMutationVariables<Method extends MutationMethodInternal>(
  backend: Backend | null,
  method: Method,
  mutationKey?: readonly unknown[]
) {
  return reactQuery.useMutationState<
    Parameters<Extract<Backend[Method], (...args: never) => unknown>>
  >({
    filters: {
      mutationKey: [backend, method, ...(mutationKey ?? [])],
      status: 'pending',
    },
    // eslint-disable-next-line no-restricted-syntax
    select: mutation => mutation.state.variables as never,
  })
}

// ===================================
// === getBackendMutationVariables ===
// ===================================

/** Access mutation variables from a React Query Mutation. */
export function getBackendMutationVariables<Method extends MutationMethodInternal>(
  queryClient: reactQuery.QueryClient,
  backend: Backend | null,
  method: Method,
  mutationKey?: readonly unknown[]
) {
  // This is SAFE - this is a type error because `findAll` is not generic.
  // eslint-disable-next-line no-restricted-syntax
  const mutations = queryClient.getMutationCache().findAll({
    mutationKey: [backend, method, ...(mutationKey ?? [])],
    status: 'pending',
  }) as readonly reactQuery.Mutation<
    Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
    Error,
    Parameters<Extract<Backend[Method], (...args: never) => unknown>>
  >[]
  return mutations.flatMap(mutation => (mutation.state.variables ? [mutation.state.variables] : []))
}

// =======================================
// === useBackendMutationWithVariables ===
// =======================================

/** Wrap a backend method call in a React Query Mutation, and access its variables. */
export function useBackendMutationWithVariables<Method extends MutationMethod>(
  backend: Backend,
  method: Method,
  options?: Omit<
    reactQuery.UseMutationOptions<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
      Error,
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
      unknown
    >,
    'mutationFn'
  >
) {
  const mutation = useBackendMutation(backend, method, options)
  return {
    mutation,
    mutate: mutation.mutate,
    mutateAsync: mutation.mutateAsync,
    variables: useBackendMutationVariables(backend, method, options?.mutationKey),
  }
}

// ===================
// === Placeholder ===
// ===================

/** An object with a `isPlaceholder` property. */
interface Placeholder {
  readonly isPlaceholder: boolean
}

// =======================
// === WithPlaceholder ===
// =======================

/** An existing type, with an added `isPlaceholder` property. */
export type WithPlaceholder<T extends object> = Placeholder & T

// ========================
// === toNonPlaceholder ===
// ========================

/** Return an object with an additional field `isPlaceholder: false`. */
function toNonPlaceholder<T extends object>(value: T) {
  return { ...value, isPlaceholder: false }
}

// ===========================
// === useBackendListUsers ===
// ===========================

/** A list of users, taking into account optimistic state. */
export function useBackendListUsers(
  backend: Backend
): readonly WithPlaceholder<backendModule.User>[] | null {
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  const changeUserGroupVariables = useBackendMutationVariables(backend, 'changeUserGroup')
  return React.useMemo(() => {
    if (listUsersQuery.data == null) {
      return null
    } else {
      const result = listUsersQuery.data.map(toNonPlaceholder)
      const userIdToIndex = new Map(result.map((user, i) => [user.userId, i]))
      for (const [userId, body] of changeUserGroupVariables) {
        const index = userIdToIndex.get(userId)
        const user = index == null ? null : result[index]
        if (index != null && user != null) {
          result[index] = { ...user, userGroups: body.userGroups }
        }
      }
      return result
    }
  }, [changeUserGroupVariables, listUsersQuery.data])
}

// ================================
// === useBackendListUserGroups ===
// ================================

/** A list of user groups, taking into account optimistic state. */
export function useBackendListUserGroups(
  backend: Backend
): readonly WithPlaceholder<backendModule.UserGroupInfo>[] | null {
  const { user } = authProvider.useNonPartialUserSession()
  invariant(user != null, 'User must exist for user groups to be listed.')
  const listUserGroupsQuery = useBackendQuery(backend, 'listUserGroups', [])
  const createUserGroupVariables = useBackendMutationVariables(backend, 'createUserGroup')
  const deleteUserGroupVariables = useBackendMutationVariables(backend, 'deleteUserGroup')
  return React.useMemo(() => {
    if (listUserGroupsQuery.data == null) {
      return null
    } else {
      const deletedUserGroupIds = new Set(deleteUserGroupVariables.map(([id]) => id))
      const userGroupsBase = listUserGroupsQuery.data
        .filter(userGroup => !deletedUserGroupIds.has(userGroup.id))
        .map(toNonPlaceholder)
      return [
        ...createUserGroupVariables.map(([body]) => ({
          organizationId: user.organizationId,
          id: backendModule.newPlaceholderUserGroupId(),
          groupName: body.name,
          isPlaceholder: true,
        })),
        ...userGroupsBase,
      ]
    }
  }, [
    user.organizationId,
    createUserGroupVariables,
    deleteUserGroupVariables,
    listUserGroupsQuery.data,
  ])
}

// =========================================
// === useBackendListUserGroupsWithUsers ===
// =========================================

/** A user group, as well as the users that are a part of the user group. */
export interface UserGroupInfoWithUsers extends backendModule.UserGroupInfo {
  readonly users: readonly WithPlaceholder<backendModule.User>[]
}

/** A list of user groups, taking into account optimistic state. */
export function useBackendListUserGroupsWithUsers(
  backend: Backend
): readonly WithPlaceholder<UserGroupInfoWithUsers>[] | null {
  const userGroupsRaw = useBackendListUserGroups(backend)
  // Old user list
  const listUsersQuery = useBackendQuery(backend, 'listUsers', [])
  // Current user list, including optimistic updates
  const users = useBackendListUsers(backend)
  return React.useMemo(() => {
    if (userGroupsRaw == null || listUsersQuery.data == null || users == null) {
      return null
    } else {
      const currentUserGroupsById = new Map(
        listUsersQuery.data.map(user => [user.userId, new Set(user.userGroups)])
      )
      const result = userGroupsRaw.map(userGroup => {
        const usersInGroup: readonly WithPlaceholder<backendModule.User>[] = users
          .filter(user => user.userGroups?.includes(userGroup.id))
          .map(user => {
            if (currentUserGroupsById.get(user.userId)?.has(userGroup.id) !== true) {
              return { ...user, isPlaceholder: true }
            } else {
              return user
            }
          })
        return { ...userGroup, users: usersInGroup }
      })
      return result
    }
  }, [listUsersQuery.data, userGroupsRaw, users])
}

// ==========================
// === useBackendListTags ===
// ==========================

/** A list of asset tags, taking into account optimistic state. */
export function useBackendListTags(
  backend: Backend | null
): readonly WithPlaceholder<backendModule.Label>[] | null {
  const listTagsQuery = useBackendQuery(backend, 'listTags', [])
  const createTagVariables = useBackendMutationVariables(backend, 'createTag')
  const deleteTagVariables = useBackendMutationVariables(backend, 'deleteTag')
  return React.useMemo(() => {
    if (listTagsQuery.data == null) {
      return null
    } else {
      const deletedTags = new Set(deleteTagVariables.map(variables => variables[0]))
      const result = listTagsQuery.data
        .filter(tag => !deletedTags.has(tag.id))
        .map(toNonPlaceholder)
      return [
        ...result,
        ...createTagVariables.map(variables => ({
          id: backendModule.TagId(`tag-${uniqueString.uniqueString()}`),
          value: backendModule.LabelName(variables[0].value),
          color: variables[0].color,
          isPlaceholder: true,
        })),
      ]
    }
  }, [createTagVariables, deleteTagVariables, listTagsQuery.data])
}

// =========================
// === useBackendUsersMe ===
// =========================

/** The current user, taking into account optimistic state. */
export function useBackendUsersMe(backend: Backend | null) {
  const usersMeQuery = useBackendQuery(backend, 'usersMe', [])
  const updateUserVariables = useBackendMutationVariables(backend, 'updateUser')
  const uploadUserPictureVariables = useBackendMutationVariables(backend, 'uploadUserPicture')
  return React.useMemo(() => {
    if (usersMeQuery.data == null) {
      return null
    } else {
      let result = usersMeQuery.data
      for (const [{ username }] of updateUserVariables) {
        if (username != null) {
          result = { ...result, name: username }
        }
      }
      for (const [, file] of uploadUserPictureVariables) {
        result = {
          ...result,
          profilePicture: backendModule.HttpsUrl(createUserPictureUrl(backend, file)),
        }
      }
      return result
    }
  }, [backend, usersMeQuery.data, updateUserVariables, uploadUserPictureVariables])
}

// =================================
// === useBackendGetOrganization ===
// =================================

/** The current user's organization, taking into account optimistic state. */
export function useBackendGetOrganization(backend: Backend | null) {
  const getOrganizationQuery = useBackendQuery(backend, 'getOrganization', [])
  const updateOrganizationVariables = useBackendMutationVariables(backend, 'updateOrganization')
  const uploadOrganizationPictureVariables = useBackendMutationVariables(
    backend,
    'uploadOrganizationPicture'
  )
  return React.useMemo(() => {
    if (getOrganizationQuery.data == null) {
      return null
    } else {
      let result = getOrganizationQuery.data
      for (const [update] of updateOrganizationVariables) {
        result = { ...result, ...update }
      }
      for (const [, file] of uploadOrganizationPictureVariables) {
        result = {
          ...result,
          picture: backendModule.HttpsUrl(createOrganizationPictureUrl(backend, file)),
        }
      }
      return result
    }
  }, [
    backend,
    getOrganizationQuery.data,
    updateOrganizationVariables,
    uploadOrganizationPictureVariables,
  ])
}

// ===============================
// === useBackendListDirectory ===
// ===============================

/** A list of assets in the directory with the given ID, taking into account optimistic state. */
export function useBackendListDirectory(
  backend: Backend,
  directoryId: backendModule.DirectoryId,
  filterBy = backendModule.FilterBy.active
) {
  const { user } = authProvider.useNonPartialUserSession()
  const listDirectoryQuery = useBackendQuery(
    backend,
    'listDirectory',
    [
      {
        filterBy,
        labels: null,
        parentId: directoryId,
        recentProjects: false,
      },
    ],
    {
      queryKey: [directoryId, filterBy],
    }
  )
  const createDirectoryVariables = useBackendMutationVariables(backend, 'createDirectory')
  const createProjectVariables = useBackendMutationVariables(backend, 'createProject')
  const createDatalinkVariables = useBackendMutationVariables(backend, 'createDatalink')
  const createSecretVariables = useBackendMutationVariables(backend, 'createSecret')
  const uploadFileVariables = useBackendMutationVariables(backend, 'uploadFile')
  return React.useMemo(() => {
    if (listDirectoryQuery.data == null) {
      return null
    } else {
      const result = listDirectoryQuery.data.map(toNonPlaceholder)
      const placeholderProjectState = { type: backendModule.ProjectState.new, volumeId: '' }
      const createAssetObject = <T extends Partial<backendModule.AnyAsset>>(rest: T) => ({
        description: null,
        labels: [],
        permissions: permissions.tryGetSingletonOwnerPermission(user),
        projectState: null,
        modifiedAt: dateTime.toRfc3339(new Date()),
        parentId: directoryId,
        isPlaceholder: true,
        ...rest,
      })
      for (const [body] of createDirectoryVariables) {
        result.push(
          createAssetObject({
            type: backendModule.AssetType.directory,
            id: backendModule.DirectoryId(
              `${backendModule.AssetType.directory}-${uniqueString.uniqueString()}`
            ),
            title: body.title,
          })
        )
      }
      for (const [body] of createProjectVariables) {
        result.push(
          createAssetObject({
            type: backendModule.AssetType.project,
            id: backendModule.ProjectId(
              `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
            ),
            title: body.projectName,
            projectState: placeholderProjectState,
          })
        )
      }
      for (const [body] of createDatalinkVariables) {
        if (body.datalinkId == null) {
          result.push(
            createAssetObject({
              type: backendModule.AssetType.datalink,
              id: backendModule.DatalinkId(
                `${backendModule.AssetType.datalink}-${uniqueString.uniqueString()}`
              ),
              title: body.name,
            })
          )
        }
      }
      for (const [body] of createSecretVariables) {
        result.push(
          createAssetObject({
            type: backendModule.AssetType.secret,
            id: backendModule.SecretId(
              `${backendModule.AssetType.secret}-${uniqueString.uniqueString()}`
            ),
            title: body.name,
          })
        )
      }
      for (const [body] of uploadFileVariables) {
        const projectNameAndExtension = backendModule.extractProjectExtension(body.fileName)
        const projectName =
          projectNameAndExtension.extension === '' ? null : projectNameAndExtension.basename
        result.push(
          projectName == null
            ? createAssetObject({
                type: backendModule.AssetType.file,
                id: backendModule.FileId(
                  `${backendModule.AssetType.file}-${uniqueString.uniqueString()}`
                ),
                title: body.fileName,
              })
            : createAssetObject({
                type: backendModule.AssetType.project,
                id: backendModule.ProjectId(
                  `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
                ),
                title: projectName,
                projectState: placeholderProjectState,
              })
        )
      }
      return result
    }
  }, [
    directoryId,
    user,
    listDirectoryQuery.data,
    createDatalinkVariables,
    createDirectoryVariables,
    createProjectVariables,
    createSecretVariables,
    uploadFileVariables,
  ])
}

// ===============================
// === getBackendListDirectory ===
// ===============================

/** A list of assets in the directory with the given ID, taking into account optimistic state. */
export function getBackendListDirectory(
  queryClient: reactQuery.QueryClient,
  user: backendModule.User | null,
  backend: Backend,
  directoryId: backendModule.DirectoryId,
  filterBy = backendModule.FilterBy.active
) {
  const listDirectoryData = getBackendQueryData(
    queryClient,
    backend,
    'listDirectory',
    [
      {
        filterBy,
        labels: null,
        parentId: directoryId,
        recentProjects: false,
      },
    ],
    {
      queryKey: [directoryId, filterBy],
    }
  )
  if (listDirectoryData == null) {
    return
  } else {
    queryClient.getMutationCache().findAll({})
    const createDirectoryVariables = getBackendMutationVariables(
      queryClient,
      backend,
      'createDirectory'
    )
    const createProjectVariables = getBackendMutationVariables(
      queryClient,
      backend,
      'createProject'
    )
    const createDatalinkVariables = getBackendMutationVariables(
      queryClient,
      backend,
      'createDatalink'
    )
    const createSecretVariables = getBackendMutationVariables(queryClient, backend, 'createSecret')
    const uploadFileVariables = getBackendMutationVariables(queryClient, backend, 'uploadFile')
    const result = listDirectoryData.map(toNonPlaceholder)
    const placeholderProjectState = { type: backendModule.ProjectState.new, volumeId: '' }
    const createAssetObject = <T extends Partial<backendModule.AnyAsset>>(rest: T) => ({
      description: null,
      labels: [],
      permissions: permissions.tryGetSingletonOwnerPermission(user),
      projectState: null,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: directoryId,
      isPlaceholder: true,
      ...rest,
    })
    for (const [body] of createDirectoryVariables) {
      result.push(
        createAssetObject({
          type: backendModule.AssetType.directory,
          id: backendModule.DirectoryId(
            `${backendModule.AssetType.directory}-${uniqueString.uniqueString()}`
          ),
          title: body.title,
        })
      )
    }
    for (const [body] of createProjectVariables) {
      result.push(
        createAssetObject({
          type: backendModule.AssetType.project,
          id: backendModule.ProjectId(
            `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
          ),
          title: body.projectName,
          projectState: placeholderProjectState,
        })
      )
    }
    for (const [body] of createDatalinkVariables) {
      if (body.datalinkId == null) {
        result.push(
          createAssetObject({
            type: backendModule.AssetType.datalink,
            id: backendModule.DatalinkId(
              `${backendModule.AssetType.datalink}-${uniqueString.uniqueString()}`
            ),
            title: body.name,
          })
        )
      }
    }
    for (const [body] of createSecretVariables) {
      result.push(
        createAssetObject({
          type: backendModule.AssetType.secret,
          id: backendModule.SecretId(
            `${backendModule.AssetType.secret}-${uniqueString.uniqueString()}`
          ),
          title: body.name,
        })
      )
    }
    for (const [body] of uploadFileVariables) {
      const projectNameAndExtension = backendModule.extractProjectExtension(body.fileName)
      const projectName =
        projectNameAndExtension.extension === '' ? null : projectNameAndExtension.basename
      result.push(
        projectName == null
          ? createAssetObject({
              type: backendModule.AssetType.file,
              id: backendModule.FileId(
                `${backendModule.AssetType.file}-${uniqueString.uniqueString()}`
              ),
              title: body.fileName,
            })
          : createAssetObject({
              type: backendModule.AssetType.project,
              id: backendModule.ProjectId(
                `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
              ),
              title: projectName,
              projectState: placeholderProjectState,
            })
      )
    }
    return result
  }
}

// ==================================
// === ensureBackendListDirectory ===
// ==================================

/** A list of assets in the directory with the given ID, taking into account optimistic state. */
export async function ensureBackendListDirectory(
  queryClient: reactQuery.QueryClient,
  user: backendModule.User | null,
  backend: Backend,
  directoryId: backendModule.DirectoryId,
  filterBy = backendModule.FilterBy.active
) {
  const listDirectoryData = await ensureBackendQueryData(
    queryClient,
    backend,
    'listDirectory',
    [
      {
        filterBy,
        labels: null,
        parentId: directoryId,
        recentProjects: false,
      },
    ],
    {
      queryKey: [directoryId, filterBy],
    }
  )
  queryClient.getMutationCache().findAll({})
  const createDirectoryVariables = getBackendMutationVariables(
    queryClient,
    backend,
    'createDirectory'
  )
  const createProjectVariables = getBackendMutationVariables(queryClient, backend, 'createProject')
  const createDatalinkVariables = getBackendMutationVariables(
    queryClient,
    backend,
    'createDatalink'
  )
  const createSecretVariables = getBackendMutationVariables(queryClient, backend, 'createSecret')
  const uploadFileVariables = getBackendMutationVariables(queryClient, backend, 'uploadFile')
  const result = listDirectoryData.map(toNonPlaceholder)
  const placeholderProjectState = { type: backendModule.ProjectState.new, volumeId: '' }
  const createAssetObject = <T extends Partial<backendModule.AnyAsset>>(rest: T) => ({
    description: null,
    labels: [],
    permissions: permissions.tryGetSingletonOwnerPermission(user),
    projectState: null,
    modifiedAt: dateTime.toRfc3339(new Date()),
    parentId: directoryId,
    isPlaceholder: true,
    ...rest,
  })
  for (const [body] of createDirectoryVariables) {
    result.push(
      createAssetObject({
        type: backendModule.AssetType.directory,
        id: backendModule.DirectoryId(
          `${backendModule.AssetType.directory}-${uniqueString.uniqueString()}`
        ),
        title: body.title,
      })
    )
  }
  for (const [body] of createProjectVariables) {
    result.push(
      createAssetObject({
        type: backendModule.AssetType.project,
        id: backendModule.ProjectId(
          `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
        ),
        title: body.projectName,
        projectState: placeholderProjectState,
      })
    )
  }
  for (const [body] of createDatalinkVariables) {
    if (body.datalinkId == null) {
      result.push(
        createAssetObject({
          type: backendModule.AssetType.datalink,
          id: backendModule.DatalinkId(
            `${backendModule.AssetType.datalink}-${uniqueString.uniqueString()}`
          ),
          title: body.name,
        })
      )
    }
  }
  for (const [body] of createSecretVariables) {
    result.push(
      createAssetObject({
        type: backendModule.AssetType.secret,
        id: backendModule.SecretId(
          `${backendModule.AssetType.secret}-${uniqueString.uniqueString()}`
        ),
        title: body.name,
      })
    )
  }
  for (const [body] of uploadFileVariables) {
    const projectNameAndExtension = backendModule.extractProjectExtension(body.fileName)
    const projectName =
      projectNameAndExtension.extension === '' ? null : projectNameAndExtension.basename
    result.push(
      projectName == null
        ? createAssetObject({
            type: backendModule.AssetType.file,
            id: backendModule.FileId(
              `${backendModule.AssetType.file}-${uniqueString.uniqueString()}`
            ),
            title: body.fileName,
          })
        : createAssetObject({
            type: backendModule.AssetType.project,
            id: backendModule.ProjectId(
              `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
            ),
            title: projectName,
            projectState: placeholderProjectState,
          })
    )
  }
  return result
}

// =====================================
// === getBackendAllKnownDirectories ===
// =====================================

/** Return a map containing all directories known by the  */
export function getBackendAllKnownDirectories(
  queryClient: reactQuery.QueryClient,
  user: backendModule.User | null,
  backend: Backend
) {
  const queries = queryClient.getQueriesData({
    queryKey: [backend, 'listDirectory'],
    predicate: query => {
      /* backend, method = 'listDirectory', body */
      const [, , body] = query.queryKey
      return (
        typeof body === 'object' &&
        body != null &&
        'filterBy' in body &&
        body.filterBy === backendModule.FilterBy.active
      )
    },
  })
  return object.fromEntries(
    queries.flatMap(kv => {
      const [queryKey] = kv
      /* backend, method = 'listDirectory', body */
      const [, , body] = queryKey
      const directoryId =
        typeof body === 'object' &&
        body != null &&
        'parentId' in body &&
        typeof body.parentId === 'string'
          ? backendModule.DirectoryId(body.parentId)
          : null
      if (directoryId == null) {
        return []
      } else {
        const contents = getBackendListDirectory(queryClient, user, backend, directoryId)
        return contents == null ? [] : [[directoryId, contents]]
      }
    })
  )
}

// ==========================
// === useBackendGetAsset ===
// ==========================

/** An arbitrary asset, taking into account optimistic state. */
export function useBackendGetAsset<Id extends backendModule.AssetId>(
  backend: Backend,
  id: Id,
  parentId: backendModule.DirectoryId,
  filterBy?: backendModule.FilterBy
) {
  const siblings = useBackendListDirectory(backend, parentId, filterBy)
  // This is SAFE, as the id must be the correct newtype based on the `.find`.
  // eslint-disable-next-line no-restricted-syntax
  return (siblings?.find(sibling => sibling.id === id) ?? null) as WithPlaceholder<
    Extract<backendModule.AnyAsset, Record<'id', Id>>
  > | null
}

// =========================================
// === useBackendCreateDirectoryMutation ===
// =========================================

/** Wrapper over {@link Backend.createDirectory} that renames the folder based on the names of its
 * siblings. */
export function useBackendCreateDirectoryMutation(backend: Backend) {
  const queryClient = reactQuery.useQueryClient()
  const { user } = authProvider.useNonPartialUserSession()
  const createDirectoryMutation = useBackendMutationInternal(backend, 'createDirectory')
  return reactQuery.useMutation({
    mutationFn: async ([body]: Parameters<Backend['createDirectory']>) => {
      const parentId = body.parentId ?? backend.rootDirectoryId(user)
      if (parentId == null) {
        return null
      } else {
        const siblings = await ensureBackendListDirectory(queryClient, user, backend, parentId)
        const prefix = `${body.title} `
        const samePrefixRegex = new RegExp(`^${prefix}(?<index>\\d+)$`)
        const indices = siblings
          .filter(backendModule.assetIsProject)
          .map(item => samePrefixRegex.exec(item.title)?.groups?.index)
          .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        const title = prefix + (Math.max(0, ...indices) + 1)
        return await createDirectoryMutation.mutateAsync([{ title, parentId }])
      }
    },
  })
}

// =======================================
// === useBackendCreateProjectMutation ===
// =======================================

/** Wrapper over {@link Backend.createProject} that renames the project based on the names of its
 * siblings. */
export function useBackendCreateProjectMutation(backend: Backend) {
  const queryClient = reactQuery.useQueryClient()
  const { user } = authProvider.useNonPartialUserSession()
  const createProjectMutation = useBackendMutationInternal(backend, 'createProject')
  return reactQuery.useMutation({
    mutationFn: async ([body]: Parameters<Backend['createProject']>) => {
      const parentDirectoryId = body.parentDirectoryId ?? backend.rootDirectoryId(user)
      if (parentDirectoryId == null) {
        return null
      } else {
        const siblings = await ensureBackendListDirectory(
          queryClient,
          user,
          backend,
          parentDirectoryId
        )
        const prefix = `${body.projectName} `
        const samePrefixRegex = new RegExp(`^${prefix}(?<index>\\d+)$`)
        const indices = siblings
          .filter(backendModule.assetIsProject)
          .map(item => samePrefixRegex.exec(item.title)?.groups?.index)
          .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        const projectName = prefix + (Math.max(0, ...indices) + 1)
        return await createProjectMutation.mutateAsync([{ projectName, parentDirectoryId }])
      }
    },
  })
}

// =====================================
// === useBackendUploadFilesMutation ===
// =====================================

/** Wrapper over {@link Backend.uploadFile} that shows a modal when assets have conflicting names. */
export function useBackendUploadFilesMutation(backend: Backend) {
  const setIsAssetOpen = store.useStore(storeState => storeState.setIsAssetOpen)
  const queryClient = reactQuery.useQueryClient()
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()

  const uploadFileMutation = useBackendMutationInternal(backend, 'uploadFile')

  return reactQuery.useMutation({
    mutationFn: async ([jsFiles, parentId]: [
      files: Iterable<File>,
      parentId: backendModule.DirectoryId,
    ]) => {
      const localBackend = backend instanceof LocalBackend ? backend : null
      const reversedFiles = [...jsFiles].reverse()
      const siblings = await ensureBackendListDirectory(queryClient, user, backend, parentId)
      const siblingFiles = siblings.filter(backendModule.assetIsFile)
      const siblingProjects = siblings.filter(backendModule.assetIsProject)
      const siblingFileTitles = new Set(siblingFiles.map(asset => asset.title))
      const siblingProjectTitles = new Set(siblingProjects.map(asset => asset.title))
      const files = reversedFiles.filter(backendModule.fileIsNotProject)
      const projects = reversedFiles.filter(backendModule.fileIsProject)
      const duplicateFiles = files.filter(file => siblingFileTitles.has(file.name))
      const duplicateProjects = projects.filter(project =>
        siblingProjectTitles.has(backendModule.stripProjectExtension(project.name))
      )
      const ownerPermission = permissions.tryGetSingletonOwnerPermission(user)
      if (duplicateFiles.length === 0 && duplicateProjects.length === 0) {
        setIsAssetOpen(backend.type, parentId, true)
        for (const file of files) {
          uploadFileMutation.mutate([
            { fileId: null, fileName: file.name, parentDirectoryId: parentId },
            file,
          ])
        }
        for (const project of projects) {
          const basename = backendModule.stripProjectExtension(project.name)
          uploadFileMutation.mutate([
            { fileId: null, fileName: basename, parentDirectoryId: parentId },
            project,
          ])
        }
      } else {
        const siblingFilesByName = new Map(siblingFiles.map(file => [file.title, file]))
        const siblingProjectsByName = new Map(
          siblingProjects.map(project => [project.title, project])
        )
        const conflictingFiles = duplicateFiles.map(file => ({
          // This is SAFE, as `duplicateFiles` only contains files that have siblings
          // with the same name.
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          current: siblingFilesByName.get(file.name)!,
          new: backendModule.createPlaceholderFileAsset(file.name, parentId, ownerPermission),
          file,
        }))
        const conflictingProjects = duplicateProjects.map(project => {
          const basename = backendModule.stripProjectExtension(project.name)
          return {
            // This is SAFE, as `duplicateProjects` only contains projects that have
            // siblings with the same name.
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            current: siblingProjectsByName.get(basename)!,
            new: backendModule.createPlaceholderProjectAsset(
              basename,
              parentId,
              ownerPermission,
              user,
              localBackend?.joinPath(parentId, basename) ?? null
            ),
            file: project,
          }
        })
        setModal(
          <DuplicateAssetsModal
            backend={backend}
            conflictingFiles={conflictingFiles}
            conflictingProjects={conflictingProjects}
            siblingFileNames={siblingFilesByName.keys()}
            siblingProjectNames={siblingProjectsByName.keys()}
            nonConflictingFileCount={files.length - conflictingFiles.length}
            nonConflictingProjectCount={projects.length - conflictingProjects.length}
            doUploadNonConflicting={() => {
              setIsAssetOpen(backend.type, parentId, true)
              const nonConflictingFiles = files.filter(
                otherFile => !siblingFileTitles.has(otherFile.name)
              )
              for (const file of nonConflictingFiles) {
                uploadFileMutation.mutate([
                  { fileId: null, fileName: file.name, parentDirectoryId: parentId },
                  file,
                ])
              }
              const nonConflictingProjects = projects.filter(
                project =>
                  !siblingProjectTitles.has(backendModule.stripProjectExtension(project.name))
              )
              for (const project of nonConflictingProjects) {
                const basename = backendModule.stripProjectExtension(project.name)
                uploadFileMutation.mutate([
                  { fileId: null, fileName: basename, parentDirectoryId: parentId },
                  project,
                ])
              }
            }}
          />
        )
      }
    },
  })
}

// ========================
// === useDownloadAsset ===
// ========================

/** Return a function to download an arbitrary asset. */
export function useDownloadAsset(backend: Backend) {
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  return async (asset: backendModule.AnyAsset) => {
    switch (backend.type) {
      case backendModule.BackendType.remote: {
        switch (asset.type) {
          case backendModule.AssetType.project: {
            try {
              const details = await backend.getProjectDetails(asset.id, asset.parentId)
              if (details.url != null) {
                download.download(details.url, asset.title)
              } else {
                const error: unknown = getText('projectHasNoSourceFilesPhrase')
                toastAndLog('downloadProjectError', error, asset.title)
              }
            } catch (error) {
              toastAndLog('downloadProjectError', error, asset.title)
            }
            break
          }
          case backendModule.AssetType.file: {
            try {
              const details = await backend.getFileDetails(asset.id)
              if (details.url != null) {
                download.download(details.url, asset.title)
              } else {
                const error: unknown = getText('fileNotFoundPhrase')
                toastAndLog('downloadFileError', error, asset.title)
              }
            } catch (error) {
              toastAndLog('downloadFileError', error, asset.title)
            }
            break
          }
          case backendModule.AssetType.datalink: {
            const value = await backend.getDatalink(asset.id)
            const fileName = `${asset.title}.datalink`
            download.download(
              URL.createObjectURL(
                new File([JSON.stringify(value)], fileName, {
                  type: 'application/json+x-enso-data-link',
                })
              ),
              fileName
            )
            break
          }
          default: {
            break
          }
        }
        break
      }
      case backendModule.BackendType.local: {
        if (asset.type === backendModule.AssetType.project) {
          const uuid = localBackendModule.extractTypeAndId(asset.id).id
          download.download(
            `./api/project-manager/projects/${uuid}/enso-project`,
            `${asset.title}.enso-project`
          )
        }
        break
      }
    }
  }
}
