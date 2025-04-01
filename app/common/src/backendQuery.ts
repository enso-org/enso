/** @file Framework-independent helpers for constructing backend Tanstack queries. */

import type * as queryCore from '@tanstack/query-core'

import type Backend from './services/Backend'
import * as backendModule from './services/Backend'
import * as object from './utilities/data/object'

/** The properties of the Backend type that are methods. */
export type BackendMethods = object.ExtractKeys<Backend, object.MethodOf<Backend>>

/** Ensure that the given type contains only names of backend methods. */
type DefineBackendMethods<T extends BackendMethods> = T

/** Names of methods corresponding to mutations. */
export type BackendMutationMethod = DefineBackendMethods<
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
  | 'createProjectExecution'
  | 'createSecret'
  | 'createTag'
  | 'createUser'
  | 'createUserGroup'
  | 'declineInvitation'
  | 'deleteAsset'
  | 'deleteDatalink'
  | 'deleteInvitation'
  | 'deleteProjectExecution'
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
  | 'syncProjectExecution'
  | 'undoDeleteAsset'
  | 'updateAsset'
  | 'updateDirectory'
  | 'updateFile'
  | 'updateOrganization'
  | 'updateProject'
  | 'updateProjectExecution'
  | 'updateSecret'
  | 'updateUser'
  | 'uploadFileChunk'
  | 'uploadFileEnd'
  | 'uploadFileStart'
  | 'uploadOrganizationPicture'
  | 'uploadUserPicture'
>

/** Names of methods corresponding to queries. */
export type BackendQueryMethod = Exclude<BackendMethods, BackendMutationMethod>

/** A value for {@link INVALIDATION_MAP} representing all queries. */
export const INVALIDATE_ALL_QUERIES = Symbol('invalidate all queries')
/** A mapping between mutation methods and queries invalidated by them. */
export const INVALIDATION_MAP: Partial<
  Record<BackendMutationMethod, readonly (BackendQueryMethod | typeof INVALIDATE_ALL_QUERIES)[]>
> = {
  createUser: ['usersMe'],
  updateUser: [INVALIDATE_ALL_QUERIES],
  deleteUser: ['usersMe'],
  restoreUser: ['usersMe'],
  uploadUserPicture: ['usersMe'],
  updateOrganization: ['getOrganization'],
  uploadOrganizationPicture: ['getOrganization'],
  createUserGroup: [INVALIDATE_ALL_QUERIES],
  deleteUserGroup: [INVALIDATE_ALL_QUERIES],
  changeUserGroup: [INVALIDATE_ALL_QUERIES],
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
  updateProject: ['listDirectory'],
  updateFile: ['listDirectory'],
  updateDirectory: ['listDirectory'],
  createDatalink: ['listDirectory', 'getDatalink'],
  uploadFileEnd: ['listDirectory', 'listAssetVersions'],
  copyAsset: ['listDirectory', 'listAssetVersions'],
  deleteAsset: ['listDirectory', 'listAssetVersions'],
  undoDeleteAsset: ['listDirectory'],
  updateAsset: ['listDirectory', 'listAssetVersions'],
  openProject: ['listDirectory'],
  closeProject: ['listDirectory', 'listAssetVersions'],
  createProjectExecution: ['listProjectExecutions'],
  updateProjectExecution: ['listProjectExecutions'],
  syncProjectExecution: ['listProjectExecutions'],
  deleteProjectExecution: ['listProjectExecutions'],
}

/** For each backend method, an optional function defining how to create a query key from its arguments. */
type BackendQueryNormalizers = {
  [Method in BackendMethods]?: (
    ...args: Readonly<Parameters<Backend[Method]>>
  ) => queryCore.QueryKey
}

const NORMALIZE_METHOD_QUERY: BackendQueryNormalizers = {
  listDirectory: (query) => [query.parentId, object.omit(query, 'parentId')],
  getFileDetails: (fileId) => [fileId],
}

/** Creates a partial query key representing the given method and arguments. */
function normalizeMethodQuery<Method extends BackendMethods>(
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
) {
  return NORMALIZE_METHOD_QUERY[method]?.(...args) ?? args
}

/** Returns query options to use for the given backend method invocation. */
export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  keyExtra?: queryCore.QueryKey | undefined,
): {
  queryKey: queryCore.QueryKey
  networkMode: queryCore.NetworkMode
} {
  return {
    ...backendBaseOptions(backend),
    queryKey: backendQueryKey(backend, method, args, keyExtra),
  }
}

/** Returns the QueryKey to use for the given backend method invocation. */
export function backendQueryKey<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  keyExtra?: queryCore.QueryKey | undefined,
): queryCore.QueryKey {
  return [backend?.type, method, ...normalizeMethodQuery(method, args), ...(keyExtra ?? [])]
}

/** Returns options applicable to any method of the given backend. */
export function backendBaseOptions(backend: Backend | null): {
  networkMode: queryCore.NetworkMode
} {
  return {
    networkMode: backend?.type === backendModule.BackendType.local ? 'always' : 'online',
  }
}
