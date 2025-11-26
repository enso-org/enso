/** @file Framework-independent helpers for constructing backend Tanstack queries. */
import type { Backend } from 'enso-common/src/services/Backend'
import * as backendModule from 'enso-common/src/services/Backend'
import { omit, type ExtractKeys, type MethodOf } from 'enso-common/src/utilities/data/object'

/** Should match `NetworkMode` in TanStack query core. */
type NetworkMode = 'online' | 'always' | 'offlineFirst'

/** The properties of the Backend type that are methods. */
export type BackendMethods = ExtractKeys<Backend, MethodOf<Backend>>

/** Ensure that the given type contains only names of backend methods. */
type DefineBackendMethods<T extends BackendMethods> = T

/** Names of methods corresponding to mutations. */
export type BackendMutationMethod = DefineBackendMethods<
  | 'acceptInvitation'
  | 'associateTag'
  | 'changeUserGroup'
  | 'closeProject'
  | 'copyAsset'
  | 'cancelSubscription'
  | 'createCheckoutSession'
  | 'createCredential'
  | 'createDatalink'
  | 'createDirectory'
  | 'createPermission'
  | 'createApiKey'
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
  | 'deleteApiKey'
  | 'deleteProjectExecution'
  | 'deleteTag'
  | 'deleteUser'
  | 'deleteUserGroup'
  | 'duplicateProject'
  | 'exportArchive'
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
  | 'uploadImage'
  | 'uploadOrganizationPicture'
  | 'uploadUserPicture'
>

/** Names of methods corresponding to queries. */
export type BackendQueryMethod = Exclude<BackendMethods, BackendMutationMethod>

export const STALE_TIME_MAP: Partial<Record<BackendQueryMethod, number>> = {
  getOrganization: Infinity,
  usersMe: Infinity,
  listUsers: Infinity,
}

export const PERSISTENCE_MAP: Partial<Record<BackendQueryMethod, false>> = {
  listDirectory: false,
  searchDirectory: false,
  listTags: false,
  getAssetDetails: false,
}

/** A value for {@link INVALIDATION_MAP} representing all queries. */
export const INVALIDATE_ALL_QUERIES = Symbol('invalidate all queries')
/** A mapping between mutation methods and queries invalidated by them. */
export const INVALIDATION_MAP: Partial<
  Record<BackendMutationMethod, readonly (BackendQueryMethod | typeof INVALIDATE_ALL_QUERIES)[]>
> = {
  createUser: ['usersMe'],
  updateUser: [INVALIDATE_ALL_QUERIES],
  deleteUser: [
    'usersMe',
    'listUsers',
    'listUserGroups',
    'listDirectory',
    'searchDirectory',
    'getAssetDetails',
  ],
  removeUser: [
    'usersMe',
    'listUsers',
    'listUserGroups',
    'listDirectory',
    'searchDirectory',
    'getAssetDetails',
  ],
  restoreUser: ['usersMe'],
  uploadUserPicture: ['usersMe'],
  updateOrganization: ['getOrganization'],
  uploadOrganizationPicture: ['getOrganization'],
  createUserGroup: [INVALIDATE_ALL_QUERIES],
  deleteUserGroup: [INVALIDATE_ALL_QUERIES],
  changeUserGroup: [INVALIDATE_ALL_QUERIES],
  createTag: ['listTags'],
  deleteTag: ['listTags'],
  associateTag: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  acceptInvitation: [INVALIDATE_ALL_QUERIES],
  declineInvitation: ['usersMe'],
  createProject: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  duplicateProject: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  createDirectory: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  createSecret: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  updateSecret: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  updateProject: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  updateFile: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  updateDirectory: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  createDatalink: ['listDirectory', 'searchDirectory', 'getDatalink', 'getAssetDetails'],
  uploadFileEnd: ['listDirectory', 'searchDirectory', 'listAssetVersions', 'getAssetDetails'],
  copyAsset: ['listDirectory', 'searchDirectory', 'listAssetVersions', 'getAssetDetails'],
  deleteAsset: ['listDirectory', 'searchDirectory', 'listAssetVersions', 'getAssetDetails'],
  undoDeleteAsset: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  updateAsset: ['listDirectory', 'searchDirectory', 'listAssetVersions', 'getAssetDetails'],
  openProject: ['listDirectory', 'searchDirectory', 'getAssetDetails'],
  closeProject: ['listDirectory', 'searchDirectory', 'listAssetVersions', 'getAssetDetails'],
  createProjectExecution: ['listProjectExecutions'],
  updateProjectExecution: ['listProjectExecutions'],
  syncProjectExecution: ['listProjectExecutions'],
  deleteProjectExecution: ['listProjectExecutions'],
  createApiKey: ['listApiKeys'],
  deleteApiKey: ['listApiKeys'],
  uploadImage: ['listDirectory', 'searchDirectory'],
}

/** For each backend method, an optional function defining how to create a query key from its arguments. */
type BackendQueryNormalizers = {
  [Method in BackendMethods]?: (
    ...args: Readonly<Parameters<Backend[Method]>>
  ) => readonly unknown[]
}

const NORMALIZE_METHOD_QUERY: BackendQueryNormalizers = {
  listDirectory: (query) => [query.parentId, omit(query, 'parentId')],
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
  keyExtra?: readonly unknown[] | undefined,
): {
  queryKey: readonly unknown[]
  networkMode: NetworkMode
} {
  return {
    ...backendBaseOptions(backend),
    queryKey: backendQueryKey(backend, method, args, keyExtra),
  }
}

/** Returns the QueryKey to use for the given backend method invocation. */
export function backendQueryKey<
  Method extends BackendMethods,
  TQueryKey extends readonly unknown[] = readonly unknown[],
>(
  backend: Backend | null,
  method: Method,
  args: Readonly<Parameters<Backend[Method]>>,
  keyExtra?: TQueryKey | undefined,
) {
  return [backend?.type, method, ...normalizeMethodQuery(method, args), ...(keyExtra ?? [])]
}

/** Returns options applicable to any method of the given backend. */
export function backendBaseOptions(backend: Backend | null): {
  networkMode: NetworkMode
} {
  return {
    networkMode: backend?.type === backendModule.BackendType.local ? 'always' : 'online',
  }
}
