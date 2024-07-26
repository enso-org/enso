/** @file Framework-independent helpers for constructing backend Tanstack queries. */

import type * as queryCore from '@tanstack/query-core'

import type Backend from './services/Backend'
import * as backendModule from './services/Backend'
import * as object from './utilities/data/object'

/** The properties of the Backend type that are methods. */
export type BackendMethods = object.ExtractKeys<Backend, object.MethodOf<Backend>>

/** For each backend method, an optional function defining how to create a query key from its arguments. */
type BackendQueryNormalizers = {
  [Method in BackendMethods]?: (...args: Parameters<Backend[Method]>) => queryCore.QueryKey
}

const NORMALIZE_METHOD_QUERY: BackendQueryNormalizers = {
  listDirectory: query => [query.parentId, object.omit(query, 'parentId')],
  getFileDetails: fileId => [fileId],
}

/** Creates a partial query key representing the given method and arguments. */
function normalizeMethodQuery<Method extends BackendMethods>(
  method: Method,
  args: Parameters<Backend[Method]>,
) {
  return NORMALIZE_METHOD_QUERY[method]?.(...args) ?? args
}

/** Returns query options to use for the given backend method invocation. */
export function backendQueryOptions<Method extends BackendMethods>(
  backend: Backend | null,
  method: Method,
  args: Parameters<Backend[Method]>,
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
  args: Parameters<Backend[Method]>,
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
