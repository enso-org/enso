/** @file The mock API. */
import * as backend from 'enso-common/src/services/Backend'
import * as paths from 'enso-common/src/services/Backend/remoteBackendPaths'
import {
  organizationIdToDirectoryId,
  userGroupIdToDirectoryId,
  userIdToDirectoryId,
} from 'enso-common/src/services/RemoteBackend/ids'
import * as dateTime from 'enso-common/src/utilities/data/dateTime'
import * as object from 'enso-common/src/utilities/data/object'
import * as permissions from 'enso-common/src/utilities/permissions'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { test, type Page, type Request, type Route } from 'integration-test/base'
import { readFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import invariant from 'tiny-invariant'
import { VALID_PASSWORD } from '../actions/utilities'

const __dirname = dirname(fileURLToPath(import.meta.url))

/** The HTTP status code representing a response with an empty body. */
const HTTP_STATUS_NO_CONTENT = 204
/** The HTTP status code representing a bad request. */
const HTTP_STATUS_BAD_REQUEST = 400
/** The HTTP status code representing a URL that does not exist. */
const HTTP_STATUS_NOT_FOUND = 404
/** A user id that is a path glob. */
const GLOB_USER_ID = backend.UserId('*')
/** An asset ID that is a path glob. */
const GLOB_ASSET_ID = backend.AssetId('*')
/** A directory ID that is a path glob. */
const GLOB_DIRECTORY_ID = backend.DirectoryId('directory-*')
/** A project ID that is a path glob. */
const GLOB_PROJECT_ID = backend.ProjectId('*')
/** A tag ID that is a path glob. */
const GLOB_TAG_ID = backend.TagId('*')
const BASE_URL = 'https://mock/'
const MOCK_S3_BUCKET_URL = 'https://mock-s3-bucket.com/'

const lastDate = new Date(0)

function newDate() {
  let date = new Date()
  while (Number(date) === Number(lastDate)) {
    // Busy loop until date is different.
    date = new Date()
  }
  return dateTime.toRfc3339(date)
}

function array<T>(): Readonly<T>[] {
  return []
}

const INITIAL_CALLS_OBJECT = {
  changePassword: array<{ oldPassword: string; newPassword: string }>(),
  getAssetDetails: array<{ assetId: backend.AssetId }>(),
  listDirectory: array<ListDirectoryQuery>(),
  searchDirectory: array<SearchDirectoryQuery>(),
  listSecrets: array<object>(),
  listTags: array<object>(),
  listUsers: array<object>(),
  listUserGroups: array<object>(),
  getProjectDetails: array<{ projectId: backend.ProjectId; presigned: boolean }>(),
  copyAsset: array<{ assetId: backend.AssetId; parentId: backend.DirectoryId }>(),
  listInvitations: array<object>(),
  inviteUser: array<object>(),
  createPermission: array<object>(),
  closeProject: array<{ projectId: backend.ProjectId }>(),
  openProject: array<{ projectId: backend.ProjectId }>(),
  hybridSetOpenInProgress: array<{ projectId: backend.ProjectId }>(),
  hybridSetOpened: array<{ projectId: backend.ProjectId }>(),
  deleteTag: array<{ tagId: backend.TagId }>(),
  postLogEvent: array<object>(),
  uploadUserPicture: array<{ content: string }>(),
  uploadOrganizationPicture: array<{ content: string }>(),
  s3Put: array<object>(),
  s3Get: array<object>(),
  uploadFileStart: array<{ uploadId: backend.FileId }>(),
  uploadFileEnd: array<backend.UploadFileEndRequestBody>(),
  createSecret: array<backend.CreateSecretRequestBody>(),
  createCheckoutSession: array<backend.CreateCheckoutSessionRequestBody>(),
  updateAsset: array<{ assetId: backend.AssetId } & backend.UpdateAssetRequestBody>(),
  associateTag: array<{ assetId: backend.AssetId; labels: readonly backend.LabelName[] }>(),
  updateDirectory: array<
    { directoryId: backend.DirectoryId } & backend.UpdateDirectoryRequestBody
  >(),
  deleteAsset: array<{ assetId: backend.AssetId; force: boolean }>(),
  undoDeleteAsset: array<{ assetId: backend.AssetId }>(),
  createUser: array<backend.CreateUserRequestBody>(),
  createUserGroup: array<backend.CreateUserGroupRequestBody>(),
  changeUserGroup: array<{ userId: backend.UserId } & backend.ChangeUserGroupRequestBody>(),
  updateCurrentUser: array<backend.UpdateUserRequestBody>(),
  usersMe: array<object>(),
  updateOrganization: array<backend.UpdateOrganizationRequestBody>(),
  getOrganization: array<object>(),
  createTag: array<backend.CreateTagRequestBody>(),
  createProject: array<backend.CreateProjectRequestBody>(),
  createDirectory: array<backend.CreateDirectoryRequestBody>(),
  resolveProjectAssetData: array<{ projectId: backend.ProjectId; path: string }>(),
  getProjectAsset: array<{ projectId: backend.ProjectId }>(),
  updateProject: array<backend.UpdateProjectRequestBody>(),
}

const READONLY_INITIAL_CALLS_OBJECT: TrackedCallsInternal = INITIAL_CALLS_OBJECT

export { READONLY_INITIAL_CALLS_OBJECT as INITIAL_CALLS_OBJECT }

type TrackedCallsInternal = {
  [K in keyof typeof INITIAL_CALLS_OBJECT]: Readonly<(typeof INITIAL_CALLS_OBJECT)[K]>
}

export interface TrackedCalls extends TrackedCallsInternal {}

/** The type for the search query for the "list directory" endpoint. */
interface ListDirectoryQuery {
  readonly parent_id?: string
  readonly filter_by?: backend.FilterBy
  readonly labels?: backend.LabelName[]
  readonly recent_projects?: boolean
  readonly sort_expression?: backend.AssetSortExpression | null
  readonly sort_direction?: backend.AssetSortDirection | null
  readonly from?: backend.AssetId | null
  readonly from_modified_at?: dateTime.Rfc3339DateTime | null
  readonly page_size?: number | null
}

/** The type for the search query for the "search directory" endpoint. */
interface SearchDirectoryQuery {
  readonly parent_id?: backend.DirectoryId | null
  readonly query?: string | null
  readonly title?: string | null
  readonly description?: string | null
  readonly type?: string | null
  readonly extension?: string | null
  readonly labels?: readonly backend.LabelName[] | null
  readonly sort_expression?: backend.AssetSortExpression | null
  readonly sort_direction?: backend.AssetSortDirection | null
  readonly from?: backend.PaginationToken | null
  readonly page_size?: number | null
}

/** The return type of {@link mockCloudApi}. */
export interface MockCloudApi extends Awaited<ReturnType<typeof mockCloudApi>> {}

/** Add route handlers for the mock API to a page. */
export async function mockCloudApi(page: Page) {
  const defaultEmail = 'email@example.com' as backend.EmailAddress
  const defaultUsername = 'user name'
  const defaultPassword = VALID_PASSWORD
  const defaultOrganizationId = backend.OrganizationId('organization-placeholder id')
  const defaultOrganizationName = 'organization name'
  const defaultUserId = backend.UserId('user-placeholder id')
  const defaultDirectoryId = backend.DirectoryId('directory-placeholder id')
  const defaultUser: backend.User = {
    email: defaultEmail,
    name: defaultUsername,
    organizationId: defaultOrganizationId,
    userId: defaultUserId,
    isEnabled: true,
    rootDirectoryId: defaultDirectoryId,
    userGroups: null,
    plan: backend.Plan.solo,
    isOrganizationAdmin: true,
    isEnsoTeamMember: true,
    groups: [],
  }

  const defaultOrganization: backend.OrganizationInfo = {
    id: defaultOrganizationId,
    name: defaultOrganizationName,
    address: null,
    email: null,
    picture: null,
    website: null,
    subscription: {},
  }

  const callsObjects = new Set<typeof INITIAL_CALLS_OBJECT>()
  let totalSeats = 1
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  let subscriptionDuration = 0

  let isOnline = true
  let currentUser: backend.User | null = defaultUser
  let currentProfilePicture: string | null = null
  let currentPassword = defaultPassword
  let currentOrganization: backend.OrganizationInfo | null = defaultOrganization
  let currentOrganizationProfilePicture: string | null = null

  const assetMap = new Map<backend.AssetId, backend.AnyAsset>()
  const deletedAssets = new Set<backend.AssetId>()
  let assets: backend.AnyAsset[] = []
  const labels: backend.Label[] = []
  const labelsByValue = new Map<backend.LabelName, backend.Label>()
  const labelMap = new Map<backend.TagId, backend.Label>()
  const users: backend.User[] = [defaultUser]
  const usersMap = new Map<backend.UserId, backend.User>()
  const userGroups: backend.UserGroupInfo[] = [
    {
      id: backend.UserGroupId('usergroup-1'),
      groupName: 'User Group 1',
      organizationId: currentOrganization.id,
    },
  ]

  usersMap.set(defaultUser.userId, defaultUser)

  function getParentPath(parentId: backend.DirectoryId, acc: string[] = []) {
    const parent = assetMap.get(parentId)

    if (parent == null) {
      return backend.ParentsPath([parentId, ...acc].join('/'))
    }

    // this should never happen, but we need to check it for a case
    invariant(parent.type === backend.AssetType.directory, 'Parent is not a directory')

    return getParentPath(parent.parentId, [parent.id, ...acc])
  }

  function getVirtualParentPath(parentId: backend.DirectoryId, parts: string[] = []) {
    const parent = assetMap.get(parentId)

    if (parent == null) {
      return backend.VirtualParentsPath(parts.join('/'))
    }

    // This should never happen, but we need to check it for type-safety purposes.
    invariant(parent.type === backend.AssetType.directory, 'Parent is not a directory')

    return getVirtualParentPath(parent.parentId, [parent.title, ...parts])
  }

  function getEnsoPath(parentId: backend.DirectoryId, parts: string[] = []) {
    const parent = assetMap.get(parentId)

    if (parent == null) {
      // FIXME: Support teams in API mock
      return backend.EnsoPath(`enso://Users/${defaultUser.name}/${parts.join('/')}`)
    }

    // This should never happen, but we need to check it for type-safety purposes.
    invariant(parent.type === backend.AssetType.directory, 'Parent is not a directory')

    return getEnsoPath(parent.parentId, [parent.title, ...parts])
  }

  function trackCalls() {
    const calls = structuredClone(INITIAL_CALLS_OBJECT)
    callsObjects.add(calls)
    return calls
  }

  function pushToKey<Object extends Record<keyof Object, unknown[]>, Key extends keyof Object>(
    object: Object,
    key: Key,
    item: Object[Key][number],
  ) {
    object[key].push(item)
  }

  function called<Key extends keyof typeof INITIAL_CALLS_OBJECT>(
    key: Key,
    args: (typeof INITIAL_CALLS_OBJECT)[Key][number],
  ) {
    for (const callsObject of callsObjects) {
      pushToKey(callsObject, key, args)
    }
  }

  function listDirectory(query: ListDirectoryQuery) {
    const parentId = query.parent_id ?? defaultDirectoryId
    let filteredAssets = assets.filter((asset) => asset.parentId === parentId)
    switch (query.filter_by) {
      case backend.FilterBy.active: {
        filteredAssets = filteredAssets.filter((asset) => !deletedAssets.has(asset.id))
        break
      }
      case backend.FilterBy.trashed: {
        filteredAssets = assets.filter((asset) => deletedAssets.has(asset.id))
        break
      }
      case backend.FilterBy.recent: {
        filteredAssets = assets.filter((asset) => !deletedAssets.has(asset.id)).slice(0, 10)
        break
      }
      case backend.FilterBy.all:
      case null:
      case undefined: {
        // do nothing
        break
      }
    }
    const sortedAssets = [...filteredAssets].sort((a, b) =>
      backend.compareAssets(a, b, query.sort_expression, query.sort_direction),
    )
    const index =
      query.from == null ? 0 : sortedAssets.findIndex((asset) => asset.id === query.from) + 1
    return sortedAssets.slice(index, query.page_size != null ? index + query.page_size : undefined)
  }

  function listRootDirectory() {
    return listDirectory({})
  }

  function searchDirectory(query: SearchDirectoryQuery) {
    called('searchDirectory', query)
    const parentId = query.parent_id ?? defaultDirectoryId
    const queuedParentIds = [parentId]
    const isMatch = backend.doesAssetMatchQuery({
      parentId: query.parent_id ?? null,
      query: query.query ?? null,
      title: query.title ?? null,
      description: query.description ?? null,
      type: query.type ?? null,
      extension: query.extension ?? null,
      labels: query.labels ?? [],
      sortExpression: query.sort_expression ?? null,
      sortDirection: query.sort_direction ?? null,
      from: query.from ?? null,
      pageSize: query.page_size ?? null,
    })
    const matchingAssets: backend.AnyAsset[] = []
    while (true) {
      const currentParentId = queuedParentIds.shift()
      if (currentParentId == null) break
      const siblings = assets.filter((asset) => asset.parentId === currentParentId)
      for (const sibling of siblings) {
        if (sibling.type === backend.AssetType.directory) {
          queuedParentIds.push(sibling.id)
        }
        if (isMatch(sibling)) {
          matchingAssets.push(sibling)
        }
      }
    }
    const sortedAssets = [...matchingAssets].sort((a, b) =>
      backend.compareAssets(a, b, query.sort_expression, query.sort_direction),
    )
    const index =
      query.from == null ?
        0
      : sortedAssets.findIndex(
          (asset) => asset.id === (query.from as backend.AssetId | null | undefined),
        ) + 1
    return sortedAssets.slice(index, query.page_size != null ? index + query.page_size : undefined)
  }

  const addAsset = <T extends backend.AnyAsset>(asset: T) => {
    assetMap.set(asset.id, asset)
    assets = Array.from(assetMap.values())

    return asset
  }

  const deleteAsset = (assetId: backend.AssetId) => {
    const alreadyDeleted = deletedAssets.has(assetId)
    deletedAssets.add(assetId)

    return !alreadyDeleted
  }

  const forceDeleteAsset = (assetId: backend.AssetId) => {
    const hasAsset = assetMap.has(assetId)
    deletedAssets.delete(assetId)
    assetMap.delete(assetId)
    assets.splice(
      assets.findIndex((asset) => asset.id === assetId),
      1,
    )
    return hasAsset
  }

  const undeleteAsset = (assetId: backend.AssetId) => {
    const wasDeleted = deletedAssets.has(assetId)
    deletedAssets.delete(assetId)
    return wasDeleted
  }

  const editAsset = (assetId: backend.AssetId, rest: Partial<backend.AnyAsset>) => {
    const asset = assetMap.get(assetId)

    if (asset == null) {
      throw new Error(`Asset ${assetId} not found`)
    }

    const updated = object.merge(asset, rest)

    addAsset(updated)

    return updated
  }

  const createUserPermission = (
    user: backend.User,
    permission: permissions.PermissionAction = permissions.PermissionAction.own,
    rest: Partial<backend.UserPermission> = {},
  ): backend.UserPermission =>
    object.merge(
      {
        user,
        permission,
      },
      rest,
    )

  const createUserGroupPermission = (
    userGroup: backend.UserGroupInfo,
    permission: permissions.PermissionAction = permissions.PermissionAction.own,
    rest: Partial<backend.UserGroupPermission> = {},
  ): backend.UserGroupPermission => object.merge({ userGroup, permission }, rest)

  function createAsset<T extends backend.AnyAsset>(rest: Pick<T, 'id' | 'type'> & Partial<T>): T
  function createAsset(
    rest: Pick<backend.AnyAsset, 'id' | 'type'> & Partial<backend.AnyAsset>,
  ): backend.AnyAsset {
    // @ts-expect-error This is UNSAFE if the generic parameter is explicitly specified.
    return {
      projectState: null,
      extension: null,
      title: rest.title ?? '',
      modifiedAt: newDate(),
      description: rest.description ?? '',
      labels: [],
      parentId: defaultDirectoryId,
      permissions: [createUserPermission(defaultUser, permissions.PermissionAction.own)],
      get parentsPath() {
        return getParentPath(this.parentId)
      },
      get virtualParentsPath() {
        return getVirtualParentPath(this.parentId)
      },
      get ensoPath() {
        return backend.EnsoPath(
          `${getEnsoPath(this.parentId).replace(/[/]$/, '')}/${rest.title ?? ''}`,
        )
      },
      ...rest,
    }
  }

  const createDirectory = (rest: Partial<backend.DirectoryAsset> = {}): backend.DirectoryAsset => {
    const title =
      rest.title ??
      (() => {
        const parentId = rest.parentId ?? defaultDirectoryId
        let i = 0
        for (const asset of assets) {
          if (asset.parentId !== parentId) continue
          const match = asset.title.match(/^New Folder (\d+)$/)
          if (match?.[1] == null) continue
          i = Math.max(i, Number(match[1]))
        }
        return `New Folder ${i + 1}`
      })()

    return createAsset({
      type: backend.AssetType.directory,
      id: backend.DirectoryId(`directory-${uniqueString()}`),
      title,
      ...rest,
    })
  }

  const createProject = (rest: Partial<backend.ProjectAsset> = {}): backend.ProjectAsset => {
    const title =
      rest.title ??
      (() => {
        const parentId = rest.parentId ?? defaultDirectoryId
        let i = 0
        for (const asset of assets) {
          if (asset.parentId !== parentId) continue
          const match = asset.title.match(/^New Project (\d+)$/)
          if (match?.[1] == null) continue
          i = Math.max(i, Number(match[1]))
        }
        return `New Project ${i + 1}`
      })()

    return createAsset({
      type: backend.AssetType.project,
      id: backend.ProjectId('project-' + uniqueString()),
      title,
      projectState: {
        type: backend.ProjectState.closed,
        volumeId: '',
      },
      ...rest,
    })
  }

  const createFile = (rest: Partial<backend.FileAsset> = {}): backend.FileAsset => {
    return createAsset({
      type: backend.AssetType.file,
      id: backend.FileId('file-' + uniqueString()),
      extension: '',
      ...rest,
    })
  }

  const createSecret = (rest: Partial<backend.SecretAsset>): backend.SecretAsset => {
    return createAsset({
      type: backend.AssetType.secret,
      id: backend.SecretId('secret-' + uniqueString()),
      ...rest,
    })
  }

  const createDatalink = (rest: Partial<backend.DatalinkAsset>): backend.DatalinkAsset => {
    return createAsset({
      type: backend.AssetType.datalink,
      id: backend.DatalinkId('datalink-' + uniqueString()),
      ...rest,
    })
  }

  const createLabel = (value: string, color: backend.LChColor): backend.Label => ({
    id: backend.TagId('tag-' + uniqueString()),
    value: backend.LabelName(value),
    color,
  })

  const addDirectory = (rest: Partial<backend.DirectoryAsset> = {}) => {
    return addAsset(createDirectory(rest))
  }

  const addProject = (rest: Partial<backend.ProjectAsset> = {}) => {
    return addAsset(createProject(rest))
  }

  const addFile = (rest: Partial<backend.FileAsset> = {}) => {
    return addAsset(createFile(rest))
  }

  const addSecret = (rest: Partial<backend.SecretAsset> = {}) => {
    return addAsset(createSecret(rest))
  }

  const addDatalink = (rest: Partial<backend.DatalinkAsset> = {}) => {
    return addAsset(createDatalink(rest))
  }

  const addLabel = (value: string, color: backend.LChColor) => {
    const label = createLabel(value, color)
    labels.push(label)
    labelsByValue.set(label.value, label)
    labelMap.set(label.id, label)
    return label
  }

  const setLabels = (id: backend.AssetId, newLabels: readonly backend.LabelName[]) => {
    const ids = new Set<backend.AssetId>([id])
    for (const [innerId, asset] of assetMap) {
      if (ids.has(asset.parentId)) {
        ids.add(innerId)
      }
    }
    for (const innerId of ids) {
      const asset = assetMap.get(innerId)
      if (asset != null) {
        object.unsafeMutable(asset).labels = newLabels
      }
    }
  }

  const createCheckoutSession = (_body: backend.CreateCheckoutSessionRequestBody) => {
    return {
      url: backend.HttpsUrl('http://stripe.com/checkout/session'),
    } satisfies backend.CheckoutSession
  }

  const addUser = (name: string, rest: Partial<backend.User> = {}) => {
    const organizationId = currentOrganization?.id ?? defaultOrganizationId
    const user: backend.User = {
      userId: backend.UserId(`user-${uniqueString()}`),
      name,
      email: backend.EmailAddress(`${name}@example.org`),
      organizationId,
      rootDirectoryId: organizationIdToDirectoryId(organizationId),
      isEnabled: true,
      userGroups: null,
      plan: backend.Plan.enterprise,
      isOrganizationAdmin: true,
      isEnsoTeamMember: true,
      ...rest,
    }
    users.push(user)
    usersMap.set(user.userId, user)
    return user
  }

  const deleteUser = (userId: backend.UserId) => {
    usersMap.delete(userId)
    const index = users.findIndex((user) => user.userId === userId)
    if (index === -1) {
      return false
    } else {
      users.splice(index, 1)
      return true
    }
  }

  const addUserGroup = (name: string, rest?: Partial<backend.UserGroupInfo>) => {
    const userGroup: backend.UserGroupInfo = {
      id: backend.UserGroupId(`usergroup-${uniqueString()}`),
      groupName: name,
      organizationId: currentOrganization?.id ?? defaultOrganizationId,
      ...rest,
    }
    userGroups.push(userGroup)
    return userGroup
  }

  const deleteUserGroup = (userGroupId: backend.UserGroupId) => {
    const index = userGroups.findIndex((userGroup) => userGroup.id === userGroupId)
    if (index === -1) {
      return false
    } else {
      users.splice(index, 1)
      return true
    }
  }

  // FIXME[sb]: Add missing endpoints:
  // addPermission,
  // deletePermission,

  const addUserGroupToUser = (userId: backend.UserId, userGroupId: backend.UserGroupId) => {
    const user = usersMap.get(userId)
    if (user == null || user.userGroups?.includes(userGroupId) === true) {
      // The user does not exist, or they are already in this group.
      return false
    } else {
      const newUserGroups = object.unsafeMutable(user.userGroups ?? [])
      newUserGroups.push(userGroupId)
      object.unsafeMutable(user).userGroups = newUserGroups
      return true
    }
  }

  const removeUserGroupFromUser = (userId: backend.UserId, userGroupId: backend.UserGroupId) => {
    const user = usersMap.get(userId)
    if (user?.userGroups?.includes(userGroupId) !== true) {
      // The user does not exist, or they are already not in this group.
      return false
    } else {
      object.unsafeMutable(user.userGroups).splice(user.userGroups.indexOf(userGroupId), 1)
      return true
    }
  }

  /**
   * Transform glob patterts that we use in URLs into a regexp that matches them.
   * This is NOT a complete glob implementation, it only supports * and ** patterns.
   */
  function simpleGlobToRegex(glob: string): RegExp {
    const regexBody = glob
      .replace(/[.*+?^${}()|[\]\\]/g, '\\$&') // regex escape
      .replace(/\\\*\\\*/g, '(.*)') // ** → (.*)
      .replace(/\\\*/g, '([^/]*)') // * → ([^/]*)
    return new RegExp(`^${regexBody}$`)
  }

  await test.step('Mock API', async () => {
    const method =
      (theMethod: string) =>
      async (
        url: string,
        callback: (
          route: Route,
          request: Request,
          globCaptures: string[],
          params: URLSearchParams,
        ) => unknown,
      ) => {
        if (!url) throw new Error(`Mock API URL missing. Callback: ${callback}`)
        if (url.includes('?'))
          throw new Error(
            'Base mock API URL patterns cannot contain a query string.\n  Problematic URL: ' + url,
          )

        const urlPathRegex = simpleGlobToRegex(BASE_URL + url)
        async function handler(route: Route, request: Request): Promise<void> {
          if (request.method() !== theMethod) {
            return await route.fallback()
          } else {
            const url = new URL(request.url())
            const [_, ...globCaptures] = (url.origin + url.pathname).match(urlPathRegex)!
            const result = await callback(route, request, globCaptures, url.searchParams)
            // `null` counts as a JSON value that we will want to return.
            if (result !== undefined) {
              await route.fulfill({ json: result })
            }
          }
        }
        await page.route((url) => urlPathRegex.test(url.origin + url.pathname), handler)
      }
    const get = method('GET')
    const put = method('PUT')
    const post = method('POST')
    const patch = method('PATCH')
    const delete_ = method('DELETE')

    await page.route(BASE_URL + '**', (route, request) => {
      const message = `Missing route handler for '${request.method()} ${request.url().substring(BASE_URL.length)}'`
      throw new Error(message)
    })

    // === Mock Cognito endpoints ===

    await page.route('https://stripe.com/*', async (route) => {
      await route.fulfill()
    })

    await page.route('https://mock-cognito.com/change-password', async (route, request) => {
      if (request.method() !== 'POST') {
        await route.fallback()
      } else {
        /** The type for the JSON request payload for this endpoint. */
        interface Body {
          readonly oldPassword: string
          readonly newPassword: string
        }
        const body: Body = await request.postDataJSON()
        called('changePassword', body)
        if (body.oldPassword === currentPassword) {
          currentPassword = body.newPassword
          await route.fulfill({ status: HTTP_STATUS_NO_CONTENT })
        } else {
          await route.fulfill({ status: HTTP_STATUS_BAD_REQUEST })
        }
      }
    })

    // === Endpoints returning arrays ===

    await get(paths.LIST_DIRECTORY_PATH, (route, _req, _, params) => {
      const query = Object.fromEntries(params.entries()) as ListDirectoryQuery
      called('listDirectory', query)
      const assets = listDirectory(query)
      const last = assets.at(-1)
      const json: backend.ListDirectoryResponseBody = {
        assets,
        paginationToken: last ? backend.PaginationToken(String(last.id)) : null,
      }
      route.fulfill({ json })
    })
    await get(paths.SEARCH_DIRECTORY_PATH, (route, _req, _, params) => {
      const query = Object.fromEntries(params.entries()) as SearchDirectoryQuery
      called('searchDirectory', query)
      const assets = searchDirectory(query)
      const last = assets.at(-1)
      const json: backend.ListDirectoryResponseBody = {
        assets,
        paginationToken: last ? backend.PaginationToken(String(last.id)) : null,
      }
      route.fulfill({ json })
    })
    await get(paths.LIST_SECRETS_PATH, () => {
      called('listSecrets', {})
      return { secrets: [] } satisfies backend.ListSecretsResponseBody
    })
    await get(paths.LIST_TAGS_PATH, () => {
      called('listTags', {})
      return { tags: labels } satisfies backend.ListTagsResponseBody
    })
    await get(paths.LIST_USERS_PATH, async (route) => {
      called('listUsers', {})
      if (currentUser != null) {
        return { users } satisfies backend.ListUsersResponseBody
      } else {
        await route.fulfill({ status: HTTP_STATUS_BAD_REQUEST })
        return
      }
    })
    await get(paths.LIST_USER_GROUPS_PATH, async (route) => {
      called('listUserGroups', {})
      await route.fulfill({ json: userGroups })
    })

    // === Endpoints with dummy implementations ===
    await get(paths.getProjectDetailsPath(GLOB_PROJECT_ID), (_route, _, [maybeId], params) => {
      if (!maybeId) return
      const presigned = params.get('presigned') === 'true'
      const projectId = backend.ProjectId(maybeId)
      called('getProjectDetails', { projectId, presigned })
      const project = assetMap.get(projectId)

      if (!project) {
        throw new Error(`Cannot get details for a project that does not exist. Project ID: ${projectId} \n
        Please make sure that you've created the project before opening it.
        ------------------------------------------------------------------------------------------------
        
        Existing projects: ${Array.from(assetMap.values())
          .filter((asset) => asset.type === backend.AssetType.project)
          .map((asset) => asset.id)
          .join(', ')}`)
      }
      if (!project.projectState) {
        throw new Error(`Attempting to get a project that does not have a state. Usually it is a bug in the application.
        ------------------------------------------------------------------------------------------------
        Tried to get: \n ${JSON.stringify(project, null, 2)}`)
      }

      const name = project.title ?? 'example project name'
      return {
        organizationId: defaultOrganizationId,
        projectId: projectId,
        name,
        state: project.projectState,
        packageName: 'Project_root',
        address: backend.Address('ws://localhost/'),
        ensoPath: backend.EnsoPath(`enso://Users/${defaultUser.name}/${name}`),
        ...(presigned ? { url: backend.HttpsUrl(`${MOCK_S3_BUCKET_URL}${uniqueString()}`) } : {}),
      } satisfies backend.ProjectRaw
    })

    await get(paths.getAssetDetailsPath(GLOB_ASSET_ID), (route, _, [maybeId]) => {
      const assetId = maybeId && (decodeURIComponent(maybeId) as backend.AssetId)

      if (!assetId) {
        return route.fulfill({
          status: HTTP_STATUS_BAD_REQUEST,
          json: { message: 'Invalid Asset ID' },
        })
      }

      called('getAssetDetails', { assetId })

      const idIsDirectory = backend.isDirectoryId(assetId)

      if (idIsDirectory) {
        const isOrganizationDirectory =
          currentUser?.organizationId != null &&
          organizationIdToDirectoryId(currentUser.organizationId) === assetId
        if (isOrganizationDirectory) {
          return null
        }

        const isUserDirectory =
          currentUser?.userId != null && userIdToDirectoryId(currentUser.userId) === assetId
        if (isUserDirectory) {
          return null
        }

        const isUserGroupDirectory =
          currentUser?.groups != null &&
          currentUser.groups.some((group) => userGroupIdToDirectoryId(group.id) === assetId)

        if (isUserGroupDirectory) {
          return null
        }
      }

      const asset = assetMap.get(assetId)

      if (asset == null) {
        return route.fulfill({
          status: HTTP_STATUS_NOT_FOUND,
          json: { message: 'Asset does not exist' },
        })
      }

      return asset
    })

    // === Endpoints returning `void` ===

    await post(paths.copyAssetPath(GLOB_ASSET_ID), async (route, req, [maybeId]) => {
      /** The type for the JSON request payload for this endpoint. */
      interface Body {
        readonly parentDirectoryId: backend.DirectoryId
      }

      const assetId = maybeId ? (decodeURIComponent(maybeId) as backend.DirectoryId) : null
      // This could be an id for an arbitrary asset, but pretend it's a
      // `DirectoryId` to make TypeScript happy.
      const asset = assetId != null ? assetMap.get(assetId) : null
      if (asset == null) {
        if (assetId == null) {
          await route.fulfill({
            status: HTTP_STATUS_BAD_REQUEST,
            json: { message: 'Invalid Asset ID' },
          })
        } else {
          await route.fulfill({
            status: HTTP_STATUS_NOT_FOUND,
            json: { message: 'Asset does not exist' },
          })
        }
      } else {
        const body: Body = req.postDataJSON()
        const parentId = body.parentDirectoryId
        called('copyAsset', { assetId: assetId!, parentId })
        // Can be any asset ID.
        const id = `${assetId?.split('-')[0]}-${uniqueString()}` as backend.DirectoryId

        const json: backend.CopyAssetResponse = {
          asset: {
            id,
            parentId,
            title: asset.title + ' (copy)',
          },
        }
        const newAsset = { ...asset }
        newAsset.id = id
        newAsset.parentId = parentId
        newAsset.title += ' (copy)'
        addAsset(newAsset)

        return json
      }
    })

    await get(paths.INVITATION_PATH, (): backend.ListInvitationsResponseBody => {
      called('listInvitations', {})
      return {
        invitations: [],
        availableLicenses: totalSeats - usersMap.size,
        maxLicenses: totalSeats,
      }
    })
    await post(paths.INVITE_USER_PATH, async (route) => {
      called('inviteUser', {})
      await route.fulfill()
    })
    await post(paths.CREATE_PERMISSION_PATH, async (route) => {
      called('createPermission', {})
      await route.fulfill()
    })
    await post(paths.closeProjectPath(GLOB_PROJECT_ID), async (route, _, [id]) => {
      if (!id) return
      const projectId = backend.ProjectId(id)
      called('closeProject', { projectId })
      const project = assetMap.get(projectId)
      if (project?.projectState) {
        object.unsafeMutable(project.projectState).type = backend.ProjectState.closed
      }
      await route.fulfill()
    })
    await post(paths.openProjectPath(GLOB_PROJECT_ID), async (route, _, [maybeId]) => {
      if (!maybeId) return
      const projectId = backend.ProjectId(maybeId)
      called('openProject', { projectId })

      const project = assetMap.get(projectId)

      if (!project) {
        throw new Error(
          `Tried to open a project that does not exist. Project ID: ${projectId} \n Please make sure that you've created the project before opening it.`,
        )
      }

      if (project?.projectState) {
        object.unsafeMutable(project.projectState).type = backend.ProjectState.opened
      }

      route.fulfill()
    })
    await post(
      paths.getHybridSetOpenInProgressPath(GLOB_PROJECT_ID),
      async (route, _, [maybeId]) => {
        if (!maybeId) return
        const projectId = backend.ProjectId(maybeId)
        called('hybridSetOpenInProgress', { projectId })

        const project = assetMap.get(projectId)

        if (!project) {
          throw new Error(
            `Tried to open a project that does not exist. Project ID: ${projectId} \n Please make sure that you've created the project before opening it.`,
          )
        }

        const projectSessionId = backend.ProjectSessionId('projectsession-0000')
        if (project?.projectState) {
          object.unsafeMutable(project.projectState).type = backend.ProjectState.openInProgress
          object.unsafeMutable(project.projectState).currentSessionId = projectSessionId
        }

        return { projectSessionId }
      },
    )
    await post(paths.getHybridSetOpenedPath(GLOB_PROJECT_ID), async (route, _, [id]) => {
      if (!id) return
      const projectId = backend.ProjectId(id)
      called('hybridSetOpened', { projectId })

      const project = assetMap.get(projectId)

      if (!project) {
        throw new Error(
          `Tried to open a project that does not exist. Project ID: ${projectId} \n Please make sure that you've created the project before opening it.`,
        )
      }

      if (project?.projectState) {
        object.unsafeMutable(project.projectState).type = backend.ProjectState.opened
      }

      route.fulfill()
    })
    await delete_(paths.deleteTagPath(GLOB_TAG_ID), async (route, _, [id]) => {
      if (!id) return
      const tagId = backend.TagId(id)
      called('deleteTag', { tagId })
      await route.fulfill()
    })
    await post(paths.POST_LOG_EVENT_PATH, async (route) => {
      called('postLogEvent', {})
      await route.fulfill()
    })

    // === Entity creation endpoints ===

    await put(paths.UPLOAD_USER_PICTURE_PATH, async (route, request) => {
      const content = request.postData()
      if (content != null) {
        called('uploadUserPicture', { content })
        currentProfilePicture = content
        return null
      } else {
        await route.fallback()
        return
      }
    })
    await put(paths.UPLOAD_ORGANIZATION_PICTURE_PATH, async (route, request) => {
      const content = request.postData()
      if (content != null) {
        called('uploadOrganizationPicture', { content })
        currentOrganizationProfilePicture = content
        return null
      } else {
        await route.fallback()
        return
      }
    })
    await page.route(MOCK_S3_BUCKET_URL + '**', async (route, request) => {
      if (request.method() === 'PUT') {
        called('s3Put', {})
        await route.fulfill({
          headers: {
            'Access-Control-Expose-Headers': 'ETag',
            ETag: uniqueString(),
          },
        })
      } else if (request.method() === 'GET') {
        called('s3Get', {})
        const body = ''
        await route.fulfill({
          headers: {
            'Content-Type': 'application/octet-stream',
          },
          body,
        })
      } else {
        await route.fallback()
      }
    })
    await post(paths.UPLOAD_FILE_START_PATH, () => {
      const uploadId = backend.FileId('file-' + uniqueString())
      called('uploadFileStart', { uploadId })
      return {
        sourcePath: backend.S3FilePath(''),
        uploadId,
        presignedUrls: Array.from({ length: 10 }, () =>
          backend.HttpsUrl(`${MOCK_S3_BUCKET_URL}${uniqueString()}`),
        ),
      } satisfies backend.UploadLargeFileMetadata
    })
    await post(paths.UPLOAD_FILE_END_PATH, (_route, request) => {
      const body: backend.UploadFileEndRequestBody = request.postDataJSON()
      called('uploadFileEnd', body)

      let id = body.assetId as backend.FileId
      if (!id) {
        const file = addFile({
          id: backend.FileId(body.uploadId),
          title: body.fileName,
          ...(body.parentDirectoryId != null ? { parentId: body.parentDirectoryId } : {}),
        })
        id = file.id
      }

      return { id, project: null, jobId: null } satisfies backend.UploadedAsset
    })

    await post(paths.CREATE_SECRET_PATH, async (_route, request) => {
      const body: backend.CreateSecretRequestBody = await request.postDataJSON()
      called('createSecret', body)
      const secret = addSecret({ title: body.name })
      return secret.id
    })

    // === Other endpoints ===

    await post(paths.CREATE_CHECKOUT_SESSION_PATH, async (_route, request) => {
      const body: backend.CreateCheckoutSessionRequestBody = await request.postDataJSON()
      called('createCheckoutSession', body)
      if (currentUser) {
        object.unsafeMutable(currentUser).plan = body.price
      }
      totalSeats = body.quantity
      subscriptionDuration = body.interval
      return createCheckoutSession(body)
    })

    await patch(paths.updateAssetPath(GLOB_ASSET_ID), (route, request, [id]) => {
      if (!id) throw new Error('updateAssetPath: Missing asset ID')
      const assetId = id as backend.AssetId
      const body: backend.UpdateAssetRequestBody = request.postDataJSON()

      called('updateAsset', { ...body, assetId })

      const asset = assetMap.get(assetId)

      if (asset != null) {
        if (body.description != null) {
          object.unsafeMutable(asset).description = body.description
        }

        if (body.parentDirectoryId != null) {
          object.unsafeMutable(asset).parentId = body.parentDirectoryId
        }

        if (body.title != null) {
          object.unsafeMutable(asset).title = body.title
        }
      }

      return route.fulfill({ json: asset })
    })

    await patch(paths.associateTagPath(GLOB_ASSET_ID), async (_, request, [id]) => {
      if (!id) throw new Error('associateTag: Missing asset ID')
      const assetId = id as backend.AssetId
      /** The type for the JSON request payload for this endpoint. */
      interface Body {
        readonly labels: readonly backend.LabelName[]
      }
      /** The type for the JSON response payload for this endpoint. */
      interface Response {
        readonly tags: readonly backend.Label[]
      }
      const body: Body = await request.postDataJSON()
      called('associateTag', { ...body, assetId })
      setLabels(assetId, body.labels)
      const json: Response = {
        tags: body.labels.flatMap((value) => {
          const label = labelsByValue.get(value)
          return label != null ? [label] : []
        }),
      }
      return json
    })

    await put(paths.updateDirectoryPath(GLOB_DIRECTORY_ID), async (route, request, [maybeId]) => {
      if (!maybeId) return
      const directoryId = maybeId as backend.DirectoryId
      const body: backend.UpdateDirectoryRequestBody = request.postDataJSON()
      called('updateDirectory', { ...body, directoryId })
      const asset = assetMap.get(directoryId)
      if (asset == null) {
        await route.abort()
      } else {
        object.unsafeMutable(asset).title = body.title
        await route.fulfill({
          json: {
            id: directoryId,
            parentId: asset.parentId,
            title: body.title,
          } satisfies backend.UpdatedDirectory,
        })
      }
    })

    await delete_(paths.deleteAssetPath(GLOB_ASSET_ID), async (route, req, [id], params) => {
      if (!id) return
      const force = params.get('force') === 'true'

      // This could be an id for an arbitrary asset, but pretend it's a
      // `DirectoryId` to make TypeScript happy.
      const assetId = decodeURIComponent(id) as backend.DirectoryId

      called('deleteAsset', { assetId, force })

      if (force) {
        forceDeleteAsset(assetId)
      } else {
        deleteAsset(assetId)
      }

      await route.fulfill({ status: HTTP_STATUS_NO_CONTENT })
    })

    await patch(paths.UNDO_DELETE_ASSET_PATH, async (route, request) => {
      /** The type for the JSON request payload for this endpoint. */
      interface Body {
        readonly assetId: backend.AssetId
      }
      const body: Body = await request.postDataJSON()
      called('undoDeleteAsset', body)
      undeleteAsset(body.assetId)
      await route.fulfill({ status: HTTP_STATUS_NO_CONTENT })
    })

    await put(paths.projectUpdatePath(GLOB_PROJECT_ID), async (route, req, [maybeId]) => {
      if (!maybeId) return route.fulfill({ status: HTTP_STATUS_NOT_FOUND })

      const projectId = backend.ProjectId(maybeId)

      const body: backend.UpdateProjectRequestBody = await req.postDataJSON()

      called('updateProject', body)

      const newTitle = body.projectName

      if (newTitle == null) {
        return route.fulfill({ status: HTTP_STATUS_BAD_REQUEST })
      }

      return route.fulfill({
        json: editAsset(projectId, { title: newTitle }),
      })
    })

    await post(paths.CREATE_USER_PATH, async (_route, request) => {
      const body: backend.CreateUserRequestBody = await request.postDataJSON()

      const organizationId = body.organizationId ?? defaultUser.organizationId
      const rootDirectoryId = organizationIdToDirectoryId(organizationId)
      called('createUser', body)

      currentUser = {
        email: body.userEmail,
        name: body.userName,
        organizationId,
        userId: backend.UserId(`user-${uniqueString()}`),
        isEnabled: true,
        rootDirectoryId,
        userGroups: null,
        isOrganizationAdmin: true,
        isEnsoTeamMember: true,
        plan: backend.Plan.free,
      }
      return currentUser
    })

    await post(paths.CREATE_USER_GROUP_PATH, async (_route, request) => {
      const body: backend.CreateUserGroupRequestBody = await request.postDataJSON()
      called('createUserGroup', body)
      const userGroup = addUserGroup(body.name)
      return userGroup
    })

    await put(paths.changeUserGroupPath(GLOB_USER_ID), async (route, request, [maybeId]) => {
      if (!maybeId) return
      const userId = backend.UserId(decodeURIComponent(maybeId))
      // The type of the body sent by this app is statically known.
      const body: backend.ChangeUserGroupRequestBody = await request.postDataJSON()
      called('changeUserGroup', { userId, ...body })
      const user = usersMap.get(userId)
      if (!user) {
        await route.fulfill({ status: HTTP_STATUS_BAD_REQUEST })
      } else {
        object.unsafeMutable(user).userGroups = body.userGroups
        return user
      }
    })
    await put(paths.UPDATE_CURRENT_USER_PATH, async (_route, request) => {
      const body: backend.UpdateUserRequestBody = await request.postDataJSON()

      called('updateCurrentUser', body)

      if (currentUser && body.username != null) {
        currentUser = { ...currentUser, name: body.username }
      }

      return currentUser
    })
    await get(paths.USERS_ME_PATH, (route) => {
      called('usersMe', {})
      if (currentUser == null) {
        return route.fulfill({ status: HTTP_STATUS_NOT_FOUND })
      }

      return currentUser
    })
    await patch(paths.UPDATE_ORGANIZATION_PATH, async (route, request) => {
      const body: backend.UpdateOrganizationRequestBody = await request.postDataJSON()
      called('updateOrganization', body)
      if (body.name === '') {
        await route.fulfill({
          status: HTTP_STATUS_BAD_REQUEST,
          json: { message: 'Organization name must not be empty' },
        })
        return
      } else if (currentOrganization) {
        currentOrganization = { ...currentOrganization, ...body }
        return currentOrganization satisfies backend.OrganizationInfo
      } else {
        await route.fulfill({ status: HTTP_STATUS_NOT_FOUND })
        return
      }
    })
    await get(paths.GET_ORGANIZATION_PATH, async (route) => {
      called('getOrganization', {})
      await route.fulfill({
        json: currentOrganization,
        status: currentOrganization == null ? 404 : 200,
      })
    })
    await post(paths.CREATE_TAG_PATH, (route) => {
      const body: backend.CreateTagRequestBody = route.request().postDataJSON()
      called('createTag', body)
      return addLabel(body.value, body.color)
    })
    await post(paths.CREATE_PROJECT_PATH, (_route, request) => {
      const body: backend.CreateProjectRequestBody = request.postDataJSON()
      called('createProject', body)
      const id = backend.ProjectId(`project-${uniqueString()}`)
      const parentId = body.parentDirectoryId ?? backend.DirectoryId(`directory-${uniqueString()}`)

      const state = { type: backend.ProjectState.closed, volumeId: '' }

      const project = addProject({
        id,
        modifiedAt: newDate(),
        parentId,
        permissions: [
          {
            user: {
              organizationId: defaultOrganizationId,
              userId: defaultUserId,
              name: defaultUsername,
              email: defaultEmail,
            },
            permission: permissions.PermissionAction.own,
          },
        ],
        projectState: state,
      })

      return {
        name: project.title,
        projectId: project.id,
        state: project.projectState,
        organizationId: defaultOrganizationId,
        packageName: 'Project_root',
        ensoPath: project.ensoPath,
      } satisfies backend.CreatedProject
    })

    await post(paths.CREATE_DIRECTORY_PATH, (_route, request) => {
      const body: backend.CreateDirectoryRequestBody = request.postDataJSON()

      called('createDirectory', body)

      const id = backend.DirectoryId(`directory-${uniqueString()}`)
      const parentId = body.parentId ?? defaultDirectoryId

      const directory = addDirectory({
        id,
        parentId,
        projectState: null,
      })

      return {
        title: directory.title,
        id: directory.id,
        parentId: directory.parentId,
      }
    })

    await get(paths.getProjectAssetPath(GLOB_PROJECT_ID, '**'), (route, _, [maybeId, path]) => {
      if (!maybeId || !path) return
      const projectId = backend.ProjectId(maybeId)
      called('resolveProjectAssetData', { projectId, path })
      const content = readFileSync(join(__dirname, '../mock/project', path ?? ''), 'utf8')

      return route.fulfill({
        body: content,
        contentType: 'text/plain',
      })
    })

    await get(paths.RESOLVE_ENSO_PATH, (route, _request, _captures, params) => {
      const path = params.get('path')
      const userRoot = `enso://Users/${currentUser?.name}`
      if (!path?.startsWith(userRoot)) {
        route.fulfill({ status: HTTP_STATUS_BAD_REQUEST, json: { message: 'Invalid enso path' } })
        return
      }
      for (const asset of assetMap.values()) {
        if (asset.ensoPath === path) {
          const { type: _type, ...rest } = asset
          return rest
        }
      }
      route.fulfill({
        status: HTTP_STATUS_NOT_FOUND,
        json: {
          message: `Path '${path}' does not resolve to any asset. Available paths: ${assets.map((asset) => `'${asset.ensoPath}'`).join(', ')}`,
        },
      })
    })

    await page.route('*', async (route) => {
      if (!isOnline) {
        await route.abort('connectionfailed')
      }
    })
  })

  const api = {
    defaultEmail,
    defaultName: defaultUsername,
    defaultOrganization,
    defaultOrganizationId,
    defaultOrganizationName,
    defaultUser,
    defaultUserId,
    rootDirectoryId: defaultDirectoryId,
    get assetCount() {
      return assetMap.size
    },
    goOffline: () => {
      isOnline = false
    },
    goOnline: () => {
      isOnline = true
    },
    setPlan: (plan: backend.Plan) => {
      if (currentUser) {
        object.unsafeMutable(currentUser).plan = plan
      }
    },
    currentUser: () => currentUser,
    setCurrentUser: (user: backend.User | null) => {
      currentUser = user
    },
    currentPassword: () => currentPassword,
    currentProfilePicture: () => currentProfilePicture,
    currentOrganization: () => currentOrganization,
    setCurrentOrganization: (organization: backend.OrganizationInfo | null) => {
      currentOrganization = organization
    },
    currentOrganizationProfilePicture: () => currentOrganizationProfilePicture,
    listDirectory,
    listRootDirectory,
    addAsset,
    deleteAsset,
    editAsset,
    undeleteAsset,
    addDirectory,
    addProject,
    addFile,
    addSecret,
    addDatalink,
    createLabel,
    addLabel,
    setLabels,
    createCheckoutSession,
    addUser,
    deleteUser,
    addUserGroup,
    deleteUserGroup,
    createUserPermission,
    createUserGroupPermission,
    // TODO:
    // addPermission,
    // deletePermission,
    addUserGroupToUser,
    removeUserGroupFromUser,
    trackCalls,
  } as const

  return api
}
