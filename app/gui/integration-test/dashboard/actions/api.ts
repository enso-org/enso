/** @file The mock API. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'
import type * as remoteBackend from '#/services/RemoteBackend'
import * as remoteBackendPaths from '#/services/remoteBackendPaths'

import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as dateTime from 'enso-common/src/utilities/data/dateTime'
import * as uniqueString from 'enso-common/src/utilities/uniqueString'

import * as actions from '.'

import type { FeatureFlags } from '#/providers/FeatureFlagsProvider'
import { organizationIdToDirectoryId } from '#/services/RemoteBackend'
import { readFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import invariant from 'tiny-invariant'

// =================
// === Constants ===
// =================

const __dirname = dirname(fileURLToPath(import.meta.url))

const MOCK_SVG = `
<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 100 100">
  <defs>
    <pattern id="checkerboard" width="20" height="20" patternUnits="userSpaceOnUse">
      <rect width="10" height="10" fill="white"/>
      <rect x="10" y="0" width="10" height="10" fill="black"/>
      <rect x="0" y="10" width="10" height="10" fill="black"/>
      <rect x="10" y="10" width="10" height="10" fill="white"/>
    </pattern>
  </defs>
  <rect width="100" height="100" fill="url(#checkerboard)"/>
</svg>
`

/** The HTTP status code representing a response with an empty body. */
const HTTP_STATUS_NO_CONTENT = 204
/** The HTTP status code representing a bad request. */
const HTTP_STATUS_BAD_REQUEST = 400
/** The HTTP status code representing a URL that does not exist. */
const HTTP_STATUS_NOT_FOUND = 404
/** A user id that is a path glob. */
const GLOB_USER_ID = backend.UserId('*')
/** An asset ID that is a path glob. */
const GLOB_ASSET_ID: backend.AssetId = '*' as backend.DirectoryId
/** A directory ID that is a path glob. */
const GLOB_DIRECTORY_ID = '*' as backend.DirectoryId
/** A project ID that is a path glob. */
const GLOB_PROJECT_ID = backend.ProjectId('*')
/** A tag ID that is a path glob. */
const GLOB_TAG_ID = backend.TagId('*')
/** A checkout session ID that is a path glob. */
const GLOB_CHECKOUT_SESSION_ID = backend.CheckoutSessionId('*')
const BASE_URL = 'https://mock/'
const MOCK_S3_BUCKET_URL = 'https://mock-s3-bucket.com/'

function array<T>(): Readonly<T>[] {
  return []
}

const INITIAL_CALLS_OBJECT = {
  changePassword: array<{ oldPassword: string; newPassword: string }>(),
  listDirectory: array<{
    parent_id?: string
    filter_by?: backend.FilterBy
    labels?: backend.LabelName[]
    recent_projects?: boolean
  }>(),
  listFiles: array<object>(),
  listProjects: array<object>(),
  listSecrets: array<object>(),
  listTags: array<object>(),
  listUsers: array<object>(),
  listUserGroups: array<object>(),
  getProjectDetails: array<{ projectId: backend.ProjectId }>(),
  copyAsset: array<{ assetId: backend.AssetId; parentId: backend.DirectoryId }>(),
  listInvitations: array<object>(),
  inviteUser: array<object>(),
  createPermission: array<object>(),
  closeProject: array<{ projectId: backend.ProjectId }>(),
  openProject: array<{ projectId: backend.ProjectId }>(),
  deleteTag: array<{ tagId: backend.TagId }>(),
  postLogEvent: array<object>(),
  uploadUserPicture: array<{ content: string }>(),
  uploadOrganizationPicture: array<{ content: string }>(),
  s3Put: array<object>(),
  uploadFileStart: array<{ uploadId: backend.FileId }>(),
  uploadFileEnd: array<backend.UploadFileEndRequestBody>(),
  createSecret: array<backend.CreateSecretRequestBody>(),
  createCheckoutSession: array<backend.CreateCheckoutSessionRequestBody>(),
  getCheckoutSession: array<{
    body: backend.CreateCheckoutSessionRequestBody
    status: backend.CheckoutSessionStatus
  }>(),
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
  getProjectContent: array<{ projectId: backend.ProjectId }>(),
  getProjectAsset: array<{ projectId: backend.ProjectId }>(),
  updateProject: array<backend.UpdateProjectRequestBody>(),
}

const READONLY_INITIAL_CALLS_OBJECT: TrackedCallsInternal = INITIAL_CALLS_OBJECT

export { READONLY_INITIAL_CALLS_OBJECT as INITIAL_CALLS_OBJECT }

type TrackedCallsInternal = {
  [K in keyof typeof INITIAL_CALLS_OBJECT]: Readonly<(typeof INITIAL_CALLS_OBJECT)[K]>
}

export interface TrackedCalls extends TrackedCallsInternal {}

/** Parameters for {@link mockApi}. */
export interface MockParams {
  readonly page: test.Page
  readonly setupAPI?: SetupAPI | null | undefined
}

/**
 * Setup function for the mock API.
 * use it to setup the mock API with custom handlers.
 */
export interface SetupAPI {
  (api: Awaited<ReturnType<typeof mockApi>>): Promise<void> | void
}

/** The return type of {@link mockApi}. */
export interface MockApi extends Awaited<ReturnType<typeof mockApiInternal>> {}

export const mockApi: (params: MockParams) => Promise<MockApi> = mockApiInternal

/** Add route handlers for the mock API to a page. */
async function mockApiInternal({ page, setupAPI }: MockParams) {
  const defaultEmail = 'email@example.com' as backend.EmailAddress
  const defaultUsername = 'user name'
  const defaultPassword = actions.VALID_PASSWORD
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

  const checkoutSessionsMap = new Map<
    backend.CheckoutSessionId,
    {
      readonly body: backend.CreateCheckoutSessionRequestBody
      readonly status: backend.CheckoutSessionStatus
    }
  >()

  usersMap.set(defaultUser.userId, defaultUser)

  function getParentPath(parentId: backend.DirectoryId, acc: string[] = []) {
    const parent = assetMap.get(parentId)

    if (parent == null) {
      return [parentId, ...acc].join('/')
    }

    // this should never happen, but we need to check it for a case
    invariant(parent.type === backend.AssetType.directory, 'Parent is not a directory')

    return getParentPath(parent.parentId, [parent.id, ...acc])
  }

  function getVirtualParentPath(
    parentId: backend.DirectoryId,
    _parentTitle: string,
    acc: string[] = [],
  ) {
    const parent = assetMap.get(parentId)

    if (parent == null) {
      return acc.join('/')
    }

    // this should never happen, but we need to check it for a case
    invariant(parent.type === backend.AssetType.directory, 'Parent is not a directory')

    return getVirtualParentPath(parent.parentId, parent.title, [parent.title, ...acc])
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
  ): backend.UserGroupPermission =>
    object.merge(
      {
        userGroup,
        permission,
      },
      rest,
    )

  const createDirectory = (rest: Partial<backend.DirectoryAsset> = {}): backend.DirectoryAsset => {
    const parentId = rest.parentId ?? defaultDirectoryId

    const directoryTitles = new Set(
      assets
        .filter((asset) => asset.type === backend.AssetType.directory)
        .map((asset) => asset.title),
    )

    const title = rest.title ?? `New Folder ${directoryTitles.size + 1}`

    const directory = object.merge(
      {
        type: backend.AssetType.directory,
        id: backend.DirectoryId(`directory-${uniqueString.uniqueString()}` as const),
        projectState: null,
        extension: null,
        title,
        modifiedAt: dateTime.toRfc3339(new Date()),
        description: rest.description ?? '',
        labels: [],
        parentId,
        permissions: [createUserPermission(defaultUser, permissions.PermissionAction.own)],
        parentsPath: '',
        virtualParentsPath: '',
      },
      rest,
    )

    Object.defineProperty(directory, 'toJSON', {
      value: function toJSON() {
        const { parentsPath: _, virtualParentsPath: __, ...rest } = this

        return {
          ...rest,
          parentsPath: this.parentsPath,
          virtualParentsPath: this.virtualParentsPath,
        }
      },
    })

    Object.defineProperty(directory, 'parentsPath', {
      get: () => getParentPath(directory.parentId),
    })

    Object.defineProperty(directory, 'virtualParentsPath', {
      get: () => getVirtualParentPath(directory.id, directory.title),
    })

    return directory
  }

  const createProject = (rest: Partial<backend.ProjectAsset> = {}): backend.ProjectAsset => {
    const projectNames = new Set(
      assets
        .filter((asset) => asset.type === backend.AssetType.project)
        .map((asset) => asset.title),
    )

    const title = rest.title ?? `New Project ${projectNames.size + 1}`

    const project = object.merge(
      {
        type: backend.AssetType.project,
        id: backend.ProjectId('project-' + uniqueString.uniqueString()),
        projectState: {
          type: backend.ProjectState.closed,
          volumeId: '',
        },
        extension: null,
        title,
        modifiedAt: dateTime.toRfc3339(new Date()),
        description: rest.description ?? '',
        labels: [],
        parentId: defaultDirectoryId,
        permissions: [createUserPermission(defaultUser, permissions.PermissionAction.own)],
        parentsPath: '',
        virtualParentsPath: '',
      },
      rest,
    )
    Object.defineProperty(project, 'toJSON', {
      value: function toJSON() {
        const { parentsPath: _, virtualParentsPath: __, ...rest } = this

        return {
          ...rest,
          parentsPath: this.parentsPath,
          virtualParentsPath: this.virtualParentsPath,
        }
      },
    })

    Object.defineProperty(project, 'parentsPath', {
      get: () => getParentPath(project.parentId),
    })

    Object.defineProperty(project, 'virtualParentsPath', {
      get: () => getVirtualParentPath(project.parentId, project.title),
    })

    return project
  }

  const createFile = (rest: Partial<backend.FileAsset> = {}): backend.FileAsset => {
    const file = object.merge(
      {
        type: backend.AssetType.file,
        id: backend.FileId('file-' + uniqueString.uniqueString()),
        projectState: null,
        extension: '',
        title: rest.title ?? '',
        modifiedAt: dateTime.toRfc3339(new Date()),
        description: rest.description ?? '',
        labels: [],
        parentId: defaultDirectoryId,
        permissions: [createUserPermission(defaultUser, permissions.PermissionAction.own)],
        parentsPath: '',
        virtualParentsPath: '',
      },
      rest,
    )

    Object.defineProperty(file, 'toJSON', {
      value: function toJSON() {
        const { parentsPath: _, virtualParentsPath: __, ...rest } = this

        return {
          ...rest,
          parentsPath: this.parentsPath,
          virtualParentsPath: this.virtualParentsPath,
        }
      },
    })

    Object.defineProperty(file, 'parentsPath', {
      get: () => getParentPath(file.parentId),
    })

    Object.defineProperty(file, 'virtualParentsPath', {
      get: () => getVirtualParentPath(file.parentId, file.title),
    })

    return file
  }

  const createSecret = (rest: Partial<backend.SecretAsset>): backend.SecretAsset => {
    const secret = object.merge(
      {
        type: backend.AssetType.secret,
        id: backend.SecretId('secret-' + uniqueString.uniqueString()),
        projectState: null,
        extension: null,
        title: rest.title ?? '',
        modifiedAt: dateTime.toRfc3339(new Date()),
        description: rest.description ?? '',
        labels: [],
        parentId: defaultDirectoryId,
        permissions: [createUserPermission(defaultUser, permissions.PermissionAction.own)],
        parentsPath: '',
        virtualParentsPath: '',
      },
      rest,
    )

    Object.defineProperty(secret, 'toJSON', {
      value: function toJSON() {
        const { parentsPath: _, virtualParentsPath: __, ...rest } = this

        return {
          ...rest,
          parentsPath: this.parentsPath,
          virtualParentsPath: this.virtualParentsPath,
        }
      },
    })

    Object.defineProperty(secret, 'parentsPath', {
      get: () => getParentPath(secret.parentId),
    })

    Object.defineProperty(secret, 'virtualParentsPath', {
      get: () => getVirtualParentPath(secret.parentId, secret.title),
    })

    return secret
  }

  const createDatalink = (rest: Partial<backend.DatalinkAsset>): backend.DatalinkAsset => {
    const datalink = object.merge(
      {
        type: backend.AssetType.datalink,
        id: backend.DatalinkId('datalink-' + uniqueString.uniqueString()),
        projectState: null,
        extension: null,
        title: rest.title ?? '',
        modifiedAt: dateTime.toRfc3339(new Date()),
        description: rest.description ?? '',
        labels: [],
        parentId: defaultDirectoryId,
        permissions: [createUserPermission(defaultUser, permissions.PermissionAction.own)],
        parentsPath: '',
        virtualParentsPath: '',
      },
      rest,
    )

    Object.defineProperty(datalink, 'toJSON', {
      value: function toJSON() {
        const { parentsPath: _, virtualParentsPath: __, ...rest } = this

        return {
          ...rest,
          parentsPath: this.parentsPath,
          virtualParentsPath: this.virtualParentsPath,
        }
      },
    })

    Object.defineProperty(datalink, 'parentsPath', {
      get: () => getParentPath(datalink.parentId),
    })

    Object.defineProperty(datalink, 'virtualParentsPath', {
      get: () => getVirtualParentPath(datalink.parentId, datalink.title),
    })

    return datalink
  }

  const createLabel = (value: string, color: backend.LChColor): backend.Label => ({
    id: backend.TagId('tag-' + uniqueString.uniqueString()),
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

  const createCheckoutSession = (
    body: backend.CreateCheckoutSessionRequestBody,
    rest: Partial<backend.CheckoutSessionStatus> = {},
  ) => {
    const id = backend.CheckoutSessionId(`checkoutsession-${uniqueString.uniqueString()}`)
    const status = rest.status ?? 'trialing'
    const paymentStatus = status === 'trialing' ? 'no_payment_needed' : 'unpaid'
    const checkoutSessionStatus = {
      status,
      paymentStatus,
      ...rest,
    } satisfies backend.CheckoutSessionStatus
    checkoutSessionsMap.set(id, { body, status: checkoutSessionStatus })
    return {
      id,
      clientSecret: '',
    } satisfies backend.CheckoutSession
  }

  const addUser = (name: string, rest: Partial<backend.User> = {}) => {
    const organizationId = currentOrganization?.id ?? defaultOrganizationId
    const user: backend.User = {
      userId: backend.UserId(`user-${uniqueString.uniqueString()}`),
      name,
      email: backend.EmailAddress(`${name}@example.org`),
      organizationId,
      rootDirectoryId: organizationIdToDirectoryId(organizationId),
      isEnabled: true,
      userGroups: null,
      plan: backend.Plan.enterprise,
      isOrganizationAdmin: true,
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
      id: backend.UserGroupId(`usergroup-${uniqueString.uniqueString()}` as const),
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

  await test.test.step('Mock API', async () => {
    const method =
      (theMethod: string) =>
      async (url: string, callback: (route: test.Route, request: test.Request) => unknown) => {
        await page.route(BASE_URL + url, async (route, request) => {
          if (request.method() !== theMethod) {
            await route.fallback()
          } else {
            const result = await callback(route, request)
            // `null` counts as a JSON value that we will want to return.
            if (result !== undefined) {
              await route.fulfill({ json: result })
            }
          }
        })
      }
    const get = method('GET')
    const put = method('PUT')
    const post = method('POST')
    const patch = method('PATCH')
    const delete_ = method('DELETE')

    await page.route(BASE_URL + '**', (_route, request) => {
      throw new Error(
        `Missing route handler for '${request.method()} ${request.url().replace(BASE_URL, '')}'.`,
      )
    })

    // === Mock Cognito endpoints ===

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

    await get(remoteBackendPaths.LIST_DIRECTORY_PATH + '*', (route, request) => {
      /** The type for the search query for this endpoint. */
      interface Query {
        readonly parent_id?: string
        readonly filter_by?: backend.FilterBy
        readonly labels?: backend.LabelName[]
        readonly recent_projects?: boolean
      }
      const query = Object.fromEntries(
        new URL(request.url()).searchParams.entries(),
      ) as unknown as Query
      called('listDirectory', query)
      const parentId = query.parent_id ?? defaultDirectoryId
      let filteredAssets = assets.filter((asset) => asset.parentId === parentId)

      // This lint rule is broken; there is clearly a case for `undefined` below.
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
        case null: {
          // do nothing
          break
        }
        case undefined: {
          // do nothing
          break
        }
      }
      filteredAssets.sort(
        (a, b) => backend.ASSET_TYPE_ORDER[a.type] - backend.ASSET_TYPE_ORDER[b.type],
      )
      const json: remoteBackend.ListDirectoryResponseBody = { assets: filteredAssets }

      route.fulfill({ json })
    })
    await get(remoteBackendPaths.LIST_FILES_PATH + '*', () => {
      called('listFiles', {})
      return { files: [] } satisfies remoteBackend.ListFilesResponseBody
    })
    await get(remoteBackendPaths.LIST_PROJECTS_PATH + '*', () => {
      called('listProjects', {})
      return { projects: [] } satisfies remoteBackend.ListProjectsResponseBody
    })
    await get(remoteBackendPaths.LIST_SECRETS_PATH + '*', () => {
      called('listSecrets', {})
      return { secrets: [] } satisfies remoteBackend.ListSecretsResponseBody
    })
    await get(remoteBackendPaths.LIST_TAGS_PATH + '*', () => {
      called('listTags', {})
      return { tags: labels } satisfies remoteBackend.ListTagsResponseBody
    })
    await get(remoteBackendPaths.LIST_USERS_PATH + '*', async (route) => {
      called('listUsers', {})
      if (currentUser != null) {
        return { users } satisfies remoteBackend.ListUsersResponseBody
      } else {
        await route.fulfill({ status: HTTP_STATUS_BAD_REQUEST })
        return
      }
    })
    await get(remoteBackendPaths.LIST_USER_GROUPS_PATH + '*', async (route) => {
      called('listUserGroups', {})
      await route.fulfill({ json: userGroups })
    })

    // === Endpoints with dummy implementations ===
    await get(remoteBackendPaths.getProjectDetailsPath(GLOB_PROJECT_ID), (_route, request) => {
      const maybeId = request.url().match(/[/]projects[/]([^?/]+)/)?.[1]
      if (!maybeId) return
      const projectId = backend.ProjectId(maybeId)
      called('getProjectDetails', { projectId })
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

      return {
        organizationId: defaultOrganizationId,
        projectId: projectId,
        name: 'example project name',
        state: project.projectState,
        packageName: 'Project_root',
        // eslint-disable-next-line camelcase
        ide_version: null,
        // eslint-disable-next-line camelcase
        engine_version: {
          value: '2023.2.1-nightly.2023.9.29',
          lifecycle: backend.VersionLifecycle.development,
        },
        address: backend.Address('ws://localhost/'),
      } satisfies backend.ProjectRaw
    })

    // === Endpoints returning `void` ===

    await post(remoteBackendPaths.copyAssetPath(GLOB_ASSET_ID), async (route, request) => {
      /** The type for the JSON request payload for this endpoint. */
      interface Body {
        readonly parentDirectoryId: backend.DirectoryId
      }

      const maybeId = request.url().match(/[/]assets[/]([^?/]+)/)?.[1]
      if (!maybeId) return
      const assetId = maybeId != null ? (decodeURIComponent(maybeId) as backend.DirectoryId) : null
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
        const body: Body = request.postDataJSON()
        const parentId = body.parentDirectoryId
        called('copyAsset', { assetId: assetId!, parentId })
        // Can be any asset ID.
        const id = `${assetId?.split('-')[0]}-${uniqueString.uniqueString()}` as backend.DirectoryId

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

    await get(remoteBackendPaths.INVITATION_PATH + '*', (): backend.ListInvitationsResponseBody => {
      called('listInvitations', {})
      return {
        invitations: [],
        availableLicenses: totalSeats - usersMap.size,
      }
    })
    await post(remoteBackendPaths.INVITE_USER_PATH + '*', async (route) => {
      called('inviteUser', {})
      await route.fulfill()
    })
    await post(remoteBackendPaths.CREATE_PERMISSION_PATH + '*', async (route) => {
      called('createPermission', {})
      await route.fulfill()
    })
    await post(remoteBackendPaths.closeProjectPath(GLOB_PROJECT_ID), async (route, request) => {
      const maybeId = request.url().match(/[/]projects[/]([^?/]+)/)?.[1]
      if (!maybeId) return
      const projectId = backend.ProjectId(maybeId)
      called('closeProject', { projectId })
      const project = assetMap.get(projectId)
      if (project?.projectState) {
        object.unsafeMutable(project.projectState).type = backend.ProjectState.closed
      }
      await route.fulfill()
    })
    await post(remoteBackendPaths.openProjectPath(GLOB_PROJECT_ID), async (route, request) => {
      const maybeId = request.url().match(/[/]projects[/]([^?/]+)/)?.[1]
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
    await delete_(remoteBackendPaths.deleteTagPath(GLOB_TAG_ID), async (route, request) => {
      const maybeId = request.url().match(/[/]projects[/]([^?/]+)/)?.[1]
      if (!maybeId) return
      const tagId = backend.TagId(maybeId)
      called('deleteTag', { tagId })
      await route.fulfill()
    })
    await post(remoteBackendPaths.POST_LOG_EVENT_PATH, async (route) => {
      called('postLogEvent', {})
      await route.fulfill()
    })

    // === Entity creation endpoints ===

    await put(remoteBackendPaths.UPLOAD_USER_PICTURE_PATH + '*', async (route, request) => {
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
    await put(remoteBackendPaths.UPLOAD_ORGANIZATION_PICTURE_PATH + '*', async (route, request) => {
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
      if (request.method() !== 'PUT') {
        called('s3Put', {})
        await route.fallback()
      } else {
        await route.fulfill({
          headers: {
            'Access-Control-Expose-Headers': 'ETag',
            ETag: uniqueString.uniqueString(),
          },
        })
      }
    })
    await post(remoteBackendPaths.UPLOAD_FILE_START_PATH + '*', () => {
      const uploadId = backend.FileId('file-' + uniqueString.uniqueString())
      called('uploadFileStart', { uploadId })
      return {
        sourcePath: backend.S3FilePath(''),
        uploadId,
        presignedUrls: Array.from({ length: 10 }, () =>
          backend.HttpsUrl(`${MOCK_S3_BUCKET_URL}${uniqueString.uniqueString()}`),
        ),
      } satisfies backend.UploadLargeFileMetadata
    })
    await post(remoteBackendPaths.UPLOAD_FILE_END_PATH + '*', (_route, request) => {
      const body: backend.UploadFileEndRequestBody = request.postDataJSON()
      called('uploadFileEnd', body)

      const file = addFile({
        id: backend.FileId(body.uploadId),
        title: body.fileName,
        ...(body.parentDirectoryId != null ? { parentId: body.parentDirectoryId } : {}),
      })

      return { id: file.id, project: null } satisfies backend.UploadedLargeAsset
    })

    await post(remoteBackendPaths.CREATE_SECRET_PATH + '*', async (_route, request) => {
      const body: backend.CreateSecretRequestBody = await request.postDataJSON()
      called('createSecret', body)
      const secret = addSecret({ title: body.name })
      return secret.id
    })

    // === Other endpoints ===

    await post(remoteBackendPaths.CREATE_CHECKOUT_SESSION_PATH + '*', async (_route, request) => {
      const body: backend.CreateCheckoutSessionRequestBody = await request.postDataJSON()
      called('createCheckoutSession', body)
      return createCheckoutSession(body)
    })
    await get(
      remoteBackendPaths.getCheckoutSessionPath(GLOB_CHECKOUT_SESSION_ID) + '*',
      (_route, request) => {
        const checkoutSessionId = request.url().match(/[/]payments[/]subscriptions[/]([^/?]+)/)?.[1]
        if (checkoutSessionId == null) {
          throw new Error('GetCheckoutSession: Missing checkout session ID in path')
        } else {
          const result = checkoutSessionsMap.get(backend.CheckoutSessionId(checkoutSessionId))
          if (result) {
            called('getCheckoutSession', result)
            if (currentUser) {
              object.unsafeMutable(currentUser).plan = result.body.plan
            }
            totalSeats = result.body.quantity
            subscriptionDuration = result.body.interval
            return result.status
          } else {
            throw new Error('GetCheckoutSession: Unknown checkout session ID')
          }
        }
      },
    )

    await patch(remoteBackendPaths.updateAssetPath(GLOB_ASSET_ID), (route, request) => {
      const maybeId = request.url().match(/[/]assets[/]([^?]+)/)?.[1]

      if (!maybeId) throw new Error('updateAssetPath: Missing asset ID in path')
      // This could be an id for an arbitrary asset, but pretend it's a
      // `DirectoryId` to make TypeScript happy.
      const assetId = maybeId as backend.DirectoryId
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
      }

      return route.fulfill({ json: asset })
    })

    await patch(remoteBackendPaths.associateTagPath(GLOB_ASSET_ID), async (_route, request) => {
      const maybeId = request.url().match(/[/]assets[/]([^/?]+)/)?.[1]
      if (!maybeId) return
      // This could be an id for an arbitrary asset, but pretend it's a
      // `DirectoryId` to make TypeScript happy.
      const assetId = maybeId as backend.DirectoryId
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

    await put(remoteBackendPaths.updateDirectoryPath(GLOB_DIRECTORY_ID), async (route, request) => {
      const maybeId = request.url().match(/[/]directories[/]([^?]+)/)?.[1]
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

    await delete_(remoteBackendPaths.deleteAssetPath(GLOB_ASSET_ID), async (route, request) => {
      const force = new URL(request.url()).searchParams.get('force') === 'true'
      const maybeId = request.url().match(/[/]assets[/]([^?]+)/)?.[1]

      if (!maybeId) return

      // This could be an id for an arbitrary asset, but pretend it's a
      // `DirectoryId` to make TypeScript happy.
      const assetId = decodeURIComponent(maybeId) as backend.DirectoryId

      called('deleteAsset', { assetId, force })

      if (force) {
        forceDeleteAsset(assetId)
      } else {
        deleteAsset(assetId)
      }

      await route.fulfill({ status: HTTP_STATUS_NO_CONTENT })
    })

    await patch(remoteBackendPaths.UNDO_DELETE_ASSET_PATH, async (route, request) => {
      /** The type for the JSON request payload for this endpoint. */
      interface Body {
        readonly assetId: backend.AssetId
      }
      const body: Body = await request.postDataJSON()
      called('undoDeleteAsset', body)
      undeleteAsset(body.assetId)
      await route.fulfill({ status: HTTP_STATUS_NO_CONTENT })
    })

    await put(remoteBackendPaths.projectUpdatePath(GLOB_PROJECT_ID), async (route, request) => {
      const maybeId = request.url().match(/[/]projects[/]([^?/]+)/)?.[1]

      if (!maybeId) return route.fulfill({ status: HTTP_STATUS_NOT_FOUND })

      const projectId = backend.ProjectId(maybeId)

      const body: backend.UpdateProjectRequestBody = await request.postDataJSON()

      called('updateProject', body)

      const newTitle = body.projectName

      if (newTitle == null) {
        return route.fulfill({ status: HTTP_STATUS_BAD_REQUEST })
      }

      return route.fulfill({
        json: editAsset(projectId, { title: newTitle }),
      })
    })

    await post(remoteBackendPaths.CREATE_USER_PATH + '*', async (_route, request) => {
      const body: backend.CreateUserRequestBody = await request.postDataJSON()

      const organizationId = body.organizationId ?? defaultUser.organizationId
      const rootDirectoryId = organizationIdToDirectoryId(organizationId)
      called('createUser', body)

      currentUser = {
        email: body.userEmail,
        name: body.userName,
        organizationId,
        userId: backend.UserId(`user-${uniqueString.uniqueString()}`),
        isEnabled: true,
        rootDirectoryId,
        userGroups: null,
        isOrganizationAdmin: true,
      }
      return currentUser
    })

    await post(remoteBackendPaths.CREATE_USER_GROUP_PATH + '*', async (_route, request) => {
      const body: backend.CreateUserGroupRequestBody = await request.postDataJSON()
      called('createUserGroup', body)
      const userGroup = addUserGroup(body.name)
      return userGroup
    })

    await put(
      remoteBackendPaths.changeUserGroupPath(GLOB_USER_ID) + '*',
      async (route, request) => {
        const maybeId = request.url().match(/[/]users[/]([^?/]+)/)?.[1]
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
      },
    )
    await put(remoteBackendPaths.UPDATE_CURRENT_USER_PATH + '*', async (_route, request) => {
      const body: backend.UpdateUserRequestBody = await request.postDataJSON()
      called('updateCurrentUser', body)
      if (currentUser && body.username != null) {
        currentUser = { ...currentUser, name: body.username }
      }
    })
    await get(remoteBackendPaths.USERS_ME_PATH + '*', (route) => {
      called('usersMe', {})
      if (currentUser == null) {
        return route.fulfill({ status: HTTP_STATUS_NOT_FOUND })
      } else {
        return currentUser
      }
    })
    await patch(remoteBackendPaths.UPDATE_ORGANIZATION_PATH + '*', async (route, request) => {
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
    await get(remoteBackendPaths.GET_ORGANIZATION_PATH + '*', async (route) => {
      called('getOrganization', {})
      await route.fulfill({
        json: currentOrganization,
        status: currentOrganization == null ? 404 : 200,
      })
    })
    await post(remoteBackendPaths.CREATE_TAG_PATH + '*', (route) => {
      const body: backend.CreateTagRequestBody = route.request().postDataJSON()
      called('createTag', body)
      return addLabel(body.value, body.color)
    })
    await post(remoteBackendPaths.CREATE_PROJECT_PATH + '*', (_route, request) => {
      const body: backend.CreateProjectRequestBody = request.postDataJSON()
      called('createProject', body)
      const id = backend.ProjectId(`project-${uniqueString.uniqueString()}`)
      const parentId =
        body.parentDirectoryId ??
        backend.DirectoryId(`directory-${uniqueString.uniqueString()}` as const)

      const state = { type: backend.ProjectState.closed, volumeId: '' }

      const project = addProject({
        description: null,
        id,
        labels: [],
        modifiedAt: dateTime.toRfc3339(new Date()),
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
        title: project.title,
        id: project.id,
        parentId: project.parentId,
        state: project.projectState,
        organizationId: defaultOrganizationId,
        packageName: 'Project_root',
        projectId: id,
      }
    })

    await post(remoteBackendPaths.CREATE_DIRECTORY_PATH + '*', (_route, request) => {
      const body: backend.CreateDirectoryRequestBody = request.postDataJSON()

      called('createDirectory', body)

      const id = backend.DirectoryId(`directory-${uniqueString.uniqueString()}` as const)
      const parentId = body.parentId ?? defaultDirectoryId

      const directory = addDirectory({
        description: null,
        id,
        labels: [],
        parentId,
        projectState: null,
      })

      return {
        title: directory.title,
        id: directory.id,
        parentId: directory.parentId,
      }
    })

    await get(remoteBackendPaths.getProjectContentPath(GLOB_PROJECT_ID), (route, request) => {
      const maybeId = request.url().match(/[/]projects[/]([^?/]+)/)?.[1]
      if (!maybeId) return
      const projectId = backend.ProjectId(maybeId)
      called('getProjectContent', { projectId })
      const content = readFileSync(join(__dirname, '../mock/enso-demo.main'), 'utf8')

      return route.fulfill({
        body: content,
        contentType: 'text/plain',
      })
    })

    await get(
      remoteBackendPaths.getProjectAssetPath(GLOB_PROJECT_ID, '*'),
      async (route, request) => {
        const maybeId = request.url().match(/[/]projects[/]([^?/]+)/)?.[1]

        invariant(maybeId, 'Unable to parse the ID provided')

        const projectId = backend.ProjectId(maybeId)

        called('getProjectAsset', { projectId })

        return route.fulfill({
          // This is a mock SVG image. Just a square with a black background.
          path: join(__dirname, '../mock/example.png'),
        })
      },
    )

    await page.route('mock/svg.svg', (route) => {
      return route.fulfill({ body: MOCK_SVG, contentType: 'image/svg+xml' })
    })

    await page.route('**/assets/*.svg', (route) => {
      return route.fulfill({ body: MOCK_SVG, contentType: 'image/svg+xml' })
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
    addAsset,
    deleteAsset,
    editAsset,
    undeleteAsset,
    createDirectory,
    createProject,
    createFile,
    createSecret,
    createDatalink,
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
    setFeatureFlags: (flags: Partial<FeatureFlags>) => {
      return page.addInitScript((flags: Partial<FeatureFlags>) => {
        const currentOverrideFeatureFlags =
          'overrideFeatureFlags' in window && typeof window.overrideFeatureFlags === 'object' ?
            window.overrideFeatureFlags
          : {}

        Object.defineProperty(window, 'overrideFeatureFlags', {
          value: { ...currentOverrideFeatureFlags, ...flags },
          writable: false,
        })
      }, flags)
    },
    // TODO:
    // addPermission,
    // deletePermission,
    addUserGroupToUser,
    removeUserGroupFromUser,
    trackCalls,
  } as const

  if (setupAPI) {
    await setupAPI(api)
  }

  return api
}
