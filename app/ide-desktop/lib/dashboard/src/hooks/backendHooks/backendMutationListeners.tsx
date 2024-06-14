/** @file Listen to all mutations and update state as appropriate when they succeed. */
import * as store from '#/store'

import * as backendHooks from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'
import * as permissions from '#/utilities/permissions'

// =================
// === Constants ===
// =================

const createAssetObject = <T extends Partial<backendModule.AnyAsset>>(
  user: backendModule.User | null,
  rest: T
) => ({
  description: null,
  labels: [],
  permissions: permissions.tryGetSingletonOwnerPermission(user),
  projectState: null,
  modifiedAt: dateTime.toRfc3339(new Date()),
  ...rest,
})

/** All listeners for every backend method that is a mutation. */
export const BACKEND_MUTATION_LISTENERS: Readonly<{
  [Method in backendHooks.MutationMethodInternal]: MutationListener<Method> | null
}> = {
  // === Users ===

  updateUser: (_data, variables, setQueryData) => {
    const [body] = variables
    setQueryData('usersMe', [], currentUser =>
      currentUser == null ? null : { ...currentUser, name: body.username ?? currentUser.name }
    )
  },
  uploadUserPicture: (data, _variables, setQueryData, backend) => {
    backendHooks.revokeUserPictureUrl(backend)
    setQueryData('usersMe', [], () => data)
  },

  // === Organizations ===

  updateOrganization: (data, _variables, setQueryData) => {
    setQueryData('getOrganization', [], () => data)
  },
  uploadOrganizationPicture: (data, _variables, setQueryData, backend) => {
    backendHooks.revokeOrganizationPictureUrl(backend)
    setQueryData('getOrganization', [], () => data)
  },

  // === User groups ===

  createUserGroup: (data, _variables, setQueryData) => {
    setQueryData('listUserGroups', [], userGroups => [data, ...userGroups])
  },
  deleteUserGroup: (_data, variables, setQueryData) => {
    setQueryData('listUserGroups', [], userGroups =>
      userGroups.filter(userGroup => userGroup.id !== variables[0])
    )
  },
  changeUserGroup: (_data, variables, setQueryData) => {
    const [userId, body] = variables
    setQueryData('listUsers', [], users =>
      users.map(otherUser =>
        otherUser.userId !== userId ? otherUser : { ...otherUser, userGroups: body.userGroups }
      )
    )
  },

  // === Tags ===

  createTag: (data, _variables, setQueryData) => {
    setQueryData('listTags', [], tags => [...tags, data])
  },
  deleteTag: (_data, variables, setQueryData) => {
    const [tagId] = variables
    setQueryData('listTags', [], tags => tags.filter(tag => tag.id !== tagId))
  },

  // === Projects ===

  waitUntilProjectIsReady: (data, variables, _setQueryData, backend, session) => {
    const project = data
    const [, parentId] = variables
    store.useStore.getState().setProjectStartupInfo({
      project,
      parentId: parentId ?? backend.rootDirectoryId(session.user) ?? backendModule.DirectoryId(''),
      backendType: backend.type,
      accessToken: session.accessToken ?? null,
    })
  },

  // === Create assets ===

  createDirectory: (data, variables, setQueryData, _backend, session) => {
    const [body] = variables

    setQueryData(
      'listDirectory',
      [
        {
          parentId: data.parentId,
          filterBy: backendModule.FilterBy.active,
          labels: [],
          recentProjects: false,
        },
      ],
      items => [
        ...items,
        createAssetObject(session.user, {
          type: backendModule.AssetType.directory,
          id: data.id,
          title: body.title,
          parentId: data.parentId,
        }),
      ]
    )
  },
  createProject: (data, variables, setQueryData, backend, session) => {
    const [body] = variables

    const parentId = body.parentDirectoryId ?? backend.rootDirectoryId(session.user)
    if (parentId != null) {
      setQueryData(
        'listDirectory',
        [
          {
            parentId,
            filterBy: backendModule.FilterBy.active,
            labels: [],
            recentProjects: false,
          },
        ],
        items => [
          ...items,
          createAssetObject(session.user, {
            type: backendModule.AssetType.project,
            id: data.projectId,
            title: data.name,
            parentId,
            projectState: data.state,
          }),
        ]
      )
    }
  },
  createDatalink: (data, variables, setQueryData, backend, session) => {
    const [body] = variables

    const parentId = body.parentDirectoryId ?? backend.rootDirectoryId(session.user)
    if (parentId != null) {
      setQueryData(
        'listDirectory',
        [
          {
            parentId,
            filterBy: backendModule.FilterBy.active,
            labels: [],
            recentProjects: false,
          },
        ],
        items => [
          ...items,
          createAssetObject(session.user, {
            type: backendModule.AssetType.datalink,
            id: data.id,
            title: body.name,
            parentId,
          }),
        ]
      )
    }
  },
  createSecret: (data, variables, setQueryData, backend, session) => {
    const [body] = variables
    const id = data
    const parentId = body.parentDirectoryId ?? backend.rootDirectoryId(session.user)
    if (parentId != null) {
      setQueryData(
        'listDirectory',
        [
          {
            parentId,
            filterBy: backendModule.FilterBy.active,
            labels: [],
            recentProjects: false,
          },
        ],
        items => [
          ...items,
          createAssetObject(session.user, {
            type: backendModule.AssetType.secret,
            id,
            title: body.name,
            parentId,
          }),
        ]
      )
    }
  },

  // === Update assets ===

  uploadFile: (data, variables, setQueryData, backend, session) => {
    const [body] = variables

    const parentId = body.parentDirectoryId ?? backend.rootDirectoryId(session.user)
    if (parentId != null) {
      setQueryData(
        'listDirectory',
        [
          {
            parentId,
            filterBy: backendModule.FilterBy.active,
            labels: [],
            recentProjects: false,
          },
        ],
        items => [
          ...items,
          data.project == null
            ? createAssetObject(session.user, {
                type: backendModule.AssetType.file,
                id: data.id,
                title: body.fileName,
                parentId,
              })
            : createAssetObject(session.user, {
                type: backendModule.AssetType.project,
                id: data.project.projectId,
                title: data.project.name,
                projectState: data.project.state,
                parentId,
              }),
        ]
      )
    }
  },
  updateAsset: (_data, variables, setQueryData) => {
    const [id, body] = variables
    setQueryData(
      'listDirectory',
      [
        {
          parentId: body.parentDirectoryId,
          filterBy: backendModule.FilterBy.active,
          labels: [],
          recentProjects: false,
        },
      ],
      items =>
        items.map(item =>
          item.id !== id ? item : { ...item, description: body.description ?? item.description }
        )
    )
  },

  // === Delete assets ===

  deleteAsset: (_data, variables, setQueryData, backend, session) => {
    const [id, body] = variables
    // This IIFE is required so that TypeScript does not eagerly narrow the type of this
    // variable.
    let found = ((): backendModule.AnyAsset | null => null)()
    setQueryData(
      'listDirectory',
      [
        {
          parentId: body.parentId,
          filterBy: backendModule.FilterBy.active,
          labels: [],
          recentProjects: false,
        },
      ],
      items =>
        items.filter(item => {
          if (item.id !== id) {
            return false
          } else {
            found = item
            return true
          }
        })
    )
    const rootId = backend.rootDirectoryId(session.user)
    if (!body.force && rootId != null) {
      if (found != null) {
        const deletedItem = found
        setQueryData(
          'listDirectory',
          [
            {
              parentId: rootId,
              filterBy: backendModule.FilterBy.trashed,
              labels: [],
              recentProjects: false,
            },
          ],
          items => [...items, deletedItem]
        )
      } else {
        // `ensureQueryData` is NOT an option here, because if the mutation is finished
        // then the asset is no longer in its original directory.
        void backendHooks.invalidateBackendQuery(queryClient, backend, 'listDirectory', [
          {
            parentId: rootId,
            filterBy: backendModule.FilterBy.trashed,
            labels: null,
            recentProjects: false,
          },
        ])
      }
    }
  },
  undoDeleteAsset: (_data, variables, setQueryData, backend, session) => {
    const [id] = variables
    // This IIFE is required so that TypeScript does not eagerly narrow the type of this
    // variable.
    let found = ((): backendModule.AnyAsset | null => null)()
    const rootId = backend.rootDirectoryId(session.user)
    if (rootId != null) {
      setQueryData(
        'listDirectory',
        [
          {
            parentId: rootId,
            filterBy: backendModule.FilterBy.trashed,
            labels: [],
            recentProjects: false,
          },
        ],
        items =>
          items.filter(item => {
            if (item.id !== id) {
              return false
            } else {
              found = item
              return true
            }
          })
      )
      if (found != null) {
        const deletedItem = found
        setQueryData(
          'listDirectory',
          [
            {
              parentId: found.parentId,
              filterBy: backendModule.FilterBy.active,
              labels: [],
              recentProjects: false,
            },
          ],
          items => [...items, deletedItem]
        )
      }
    }
  },

  // FIXME: Implement the rest.
  associateTag: null,
  closeProject: null,
  copyAsset: null,
  createPermission: null,
  createUser: null,
  deleteDatalink: null,
  deleteInvitation: null,
  deleteUser: null,
  duplicateProject: null,
  inviteUser: null,
  logEvent: null,
  openProject: null,
  removeUser: null,
  resendInvitation: null,
  updateDirectory: null,
  updateFile: null,
  updateProject: null,
  updateSecret: null,

  // === Ignored methods ===
  // These methods do not require any internal state to be updated.
  createCheckoutSession: null,
}

// ========================
// === MutationListener ===
// ========================

/** A listener for a specific backend method that is a mutation. */
export type MutationListener<Method extends backendHooks.MutationMethodInternal> = (
  data: Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
  variables: Parameters<Extract<Backend[Method], (...args: never) => unknown>>,
  setQueryData: <QueryMethod extends backendHooks.QueryMethod>(
    method: QueryMethod,
    key: [...Parameters<Extract<Backend[QueryMethod], (...args: never) => unknown>>, ...unknown[]],
    updater: (
      variable: Awaited<ReturnType<Extract<Backend[QueryMethod], (...args: never) => unknown>>>
    ) => Awaited<ReturnType<Extract<Backend[QueryMethod], (...args: never) => unknown>>>
  ) => void,
  backend: Backend,
  session: Exclude<authProvider.UserSession, authProvider.PartialUserSession>
) => void
