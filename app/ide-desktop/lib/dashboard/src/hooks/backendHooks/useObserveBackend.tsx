/** @file Listen to all mutations and update state as appropriate when they succeed. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as backendHooks from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'
import * as permissions from '#/utilities/permissions'

// =========================
// === useObserveBackend ===
// =========================

/** Listen to all mutations and update state as appropriate when they succeed.
 * MUST be unconditionally called exactly once for each backend type. */
export function useObserveBackend(backend: Backend | null) {
  const queryClient = reactQuery.useQueryClient()
  const [seen] = React.useState(new WeakSet())
  const session = authProvider.useUserSession()
  const user = session != null && 'user' in session ? session.user : null
  const useObserveMutations = <Method extends keyof Backend>(
    method: Method,
    onSuccess: (
      state: reactQuery.MutationState<
        Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>,
        Error,
        Parameters<Extract<Backend[Method], (...args: never) => unknown>>
      >
    ) => void
  ) => {
    const states = reactQuery.useMutationState<
      Parameters<Extract<Backend[Method], (...args: never) => unknown>>
    >({
      // Errored mutations can be safely ignored as they should not change the state.
      filters: { mutationKey: [backend, method], status: 'success' },
      // eslint-disable-next-line no-restricted-syntax
      select: mutation => mutation.state as never,
    })
    for (const state of states) {
      if (!seen.has(state)) {
        seen.add(state)
        // This is SAFE - it is just too highly dynamic for TypeScript to typecheck.
        // eslint-disable-next-line no-restricted-syntax
        onSuccess(state as never)
      }
    }
  }
  const setQueryData = <Method extends keyof Backend>(
    method: Method,
    updater: (
      variable: Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    ) => Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  ) => {
    queryClient.setQueryData<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >([backend, method], data => (data == null ? data : updater(data)))
  }
  const setQueryDataWithKey = <Method extends keyof Backend>(
    method: Method,
    key: [...Parameters<Extract<Backend[Method], (...args: never) => unknown>>, ...unknown[]],
    updater: (
      variable: Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    ) => Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
  ) => {
    queryClient.setQueryData<
      Awaited<ReturnType<Extract<Backend[Method], (...args: never) => unknown>>>
    >([backend, method, ...key], data => (data == null ? data : updater(data)))
  }

  // === Users ===

  useObserveMutations('updateUser', state => {
    if (state.variables != null) {
      const [body] = state.variables
      setQueryData('usersMe', currentUser =>
        currentUser == null ? null : { ...currentUser, name: body.username ?? currentUser.name }
      )
    }
  })
  useObserveMutations('uploadUserPicture', state => {
    if (state.data != null) {
      backendHooks.revokeUserPictureUrl(backend)
      const data = state.data
      setQueryData('usersMe', () => data)
    }
  })

  // === Organizations ===

  useObserveMutations('updateOrganization', state => {
    if (state.data != null) {
      const data = state.data
      setQueryData('getOrganization', () => data)
    }
  })
  useObserveMutations('uploadOrganizationPicture', state => {
    if (state.data != null) {
      backendHooks.revokeOrganizationPictureUrl(backend)
      const data = state.data
      setQueryData('getOrganization', () => data)
    }
  })

  // === User groups ===

  useObserveMutations('createUserGroup', state => {
    if (state.data != null) {
      const data = state.data
      setQueryData('listUserGroups', userGroups => [data, ...userGroups])
    }
  })
  useObserveMutations('deleteUserGroup', state => {
    setQueryData('listUserGroups', userGroups =>
      userGroups.filter(userGroup => userGroup.id !== state.variables?.[0])
    )
  })
  useObserveMutations('changeUserGroup', state => {
    if (state.variables != null) {
      const [userId, body] = state.variables
      setQueryData('listUsers', users =>
        users.map(otherUser =>
          otherUser.userId !== userId ? otherUser : { ...otherUser, userGroups: body.userGroups }
        )
      )
    }
  })

  // === Tags ===

  useObserveMutations('createTag', state => {
    if (state.data != null) {
      const data = state.data
      setQueryData('listTags', tags => [...tags, data])
    }
  })
  useObserveMutations('deleteTag', state => {
    if (state.variables != null) {
      const [tagId] = state.variables
      setQueryData('listTags', tags => tags.filter(tag => tag.id !== tagId))
    }
  })

  // === Create assets ===

  const createAssetObject = <T extends Partial<backendModule.AnyAsset>>(rest: T) => ({
    description: null,
    labels: [],
    permissions: permissions.tryGetSingletonOwnerPermission(user),
    projectState: null,
    modifiedAt: dateTime.toRfc3339(new Date()),
    ...rest,
  })

  useObserveMutations('createDirectory', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const data = state.data
      setQueryDataWithKey(
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
          createAssetObject({
            type: backendModule.AssetType.directory,
            id: data.id,
            title: body.title,
            parentId: data.parentId,
          }),
        ]
      )
    }
  })
  useObserveMutations('createProject', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const data = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey(
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
            createAssetObject({
              type: backendModule.AssetType.project,
              id: data.projectId,
              title: data.name,
              parentId,
              projectState: data.state,
            }),
          ]
        )
      }
    }
  })
  useObserveMutations('createDatalink', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const data = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey(
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
            createAssetObject({
              type: backendModule.AssetType.datalink,
              id: data.id,
              title: body.name,
              parentId,
            }),
          ]
        )
      }
    }
  })
  useObserveMutations('createSecret', state => {
    if (state.variables != null && state.data != null) {
      const [body] = state.variables
      const id = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey(
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
            createAssetObject({
              type: backendModule.AssetType.secret,
              id,
              title: body.name,
              parentId,
            }),
          ]
        )
      }
    }
  })

  // === Update assets ===

  useObserveMutations('uploadFile', state => {
    if (state.data != null && state.variables != null) {
      const [body] = state.variables
      const data = state.data
      const parentId = body.parentDirectoryId ?? backend?.rootDirectoryId(user)
      if (parentId != null) {
        setQueryDataWithKey(
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
              ? createAssetObject({
                  type: backendModule.AssetType.file,
                  id: data.id,
                  title: body.fileName,
                  parentId,
                })
              : createAssetObject({
                  type: backendModule.AssetType.project,
                  id: data.project.projectId,
                  title: data.project.name,
                  projectState: data.project.state,
                  parentId,
                }),
          ]
        )
      }
    }
  })
  useObserveMutations('updateAsset', state => {
    if (state.data != null && state.variables != null) {
      const [id, body] = state.variables
      setQueryDataWithKey(
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
    }
  })

  // === Delete assets ===

  useObserveMutations('deleteAsset', state => {
    if (state.variables != null) {
      const [id, body] = state.variables
      // This IIFE is required so that TypeScript does not eagerly narrow the type of this
      // variable.
      let found = ((): backendModule.AnyAsset | null => null)()
      setQueryDataWithKey(
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
      if (!body.force) {
        if (found != null) {
          const deletedItem = found
          setQueryDataWithKey(
            'listDirectory',
            [
              {
                parentId: body.parentId,
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
          void invalidateBackendQuery(queryClient, backend, 'listDirectory', [
            {
              parentId: body.parentId,
              filterBy: backendModule.FilterBy.trashed,
              labels: null,
              recentProjects: false,
            },
          ])
        }
      }
    }
  })
  useObserveMutations('undoDeleteAsset', state => {
    if (state.variables != null) {
      const [id] = state.variables
      // This IIFE is required so that TypeScript does not eagerly narrow the type of this
      // variable.
      let found = ((): backendModule.AnyAsset | null => null)()
      setQueryDataWithKey(
        'listDirectory',
        [
          {
            parentId: body.parentId,
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
        setQueryDataWithKey(
          'listDirectory',
          [
            {
              parentId: body.parentId,
              filterBy: backendModule.FilterBy.active,
              labels: [],
              recentProjects: false,
            },
          ],
          items => [...items, deletedItem]
        )
      }
    }
  })
}
