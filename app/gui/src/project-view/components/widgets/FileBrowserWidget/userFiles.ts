/** @file Support for getting information about a user's cloud files. */
import { backendQueryOptions } from '#/hooks/backendHooks'
import RemoteBackend from '#/services/RemoteBackend'
import { createUsersMeQuery } from '$/providers/auth'
import { type ToValue } from '@/util/reactivity'
import * as vueQuery from '@tanstack/vue-query'
import type Backend from 'enso-common/src/services/Backend'
import type { DirectoryId } from 'enso-common/src/services/Backend'
import { computed, toValue, type Ref } from 'vue'

export interface UserFiles {
  rootPath: ToValue<string>
  rootDirectoryId: ToValue<DirectoryId>
  /** Path to the user's home, relative to `rootPath`. */
  home: ToValue<string[]>
}

/** @returns An API for getting information about the logged-in user's files. */
export function useUserFiles(backend: ToValue<Backend | null>): {
  userFiles: Readonly<Ref<UserFiles | null>>
  userFilesError: Readonly<Ref<Error | null>>
} {
  const data = computed(() => {
    const backendValue = toValue(backend)
    if (!(backendValue instanceof RemoteBackend)) return null
    return {
      backend: backendValue,
      user: vueQuery.useQuery(createUsersMeQuery(backendValue)),
      organization: vueQuery.useQuery(backendQueryOptions(backendValue, 'getOrganization', [])),
    }
  })

  return {
    userFiles: computed<UserFiles | null>(() => {
      if (!data.value) return null
      const { backend, user: userQuery, organization: organizationQuery } = data.value
      if (!userQuery.isFetched.value || !organizationQuery.isFetched.value) return null
      const user = userQuery.data.value
      if (!user) return null
      const organization = organizationQuery.data.value
      return {
        rootPath: computed<string>(() => backend.rootPath(user)),
        rootDirectoryId: computed<DirectoryId>(() => {
          return (
            (organization && backend.rootDirectoryId(user, organization)) ?? user.rootDirectoryId
          )
        }),
        /** The user's home directory. */
        home: computed<string[]>(() => ['Users', user.name]),
      }
    }),
    userFilesError: computed(
      (): Error | null =>
        data.value?.user.error.value ?? data.value?.organization.error.value ?? null,
    ),
  }
}
