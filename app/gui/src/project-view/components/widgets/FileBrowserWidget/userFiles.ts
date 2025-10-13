/** @file Support for getting information about a user's cloud files. */
import type { Opt } from '@/util/data/opt'
import type { ToValue } from '@/util/reactivity'
import type { DirectoryId, OrganizationInfo, User } from 'enso-common/src/services/Backend'
import { computed, toValue, type Ref } from 'vue'

interface UserFilesBackend {
  rootPath: (user: User) => string
  rootDirectoryId: (user: User, organization: OrganizationInfo | null) => DirectoryId | null
}

interface QueryResult<T> {
  data: ToValue<T>
  isFetched: ToValue<boolean>
  error: ToValue<Error | null>
}

export interface UserFiles {
  rootPath: ToValue<string>
  rootDirectoryId: ToValue<DirectoryId>
  /** Path to the user's home. */
  home: ToValue<string[]>
}

/** @returns An API for getting information about the logged-in user's files. */
export function useUserFiles({
  backend,
  user,
  organization,
}: {
  backend: ToValue<UserFilesBackend | null>
  user: QueryResult<Opt<User>>
  organization: QueryResult<Opt<OrganizationInfo>>
}): { userFiles: Readonly<Ref<UserFiles | null>>; userFilesError: Readonly<Ref<Error | null>> } {
  function userFiles(backend: UserFilesBackend, user: User): UserFiles {
    return {
      rootPath: computed<string>(() => backend.rootPath(user)),
      rootDirectoryId: computed<DirectoryId>(() => {
        const currentOrganization = toValue(organization.data)
        return (
          (currentOrganization && backend.rootDirectoryId(user, currentOrganization)) ??
          user.rootDirectoryId
        )
      }),
      /** The user's home directory. */
      home: computed<string[]>(() => ['Users', user.name]),
    }
  }

  return {
    userFiles: computed<UserFiles | null>(() => {
      if (!toValue(user.isFetched) || !toValue(organization.isFetched)) return null
      const currentBackend = toValue(backend)
      if (!currentBackend) return null
      const currentUser = toValue(user.data)
      if (!currentUser) return null
      return userFiles(currentBackend, currentUser)
    }),
    userFilesError: computed<Error | null>(
      () => toValue(user.error) ?? toValue(organization.error),
    ),
  }
}
