/** @file Register listeners for all backend types. */
import * as React from 'react'

import { matchMutation, useQueryClient, type Mutation } from '@tanstack/react-query'

import { BackendType, type User } from 'enso-common/src/services/Backend'

import { useBackendMutationListener } from '#/hooks/backendHooks'

import { useAuth } from '#/providers/AuthProvider'

// ==============================
// === BackendRequestListener ===
// ==============================

/** Register listeners for all backend types. */
export default function BackendRequestListener() {
  const { setUser } = useAuth()
  useBackendMutationListener(BackendType.remote)
  useBackendMutationListener(BackendType.local)

  const queryClient = useQueryClient()
  const mutationCache = queryClient.getMutationCache()
  React.useEffect(
    () =>
      mutationCache.subscribe((event) => {
        const mutation: Mutation | undefined = event.mutation
        if (
          (event.type === 'added' || event.type === 'updated') &&
          mutation?.state.status === 'success' &&
          (matchMutation({ mutationKey: [BackendType.remote, 'uploadUserPicture'] }, mutation) ||
            matchMutation({ mutationKey: [BackendType.local, 'uploadUserPicture'] }, mutation))
        ) {
          // eslint-disable-next-line no-restricted-syntax
          setUser(mutation.state.data as Partial<User>)
        }
      }),
    [mutationCache, setUser],
  )

  return null
}
