/** @file A dummy page so that users can enter a URL to log out. */
import * as reactQuery from '@tanstack/react-query'

import * as authProvider from '#/providers/AuthProvider'

// ==============
// === Logout ===
// ==============

/** A dummy page so that users can enter a URL to log out. */
export default function Logout() {
  const { signOut } = authProvider.useAuth()

  reactQuery.useSuspenseQuery({
    queryKey: ['signOut'],
    queryFn: signOut,
    staleTime: 0,
    gcTime: 0,
  })

  // All relevant UI is displayed by `AuthProvider`.
  return null
}
