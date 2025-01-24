/**
 * @file
 *
 * A component that provides a UI for toggling paywall features.
 */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import { useShowEnsoDevtools } from './EnsoDevtoolsProvider'

const EnsoDevtoolsImpl = React.lazy(() =>
  import('./EnsoDevtoolsImpl').then((mod) => ({ default: mod.EnsoDevtools })),
)

/** A component that provides a UI for toggling paywall features. */
export function EnsoDevtools() {
  const { isEnsoTeamMember } = authProvider.useUser()

  const showEnsoDevtools = useShowEnsoDevtools()

  const shouldDisplayDevtools = (() => {
    if (showEnsoDevtools == null) {
      return isEnsoTeamMember
    }

    return showEnsoDevtools
  })()

  if (!shouldDisplayDevtools) {
    return null
  }

  return <EnsoDevtoolsImpl />
}
