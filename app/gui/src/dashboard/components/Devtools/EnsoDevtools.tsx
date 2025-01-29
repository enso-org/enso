/**
 * @file
 *
 * A component that provides a UI for toggling paywall features.
 */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import { useEffect } from 'react'
import { ensoDevtoolsStore, useShowEnsoDevtools } from './EnsoDevtoolsProvider'

const EnsoDevtoolsImpl = React.lazy(() =>
  import('./EnsoDevtoolsImpl').then((mod) => ({ default: mod.EnsoDevtools })),
)

/** A component that provides a UI for toggling paywall features. */
export function EnsoDevtools() {
  const { isEnsoTeamMember } = authProvider.useUser()

  const showEnsoDevtools = useShowEnsoDevtools()

  useEffect(() => {
    if (isEnsoTeamMember) {
      addToggleDevtoolsToWindow()
    }
  }, [isEnsoTeamMember])

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

/**
 * Adds the `toggleDevtools` function to the window object.
 */
function addToggleDevtoolsToWindow() {
  if (typeof window !== 'undefined') {
    window.toggleDevtools = ensoDevtoolsStore.getState().toggleDevtools
  }
}
