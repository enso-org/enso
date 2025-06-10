/**
 * @file
 *
 * A component that provides a UI for toggling paywall features.
 */
import * as React from 'react'

import * as authProvider from '$/providers/react/auth'
import { useEffect } from 'react'
import { ensoDevtoolsStore, useShowEnsoDevtools } from './EnsoDevtoolsProvider'

const EnsoDevtoolsImpl = React.lazy(() =>
  import('./EnsoDevtoolsImpl').then((mod) => ({ default: mod.EnsoDevtools })),
)
const EnsoDevStatus = React.lazy(() =>
  import('./EnsoDevtoolsImpl').then((mod) => ({ default: mod.EnsoDevStatus })),
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

  if (!showEnsoDevtools) {
    return null
  }

  return (
    <>
      <EnsoDevtoolsImpl />
      <EnsoDevStatus />
    </>
  )
}

/**
 * Adds the `toggleDevtools` function to the window object.
 */
function addToggleDevtoolsToWindow() {
  if (typeof window !== 'undefined') {
    window.toggleDevtools = ensoDevtoolsStore.getState().toggleDevtools
  }
}
