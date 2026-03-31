/** @file UI for editing application state. */
import { useShowEnsoDevtools } from '$/providers/react/devTools'
import * as React from 'react'

const EnsoDevtoolsImpl =
  process.env.NODE_ENV === 'development' ?
    React.lazy(() => import('./EnsoDevtoolsImpl').then((mod) => ({ default: mod.EnsoDevtools })))
  : () => null
const EnsoDevStatus =
  process.env.NODE_ENV === 'development' ?
    React.lazy(() => import('./EnsoDevtoolsImpl').then((mod) => ({ default: mod.EnsoDevStatus })))
  : () => null

/** UI for editing application state */
export function EnsoDevtools() {
  const [showEnsoDevtools] = useShowEnsoDevtools()

  return (
    <>
      {showEnsoDevtools && <EnsoDevtoolsImpl />}
      <EnsoDevStatus />
    </>
  )
}
