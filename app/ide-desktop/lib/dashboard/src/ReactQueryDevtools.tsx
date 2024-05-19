/**
 * @file
 *
 * ReactQueryDevtools component. Shows the React Query Devtools.
 */
import * as React from 'react'

import * as reactQueryDevtools from '@tanstack/react-query-devtools'

const ReactQueryDevtoolsProduction = React.lazy(() =>
  import('@tanstack/react-query-devtools/build/modern/production.js').then(d => ({
    default: d.ReactQueryDevtools,
  }))
)

/**
 * ReactQueryDevtools component.
 * Shows the React Query Devtools and provide ability to show them in production.
 */
export function ReactQueryDevtools() {
  const [showDevtools, setShowDevtools] = React.useState(false)

  React.useEffect(() => {
    window.toggleDevtools = () => {
      setShowDevtools(old => !old)
    }
  }, [])

  return (
    <>
      <reactQueryDevtools.ReactQueryDevtools />

      {showDevtools && (
        <React.Suspense fallback={null}>
          <ReactQueryDevtoolsProduction />
        </React.Suspense>
      )}
    </>
  )
}
