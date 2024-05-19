/**
 * @file
 *
 * ReactQueryDevtools component. Shows the React Query Devtools.
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
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
  // It's safitier to pass the client directly to the devtools
  // Since there might be a chance that we might have multiple versions if react-query
  // in case if we forgot to update the devtools or if npm messed up with the versions
  // or hoising issues.
  const client = reactQuery.useQueryClient()

  React.useEffect(() => {
    window.toggleDevtools = () => {
      setShowDevtools(old => !old)
    }
  }, [])

  return (
    <>
      <reactQueryDevtools.ReactQueryDevtools client={client} />

      {showDevtools && (
        <React.Suspense fallback={null}>
          <ReactQueryDevtoolsProduction client={client} />
        </React.Suspense>
      )}
    </>
  )
}
