/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import AssetProjectSession from '#/layouts/AssetProjectSession'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as loader from '#/components/Loader'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

// ============================
// === AssetProjectSessions ===
// ============================

/** Props for a {@link AssetProjectSessions}. */
export interface AssetProjectSessionsProps {
  readonly backend: Backend
  readonly item: backendModule.ProjectAsset
}

/** A list of previous versions of an asset. */
export default function AssetProjectSessions(props: AssetProjectSessionsProps) {
  return (
    <errorBoundary.ErrorBoundary>
      <React.Suspense fallback={<loader.Loader />}>
        <AssetProjectSessionsInternal {...props} />
      </React.Suspense>
    </errorBoundary.ErrorBoundary>
  )
}

/** A list of previous versions of an asset. */
function AssetProjectSessionsInternal(props: AssetProjectSessionsProps) {
  const { backend, item } = props

  const projectSessionsQuery = reactQuery.useSuspenseQuery({
    queryKey: ['getProjectSessions', item.id, item.title],
    queryFn: async () => {
      const sessions = await backend.listProjectSessions(item.id, item.title)
      return [...sessions].reverse()
    },
  })

  return (
    <div className="pointer-events-auto flex flex-col items-center overflow-y-auto overflow-x-hidden">
      {projectSessionsQuery.data.map((session) => (
        <AssetProjectSession
          key={session.projectSessionId}
          backend={backend}
          project={item}
          projectSession={session}
        />
      ))}
    </div>
  )
}
