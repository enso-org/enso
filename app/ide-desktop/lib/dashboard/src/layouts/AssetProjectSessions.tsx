/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import AssetProjectSession from '#/layouts/AssetProjectSession'

import * as loader from '#/components/Loader'

import type * as backendModule from '#/services/Backend'
import Backend from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'

// =================
// === Constants ===
// =================

/** The size (both width and height) of the loading spinner. */
const SPINNER_SIZE = 32

// ============================
// === AssetProjectSessions ===
// ============================

/** Props for a {@link AssetProjectSessions}. */
export interface AssetProjectSessionsProps {
  readonly backend: Backend
  readonly item: AssetTreeNode<backendModule.ProjectAsset>
}

/** A list of previous versions of an asset. */
export default function AssetProjectSessions(props: AssetProjectSessionsProps) {
  const { backend, item } = props

  const projectSessionsQuery = reactQuery.useSuspenseQuery({
    queryKey: ['getProjectSessions', item.item.id, item.item.title],
    queryFn: () => backend.listProjectSessions(item.item.id, item.item.title),
  })

  return (
    <React.Suspense fallback={<loader.Loader />}>
      <div className="pointer-events-auto flex flex-col items-center overflow-y-auto overflow-x-hidden">
        {projectSessionsQuery.data.map(session => (
          <AssetProjectSession
            key={session.projectSessionId}
            backend={backend}
            project={item.item}
            projectSession={session}
          />
        ))}
      </div>
    </React.Suspense>
  )
}
