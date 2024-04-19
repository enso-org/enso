/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'

import AssetProjectSession from '#/layouts/AssetProjectSession'

import StatelessSpinner from '#/components/StatelessSpinner'
import * as statelessSpinnerModule from '#/components/StatelessSpinner'

import type * as backendModule from '#/services/Backend'

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
  readonly item: AssetTreeNode<backendModule.ProjectAsset>
}

/** A list of previous versions of an asset. */
export default function AssetProjectSessions(props: AssetProjectSessionsProps) {
  const { item } = props
  const { backend } = backendProvider.useBackend()

  const projectSessions = asyncEffectHooks.useAsyncEffect(
    null,
    () => backend.listProjectSessions(item.item.id, item.item.title),
    [backend, item.item.id, item.item.title]
  )

  return (
    <div className="pointer-events-auto flex flex-col items-center overflow-y-auto overflow-x-hidden">
      {projectSessions == null ? (
        <StatelessSpinner
          state={statelessSpinnerModule.SpinnerState.loadingFast}
          size={SPINNER_SIZE}
        />
      ) : (
        projectSessions.map(session => (
          <AssetProjectSession
            key={session.projectSessionId}
            project={item.item}
            projectSession={session}
          />
        ))
      )}
    </div>
  )
}
