/** @file A list of previous versions of an asset. */
import * as reactQuery from '@tanstack/react-query'

import AssetProjectSession from '#/layouts/AssetProjectSession'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import { Result } from '#/components/Result'
import { useText } from '#/providers/TextProvider'
import type AssetTreeNode from '#/utilities/AssetTreeNode'

// ============================
// === AssetProjectSessions ===
// ============================

/** Props for a {@link AssetProjectSessions}. */
export interface AssetProjectSessionsProps {
  readonly backend: Backend
  readonly item: AssetTreeNode | null
}

/** A list of previous versions of an asset. */
export default function AssetProjectSessions(props: AssetProjectSessionsProps) {
  const { backend, item } = props

  const { getText } = useText()

  if (item == null) {
    return <Result status="info" centered title={getText('assetProjectSessions.notSelected')} />
  }

  if (backend.type === backendModule.BackendType.local) {
    return <Result status="info" centered title={getText('assetProjectSessions.localBackend')} />
  }

  if (item.item.type !== backendModule.AssetType.project) {
    return <Result status="info" centered title={getText('assetProjectSessions.notProjectAsset')} />
  }

  return (
    <AssetProjectSessionsInternal
      {...props}
      // This is safe because we already checked that the asset is a project asset above.
      // eslint-disable-next-line no-restricted-syntax
      item={item as AssetTreeNode<backendModule.ProjectAsset>}
    />
  )
}

// ====================================
// === AssetProjectSessionsInternal ===
// ====================================

/** Props for a {@link AssetProjectSessionsInternal}. */
interface AssetProjectSessionsInternalProps extends AssetProjectSessionsProps {
  readonly item: AssetTreeNode<backendModule.ProjectAsset>
}

/** A list of previous versions of an asset. */
function AssetProjectSessionsInternal(props: AssetProjectSessionsInternalProps) {
  const { backend, item } = props

  const projectSessionsQuery = reactQuery.useSuspenseQuery({
    queryKey: ['getProjectSessions', item.item.id, item.item.title],
    queryFn: async () => {
      const sessions = await backend.listProjectSessions(item.item.id, item.item.title)
      return [...sessions].reverse()
    },
  })

  return (
    <div className="flex flex-col items-center">
      {projectSessionsQuery.data.map((session) => (
        <AssetProjectSession
          key={session.projectSessionId}
          backend={backend}
          project={item.item}
          projectSession={session}
        />
      ))}
    </div>
  )
}
