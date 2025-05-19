/** @file A list of previous versions of an asset. */
import { Result } from '#/components/Result'
import { AssetPanelPlaceholder } from '#/layouts/AssetPanel/components/AssetPanelPlaceholder'
import { useText } from '#/providers/TextProvider'
import { AssetType, BackendType, type ProjectAsset } from '#/services/Backend'
import { useSuspenseQuery } from '@tanstack/react-query'
import { useAssetPanelCurrentItem } from '../AssetPanelState'
import { ProjectSession } from './ProjectSession'
import type { AssetPanelProps } from './types'

/** Props for a {@link ProjectSessions}. */
export interface ProjectSessionsProps extends AssetPanelProps {}

/** A list of previous versions of an asset. */
export function ProjectSessions(props: ProjectSessionsProps) {
  const { backend } = props
  const { getText } = useText()

  const item = useAssetPanelCurrentItem()

  if (backend.type === BackendType.local) {
    return <AssetPanelPlaceholder title={getText('assetProjectSessions.localBackend')} />
  }

  if (item == null) {
    return <AssetPanelPlaceholder title={getText('assetProjectSessions.notSelected')} />
  }

  if (item.type !== AssetType.project) {
    return <AssetPanelPlaceholder title={getText('assetProjectSessions.notProjectAsset')} />
  }

  return <AssetProjectSessionsInternal {...props} item={item} />
}

/** Props for a {@link AssetProjectSessionsInternal}. */
interface AssetProjectSessionsInternalProps extends ProjectSessionsProps {
  readonly item: ProjectAsset
}

/** A list of previous versions of an asset. */
function AssetProjectSessionsInternal(props: AssetProjectSessionsInternalProps) {
  const { backend, item } = props
  const { getText } = useText()

  const projectSessionsQuery = useSuspenseQuery({
    queryKey: ['getProjectSessions', item.id, item.title],
    queryFn: async () => {
      const sessions = await backend.listProjectSessions(item.id, item.title)
      return [...sessions].reverse()
    },
  })

  return projectSessionsQuery.data.length === 0 ?
      <Result status="info" centered title={getText('assetProjectSessions.noSessions')} />
    : <div className="flex w-full flex-col justify-start">
        {projectSessionsQuery.data.map((session, i) => (
          <ProjectSession
            key={session.projectSessionId}
            backend={backend}
            project={item}
            projectSession={session}
            index={projectSessionsQuery.data.length - i}
          />
        ))}
      </div>
}
