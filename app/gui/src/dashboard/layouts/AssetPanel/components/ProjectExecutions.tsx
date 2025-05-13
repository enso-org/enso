/** @file A list of exeuctions of a project. */
import { useSuspenseQuery } from '@tanstack/react-query'

import { Button, DialogTrigger, Text } from '#/components/AriaComponents'
import { listProjectExecutionsQueryOptions } from '#/hooks/backendHooks'
import { AssetPanelPlaceholder } from '#/layouts/AssetPanel/components/AssetPanelPlaceholder'
import { NewProjectExecutionModal } from '#/layouts/NewProjectExecutionModal'
import { useText } from '#/providers/TextProvider'
import { AssetType, BackendType, type ProjectAsset } from '#/services/Backend'
import { useAssetPanelCurrentItem } from '../AssetPanelState'
import { ProjectExecution } from './ProjectExecution'
import type { AssetPanelProps } from './types'

/** Props for a {@link ProjectExecutions}. */
export interface ProjectExecutionsProps extends AssetPanelProps {}

/** A list of exeuctions of a project. */
export function ProjectExecutions(props: ProjectExecutionsProps) {
  const { backend } = props
  const { getText } = useText()

  const item = useAssetPanelCurrentItem()

  if (backend.type === BackendType.local) {
    return <AssetPanelPlaceholder title={getText('assetProjectExecutions.localBackend')} />
  }
  if (item == null) {
    return <AssetPanelPlaceholder title={getText('assetProjectExecutions.notSelected')} />
  }
  if (item.type !== AssetType.project) {
    return <AssetPanelPlaceholder title={getText('assetProjectExecutions.notProjectAsset')} />
  }
  return <ProjectExecutionsInternal {...props} item={item} />
}

/** Props for a {@link ProjectExecutionsInternal}. */
interface ProjectExecutionsInternalProps extends ProjectExecutionsProps {
  readonly item: ProjectAsset
}

/** A list of exeuctions of a project. */
function ProjectExecutionsInternal(props: ProjectExecutionsInternalProps) {
  const { backend, item } = props
  const { getText } = useText()

  const projectExecutionsQuery = useSuspenseQuery(
    listProjectExecutionsQueryOptions(backend, item.id, item.title),
  )
  const projectExecutions = projectExecutionsQuery.data

  return (
    <div className="flex w-full flex-col items-center gap-2 self-start overflow-y-auto overflow-x-hidden">
      <DialogTrigger>
        <Button variant="outline">{getText('newProjectExecution')}</Button>

        <NewProjectExecutionModal backend={backend} item={item} />
      </DialogTrigger>

      {projectExecutions.length === 0 && (
        <Text color="disabled">{getText('noProjectExecutions')}</Text>
      )}

      {projectExecutions.map((execution) => (
        <ProjectExecution
          key={execution.executionId}
          item={item}
          backend={backend}
          projectExecution={execution}
        />
      ))}
    </div>
  )
}
