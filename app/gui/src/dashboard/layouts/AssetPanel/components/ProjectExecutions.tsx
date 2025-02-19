/** @file A list of exeuctions of a project. */
import { queryOptions, useSuspenseQuery } from '@tanstack/react-query'

import { Button, DialogTrigger, Text } from '#/components/AriaComponents'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { useStore } from '#/hooks/storeHooks'
import { assetPanelStore } from '#/layouts/AssetPanel/AssetPanelState'
import { AssetPanelPlaceholder } from '#/layouts/AssetPanel/components/AssetPanelPlaceholder'
import { NewProjectExecutionModal } from '#/layouts/NewProjectExecutionModal'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { AssetType, BackendType, type ProjectAsset } from '#/services/Backend'
import { ProjectExecution } from './ProjectExecution'

/** Props for a {@link ProjectExecutions}. */
export interface ProjectExecutionsProps {
  readonly backend: Backend
}

/** A list of exeuctions of a project. */
export function ProjectExecutions(props: ProjectExecutionsProps) {
  const { backend } = props
  const { getText } = useText()
  const { item } = useStore(assetPanelStore, (state) => ({ item: state.assetPanelProps.item }), {
    unsafeEnableTransition: true,
  })

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
    queryOptions({
      ...backendQueryOptions(backend, 'listProjectExecutions', [item.id, item.title]),
      select: (executions) => [...executions].reverse(),
    }),
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
