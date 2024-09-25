/** @file A list of previous versions of an asset. */
import { useSuspenseQuery } from '@tanstack/react-query'

import { Button, ButtonGroup, DialogTrigger, Text } from '#/components/AriaComponents'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { NewProjectExecutionModal } from '#/layouts/AssetPanel/NewProjectExecutionModal'
import ProjectExecution from '#/layouts/AssetPanel/ProjectExecution'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import type { ProjectAsset } from '#/services/Backend'

// =========================
// === ProjectExecutions ===
// =========================

/** Props for a {@link ProjectExecutions}. */
export interface ProjectExecutionsProps {
  readonly backend: Backend
  readonly item: ProjectAsset
}

/** A list of previous versions of a project. */
export default function ProjectExecutions(props: ProjectExecutionsProps) {
  return (
    <ErrorBoundary>
      <Suspense>
        <ProjectExecutionsInternal {...props} />
      </Suspense>
    </ErrorBoundary>
  )
}

// =================================
// === ProjectExecutionsInternal ===
// =================================

/** Props for a {@link ProjectExecutionsInternal}. */
interface ProjectExecutionsInternalProps extends ProjectExecutionsProps {}

/** A list of previous versions of an asset. */
function ProjectExecutionsInternal(props: ProjectExecutionsInternalProps) {
  const { backend, item } = props
  const { getText } = useText()

  const projectExecutionsQuery = useSuspenseQuery({
    queryKey: [backend.type, 'listProjectExecutions', item.id, item.title],
    queryFn: async () => {
      const executions = await backend.listProjectExecutions(item.id, item.title)
      return [...executions].reverse()
    },
  })
  const projectExecutions = projectExecutionsQuery.data

  return (
    <div className="pointer-events-auto flex flex-col items-center gap-2 overflow-y-auto overflow-x-hidden">
      <ButtonGroup>
        <DialogTrigger>
          <Button variant="outline">{getText('newProjectExecution')}</Button>
          <NewProjectExecutionModal backend={backend} item={item} />
        </DialogTrigger>
      </ButtonGroup>
      {projectExecutions.length === 0 ?
        <Text color="disabled">{getText('noProjectExecutions')}</Text>
      : projectExecutions.map((execution) => (
          <ProjectExecution
            key={execution.projectExecutionId}
            item={item}
            backend={backend}
            projectExecution={execution}
          />
        ))
      }
    </div>
  )
}
