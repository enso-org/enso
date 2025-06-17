/** @file A modal for showing logs for a project. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Loader } from '#/components/Loader'
import { Scroller } from '#/components/Scroller'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import type Backend from '#/services/Backend'
import type { ProjectSessionId } from '#/services/Backend'
import { useText } from '$/providers/react'
import { useInfiniteQuery } from '@tanstack/react-query'

/** Props for a {@link ProjectLogsModal}. */
export interface ProjectLogsModalProps {
  readonly backend: Backend
  readonly projectSessionId: ProjectSessionId
  readonly projectTitle: string
}

/** A modal for showing logs for a project. */
export default function ProjectLogsModal(props: ProjectLogsModalProps) {
  const { getText } = useText()

  return (
    <Dialog title={getText('logs')} type="fullscreen">
      <ProjectLogsModalInternal {...props} />
    </Dialog>
  )
}

/** A modal for showing logs for a project. */
function ProjectLogsModalInternal(props: ProjectLogsModalProps) {
  const { backend, projectSessionId, projectTitle } = props
  const { getText } = useText()

  const logsPages = useInfiniteQuery({
    queryKey: ['projectLogs', { projectSessionId, projectTitle }],
    queryFn: ({ pageParam }) =>
      backend.getProjectSessionLogs(projectSessionId, { scrollId: pageParam }, projectTitle),
    initialPageParam: ((): string | null => null)(),
    getNextPageParam: (page) => (page.hits.length === 0 ? null : page.scrollId),
  })
  const logs = logsPages.data?.pages.flatMap((page) => page.hits).join('\n')
  const isLoading = logsPages.isLoading
  const isFetching = logsPages.isFetching

  return (
    <div className="flex h-full flex-col gap-2">
      <Button.Group className="grow-0">
        <Button
          variant="icon"
          icon="refresh"
          aria-label={getText('reload')}
          onPress={() => logsPages.refetch()}
        />
      </Button.Group>
      {isLoading ?
        <Loader />
      : <Scroller
          scrollbar
          orientation="vertical"
          className="relative min-h-0 flex-1 after:pointer-events-none after:absolute after:inset-0 after:rounded-default after:border after:border-primary/20"
          onScroll={(event) => {
            if (isFetching) {
              return
            }
            const element = event.currentTarget
            if (element.scrollTop + element.clientHeight >= element.scrollHeight) {
              void logsPages.fetchNextPage()
            }
          }}
        >
          <pre className="m-4 whitespace-pre-wrap break-words">
            <code>{logs}</code>
          </pre>
          {isFetching && (
            <div className="my-2 flex h-8 w-full flex-col items-center">
              <StatelessSpinner size={32} phase="loading-medium" />
            </div>
          )}
        </Scroller>
      }
    </div>
  )
}
