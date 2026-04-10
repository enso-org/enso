/** @file Rendering for an arbitrary {@link FinancesSettingsSection}. */
import type { Backend, ProjectExecutionId, ProjectSession } from 'enso-common/src/services/Backend'
import { useText } from '$/providers/react'
import { useInfiniteQuery } from '@tanstack/react-query'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { MINUTE_MS, toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import { twMerge } from '#/utilities/tailwindMerge'
import { Text } from '#/components/Text'
import { Form } from '#/components/Form'
import { Scroller } from '#/components/Scroller'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import * as React from 'react'

/** Props for a {@link FinancesHeaderCell}. */
export interface FinancesHeaderCellProps extends Readonly<React.PropsWithChildren> {
  readonly className?: string
}

/** A styled table cell for an {@link FinancesSettingsSection}. */
function FinancesHeaderCell(props: FinancesHeaderCellProps) {
  const { children, className } = props

  return (
    <td
      className={twMerge(
        'border-x-2 border-transparent bg-clip-padding text-left text-sm font-semibold last:border-r-0',
        className,
      )}
    >
      {children}
    </td>
  )
}

/** Props for a {@link FinancesTableCell}. */
type FinancesTableCellProps = Readonly<React.PropsWithChildren>

/** A styled table cell for an {@link FinancesSettingsSection}. */
function FinancesTableCell(props: FinancesTableCellProps) {
  const { children } = props

  return (
    <td className="border-x-2 border-transparent bg-clip-padding px-name-column-x first:rounded-l-full last:rounded-r-full last:border-r-0">
      {children}
    </td>
  )
}

/** Props for a {@link FinancesSettingsSection}. */
export interface FinancesSettingsSectionProps {
    readonly backend: Backend
}

/** Rendering for an arbitrary {@link SettingsEntryData}. */
export default function FinancesSettingsSection(props: FinancesSettingsSectionProps) {
  const { backend } = props
  const { getText } = useText()
  const scrollerRef = React.useRef<HTMLDivElement | null>(null)

  const listExecutionsArgs = [{},] satisfies Parameters<typeof backend.listExecutions>
  const getExecutionsOptions = backendQueryOptions(backend, 'listExecutions', listExecutionsArgs, {
      queryKey: [{ infinite: true }],
    })

  const executionsPages = useInfiniteQuery({
    queryKey: getExecutionsOptions.queryKey,
    queryFn: ({ pageParam }) => backend.listExecutions({ lastExecutionId: pageParam, ...listExecutionsArgs[0] }),
    initialPageParam: ((): ProjectExecutionId | null => null)(),
    getPreviousPageParam: (currentPage, allPages) => (allPages.at(allPages.indexOf(currentPage) - 1))?.at(-1)?.executionId,
    getNextPageParam: (currentPage, _allPages) => currentPage.at(-1)?.executionId,
    staleTime: MINUTE_MS,
    meta: { persist: false },
  })
  const executions = executionsPages.data?.pages.flat()
  const fetchNextExecutionsPage = executionsPages.fetchNextPage
  const isFetching = executionsPages.isLoading || executionsPages.isFetchingNextPage
  React.useEffect(() => {
    const scrollerEl = scrollerRef.current
    if (!scrollerEl) return
    if (scrollerEl.scrollTop + scrollerEl.clientHeight >= scrollerEl.scrollHeight) {
    void fetchNextExecutionsPage()
    }
  }, [fetchNextExecutionsPage, executionsPages.data?.pages])

  const sortedExecutions = (() => executions)()

  return <>
  <Scroller
          ref={scrollerRef}
          scrollbar
          orientation="vertical"
          className="min-h-0 flex-1"
          shadowStartClassName="top-8"
          onScroll={(event) => {
            if (isFetching) return
            const element = event.currentTarget
            if (element.scrollTop + element.clientHeight >= element.scrollHeight) {
              void executionsPages.fetchNextPage()
            }
          }}
        >
    <table className="table-fixed self-start rounded-rows">
        <thead>
            <tr className="sticky top-0 z-1 h-9 bg-dashboard">
                <FinancesHeaderCell className="w-8" />
                <FinancesHeaderCell className="w-60">
                    <Text weight="bold">{'Project'}</Text>
                </FinancesHeaderCell>
                <FinancesHeaderCell className="w-60">
                    <Text weight="bold">{'Uptime'}</Text>
                </FinancesHeaderCell>
                <FinancesHeaderCell className="w-60">
                    <Text weight="bold">{'Started'}</Text>
                </FinancesHeaderCell>
            </tr>
        </thead>
        <tbody className="select-text">
            {sortedExecutions?.map((execution, i) => {
                return (
                    <tr key={i} className="h-9">
                        <FinancesTableCell />
                        <FinancesTableCell>{execution.project?.title}</FinancesTableCell>
                        <FinancesTableCell>{execution.projectSessions?.reduce((s: number, projectSession: ProjectSession) => {
                                return s + (projectSession.uptime ? projectSession.uptime : 0) 
                            }, 0)}
                        </FinancesTableCell>
                        <FinancesTableCell>{toReadableIsoString(new Date(execution.startDate))}</FinancesTableCell>
                    </tr>

                )
            })}
            {isFetching && (
                          <tr className="h-9">
                            <td colSpan={4} className="rounded-full bg-transparent">
                              <div className="flex justify-center">
                                <StatelessSpinner size={32} phase="loading-medium" />
                              </div>
                            </td>
                          </tr>
                        )}
        </tbody>
    </table>
    </Scroller>
  </>
}
