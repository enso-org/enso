/** @file Rendering for an arbitrary {@link UsageSettingsSection}. */
import { BasicInput } from '#/components/Inputs/Input'
import { Scroller } from '#/components/Scroller'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import { Text } from '#/components/Text'
import { VisualTooltip } from '#/components/VisualTooltip'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import { useQuery } from '@tanstack/react-query'
import type { Backend, ExecutionUsageSummary } from 'enso-common/src/services/Backend'
import { MINUTE_MS } from 'enso-common/src/utilities/data/dateTime'
import * as React from 'react'
import { formatUptime } from './executionUsage'

/** Format current month. */
function getCurrentMonthValue() {
  const now = new Date()
  return `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, '0')}`
}

/** Get display name for a user. */
function displayUser(summary: ExecutionUsageSummary, unknownUserPlaceholder: string) {
  return (
    <VisualTooltip tooltip={summary.user.email ?? unknownUserPlaceholder}>
      {summary.user.name ?? summary.user.email ?? unknownUserPlaceholder}
    </VisualTooltip>
  )
}

/** Get display name for a project. */
function displayProject(summary: ExecutionUsageSummary, unknownProjectPlaceholder: string) {
  const projectId = String(summary.project.projectId)
  return summary.project.name ?? (projectId || unknownProjectPlaceholder)
}

/** Unique key for the summary table rows. */
function rowKey(summary: ExecutionUsageSummary) {
  return `${summary.project.projectId}-${summary.user.email ?? summary.user.name ?? 'unknown'}`
}

/** Props for a {@link FinancesHeaderCell}. */
export interface FinancesHeaderCellProps extends Readonly<React.PropsWithChildren> {
  readonly className?: string
}

/** A styled table cell for an {@link UsageSettingsSection}. */
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
export interface FinancesTableCellProps extends Readonly<React.PropsWithChildren> {
  readonly className?: string
}

/** A styled table cell for an {@link UsageSettingsSection}. */
function FinancesTableCell(props: FinancesTableCellProps) {
  const { children, className } = props

  return (
    <td
      className={twMerge(
        'border-x-2 border-transparent bg-clip-padding px-name-column-x first:rounded-l-full last:rounded-r-full last:border-r-0',
        className,
      )}
    >
      {children}
    </td>
  )
}

/** Props for a {@link UsageSettingsSection}. */
export interface FinancesSettingsSectionProps {
  readonly backend: Backend
}

/** Rendering for an arbitrary {@link SettingsEntryData}. */
export default function UsageSettingsSection(props: FinancesSettingsSectionProps) {
  const { backend } = props
  const { getText } = useText()
  const [month, setMonth] = React.useState(getCurrentMonthValue)

  const listExecutionsSummaryArgs = [{ month }] satisfies Parameters<
    typeof backend.listExecutionsSummary
  >
  const { data, isLoading } = useQuery({
    ...backendQueryOptions(backend, 'listExecutionsSummary', listExecutionsSummaryArgs),
    queryFn: () => backend.listExecutionsSummary(...listExecutionsSummaryArgs),
    staleTime: MINUTE_MS,
    meta: { persist: false },
  })

  const unknownUserPlaceholder = getText('executionSummaryUnknownUser')
  const unknownProjectPlaceholder = getText('executionSummaryUnknownProject')

  return (
    <>
      <div className="mb-3 flex flex-wrap items-center gap-2">
        <Text className="whitespace-nowrap">{getText('executionSummaryMonthLabel')}</Text>
        <div className="w-40">
          <BasicInput
            aria-label={getText('executionSummaryMonthLabel')}
            type="month"
            value={month}
            onChange={(event) => {
              const nextMonth = event.currentTarget.value
              if (nextMonth !== '') {
                setMonth(nextMonth)
              }
            }}
          />
        </div>
      </div>
      <Scroller
        scrollbar
        orientation="vertical"
        className="min-h-0 flex-1"
        shadowStartClassName="top-8"
      >
        <table className="table-fixed self-start rounded-rows">
          <thead>
            <tr className="sticky top-0 z-1 h-9 bg-dashboard">
              <FinancesHeaderCell className="w-60">
                <Text weight="bold">{getText('executionSummaryUserColumn')}</Text>
              </FinancesHeaderCell>
              <FinancesHeaderCell className="w-60">
                <Text weight="bold">{getText('executionSummaryProjectColumn')}</Text>
              </FinancesHeaderCell>
              <FinancesHeaderCell className="w-36">
                <Text weight="bold">{getText('executionSummaryCountOfExecutionsColumn')}</Text>
              </FinancesHeaderCell>
              <FinancesHeaderCell className="w-36">
                <Text weight="bold">{getText('executionSummaryTotalUptimeColumn')}</Text>
              </FinancesHeaderCell>
              <FinancesHeaderCell className="w-36">
                <Text weight="bold">{getText('executionSummaryAverageUptimeColumn')}</Text>
              </FinancesHeaderCell>
            </tr>
          </thead>
          <tbody className="select-text">
            {isLoading && (
              <tr className="h-12">
                <td colSpan={5} className="rounded-full bg-transparent px-name-column-x py-3">
                  <div className="flex items-center justify-center gap-2">
                    <StatelessSpinner size={24} phase="loading-medium" />
                    <Text>{getText('executionSummaryLoading')}</Text>
                  </div>
                </td>
              </tr>
            )}
            {!isLoading && (!data || data.length === 0) && (
              <tr className="h-12">
                <td
                  colSpan={5}
                  className="rounded-full bg-transparent px-name-column-x py-3 text-center"
                >
                  <Text>{getText('executionSummaryEmpty')}</Text>
                </td>
              </tr>
            )}
            {data?.map((summary) => {
              const userDisplay = displayUser(summary, unknownUserPlaceholder)
              const projectDisplay = displayProject(summary, unknownProjectPlaceholder)

              return (
                <tr key={rowKey(summary)} className="h-9 rounded-rows-child">
                  <FinancesTableCell>
                    <div className="max-w-60 truncate">{userDisplay}</div>
                  </FinancesTableCell>
                  <FinancesTableCell>
                    <div className="max-w-60 truncate">{projectDisplay}</div>
                  </FinancesTableCell>
                  <FinancesTableCell>{summary.totalSessions.toLocaleString()}</FinancesTableCell>
                  <FinancesTableCell>{formatUptime(summary.totalUptimeSeconds)}</FinancesTableCell>
                  <FinancesTableCell>
                    {formatUptime(summary.averageUptimeSeconds)}
                  </FinancesTableCell>
                </tr>
              )
            })}
          </tbody>
        </table>
      </Scroller>
    </>
  )
}
