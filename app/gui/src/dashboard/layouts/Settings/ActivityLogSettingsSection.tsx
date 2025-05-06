/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import { fromDate, getLocalTimeZone, today, ZonedDateTime } from '@internationalized/date'
import * as z from 'zod'

import { Button, DatePicker, Dropdown, Form, Text } from '#/components/AriaComponents'
import { Icon } from '#/components/Icon'
import { Scroller } from '#/components/Scroller'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import { UserWithPopover } from '#/components/UserWithPopover'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { type AuditLogEvent } from '#/services/Backend'
import { iconIdFor, nextSortDirection, SortDirection, type SortInfo } from '#/utilities/sorting'
import { twMerge } from '#/utilities/tailwindMerge'
import { useInfiniteQuery, useQuery } from '@tanstack/react-query'
import { toReadableIsoString, toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import {
  DEFAULT_EVENT_ICON,
  EVENT_TYPE_ICON,
  EVENT_TYPE_NAME_ID,
  LAMBDA_KINDS,
  normalizeLambdaKind,
  SELECTABLE_LAMBDA_KINDS,
  type LambdaKind,
} from './lambdaKinds'

const GET_LOG_EVENTS_DEFAULT_PAGE_SIZE = 100

/** Create the schema for this form. */
function createActivityLogSchema() {
  return z.object({
    startDate: z.instanceof(ZonedDateTime).optional(),
    endDate: z.instanceof(ZonedDateTime).optional(),
    pageSize: z.number().int(),
  })
}

/** Sortable columns in an activity log table. */
enum ActivityLogSortableColumn {
  type = 'type',
  email = 'email',
  timestamp = 'timestamp',
}

/** Props for a {@link ActivityLogSettingsSection}. */
export interface ActivityLogSettingsSectionProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization members. */
export default function ActivityLogSettingsSection(props: ActivityLogSettingsSectionProps) {
  const { backend } = props
  const { getText } = useText()
  const [types, setTypes] = React.useState<readonly LambdaKind[]>([])
  const [typeIndices, setTypeIndices] = React.useState<readonly number[]>([])
  const [emails, setEmails] = React.useState<readonly string[]>([])
  const [emailIndices, setEmailIndices] = React.useState<readonly number[]>([])
  const [sortInfo, setSortInfo] = React.useState<SortInfo<ActivityLogSortableColumn> | null>(null)
  const { data: users = [] } = useQuery(backendQueryOptions(backend, 'listUsers', []))
  const allEmails = users.map((user) => user.email)
  const usersByEmail = new Map(users.map((user) => [user.email, user]))

  const form = Form.useForm({
    schema: createActivityLogSchema(),
    defaultValues: { pageSize: GET_LOG_EVENTS_DEFAULT_PAGE_SIZE },
  })
  const startDate = form.watch('startDate')
  const endDate = form.watch('endDate')
  const pageSize = form.watch('pageSize')
  const maxDate = today(getLocalTimeZone())

  const getLogEventsArgs = [
    {
      startDate: startDate && toRfc3339(startDate.toDate()),
      endDate: endDate && toRfc3339(endDate.toDate()),
      pageSize,
    },
  ] satisfies Parameters<typeof backend.getLogEvents>
  const getLogEventsOptions = backendQueryOptions(backend, 'getLogEvents', getLogEventsArgs)
  const logsPages = useInfiniteQuery({
    queryKey: getLogEventsOptions.queryKey,
    queryFn: ({ pageParam }) => backend.getLogEvents({ from: pageParam, ...getLogEventsArgs[0] }),
    initialPageParam: 0,
    getPreviousPageParam: (currentPage, allPages) => (allPages.indexOf(currentPage) - 1) * pageSize,
    getNextPageParam: (currentPage, allPages) => (allPages.indexOf(currentPage) + 1) * pageSize,
  })
  const logs = logsPages.data?.pages.flat()

  const filteredLogs = (() => {
    const typesSet = new Set(types.length > 0 ? types : LAMBDA_KINDS)
    const emailsSet = new Set(emails.length > 0 ? emails : allEmails)
    return logs?.filter((log) => {
      const date = log.timestamp == null ? null : fromDate(new Date(log.timestamp), 'UTC')
      if (log.lambdaKind == null) {
        return false
      }
      const kind = normalizeLambdaKind(log.lambdaKind)
      if (!kind.valid) {
        return false
      }
      if (!typesSet.has(kind.kind)) {
        return false
      }
      if (!emailsSet.has(log.userEmail)) {
        return false
      }
      if (date == null) {
        return true
      }
      return (startDate == null || date >= startDate) && (endDate == null || date <= endDate)
    })
  })()

  const sortedLogs = (() => {
    if (sortInfo == null || filteredLogs == null) {
      return filteredLogs
    } else {
      let compare: (a: AuditLogEvent, b: AuditLogEvent) => number
      const multiplier = sortInfo.direction === SortDirection.ascending ? 1 : -1
      switch (sortInfo.field) {
        case ActivityLogSortableColumn.type: {
          compare = (a, b) => {
            if (a.lambdaKind == null) {
              if (b.lambdaKind == null) {
                return 0
              }
              return multiplier
            }
            if (b.lambdaKind == null) {
              return -multiplier
            }
            const aKind = normalizeLambdaKind(a.lambdaKind)
            const aIndex = aKind.valid ? LAMBDA_KINDS.indexOf(aKind.kind) : LAMBDA_KINDS.length
            const bKind = normalizeLambdaKind(b.lambdaKind)
            const bIndex = bKind.valid ? LAMBDA_KINDS.indexOf(bKind.kind) : LAMBDA_KINDS.length
            return multiplier * (aIndex - bIndex)
          }
          break
        }
        case ActivityLogSortableColumn.email: {
          compare = (a, b) =>
            multiplier *
            (a.userEmail < b.userEmail ? -1
            : a.userEmail > b.userEmail ? 1
            : 0)
          break
        }
        case ActivityLogSortableColumn.timestamp: {
          compare = (a, b) => {
            const aTime = a.timestamp == null ? 0 : Number(new Date(a.timestamp))
            const bTime = b.timestamp == null ? 0 : Number(new Date(b.timestamp))
            return multiplier * aTime - bTime
          }
          break
        }
      }
      return [...filteredLogs].sort(compare)
    }
  })()
  const isDescending = sortInfo?.direction === SortDirection.descending
  const isLoading = sortedLogs == null

  return (
    <>
      <Form form={form} className="flex flex-row flex-wrap gap-3">
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('startDate')}</Text>
          <DatePicker
            form={form}
            name="startDate"
            size="small"
            maxValue={maxDate}
            className="w-36"
          />
        </div>
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('endDate')}</Text>
          <DatePicker form={form} name="endDate" size="small" maxValue={maxDate} className="w-36" />
        </div>
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('types')}</Text>
          <Dropdown
            aria-label={getText('types')}
            multiple
            items={SELECTABLE_LAMBDA_KINDS}
            selectedIndices={typeIndices}
            renderMultiple={({ items }) =>
              items.length === 0 || items.length === SELECTABLE_LAMBDA_KINDS.length ?
                'All'
              : (items[0] != null ? getText(EVENT_TYPE_NAME_ID[items[0]]) : '') +
                (items.length <= 1 ? '' : ` (+${items.length - 1})`)
            }
            onChange={(items, indices) => {
              setTypes(items)
              setTypeIndices(indices)
            }}
          >
            {({ item }) => getText(EVENT_TYPE_NAME_ID[item])}
          </Dropdown>
        </div>
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('users')}</Text>
          <Dropdown
            aria-label={getText('users')}
            multiple
            items={allEmails}
            selectedIndices={emailIndices}
            renderMultiple={({ items }) =>
              items.length === 0 || items.length === allEmails.length ?
                'All'
              : (items[0] ?? '') + (items.length <= 1 ? '' : `(+${items.length - 1})`)
            }
            onChange={(items, indices) => {
              setEmails(items)
              setEmailIndices(indices)
            }}
          >
            {({ item }) => item}
          </Dropdown>
        </div>
      </Form>
      <Scroller
        scrollbar
        orientation="vertical"
        className="min-h-0 flex-1 overflow-auto"
        shadowStartClassName="mt-8"
        onScroll={(event) => {
          const element = event.currentTarget
          if (element.scrollTop + element.scrollHeight >= element.clientHeight) {
            void logsPages.fetchNextPage()
          }
        }}
      >
        <table className="table-fixed self-start rounded-rows">
          <thead>
            <tr className="sticky top-0 z-1 h-9 bg-dashboard">
              <ActivityLogHeaderCell className="w-8" />
              <ActivityLogHeaderCell className="w-60">
                <Button
                  size="custom"
                  variant="custom"
                  aria-label={
                    sortInfo?.field !== ActivityLogSortableColumn.type ? getText('sortByName')
                    : isDescending ?
                      getText('stopSortingByName')
                    : getText('sortByNameDescending')
                  }
                  addonEnd={
                    <Icon
                      icon={iconIdFor(
                        sortInfo?.direction,
                        sortInfo?.field === ActivityLogSortableColumn.type,
                      )}
                      className={twMerge(
                        'ml-1 transition-all duration-arrow',
                        sortInfo?.field !== ActivityLogSortableColumn.type &&
                          'opacity-0 group-hover:opacity-50',
                      )}
                    />
                  }
                  className="group flex h-9 w-full items-center justify-start gap-2 border-0 px-name-column-x"
                  onPress={() => {
                    const nextDirection =
                      sortInfo?.field === ActivityLogSortableColumn.type ?
                        nextSortDirection(sortInfo.direction)
                      : SortDirection.ascending
                    if (nextDirection == null) {
                      setSortInfo(null)
                    } else {
                      setSortInfo({
                        field: ActivityLogSortableColumn.type,
                        direction: nextDirection,
                      })
                    }
                  }}
                >
                  <Text weight="bold">{getText('type')}</Text>
                </Button>
              </ActivityLogHeaderCell>
              <ActivityLogHeaderCell className="w-48">
                <Button
                  size="custom"
                  variant="custom"
                  aria-label={
                    sortInfo?.field !== ActivityLogSortableColumn.email ? getText('sortByEmail')
                    : isDescending ?
                      getText('stopSortingByEmail')
                    : getText('sortByEmailDescending')
                  }
                  addonEnd={
                    <Icon
                      icon={iconIdFor(
                        sortInfo?.direction,
                        sortInfo?.field === ActivityLogSortableColumn.email,
                      )}
                      className={twMerge(
                        'ml-1 transition-all duration-arrow',
                        sortInfo?.field !== ActivityLogSortableColumn.email &&
                          'opacity-0 group-hover:opacity-50',
                      )}
                    />
                  }
                  className="group flex h-9 w-full items-center justify-start gap-2 border-0 px-name-column-x"
                  onPress={() => {
                    const nextDirection =
                      sortInfo?.field === ActivityLogSortableColumn.email ?
                        nextSortDirection(sortInfo.direction)
                      : SortDirection.ascending
                    if (nextDirection == null) {
                      setSortInfo(null)
                    } else {
                      setSortInfo({
                        field: ActivityLogSortableColumn.email,
                        direction: nextDirection,
                      })
                    }
                  }}
                >
                  <Text weight="bold">{getText('user')}</Text>
                </Button>
              </ActivityLogHeaderCell>
              <ActivityLogHeaderCell className="w-40">
                <Button
                  size="custom"
                  variant="custom"
                  aria-label={
                    sortInfo?.field !== ActivityLogSortableColumn.timestamp ?
                      getText('sortByTimestamp')
                    : isDescending ?
                      getText('stopSortingByTimestamp')
                    : getText('sortByTimestampDescending')
                  }
                  addonEnd={
                    <Icon
                      icon={iconIdFor(
                        sortInfo?.direction,
                        sortInfo?.field === ActivityLogSortableColumn.timestamp,
                      )}
                      className={twMerge(
                        'ml-1 transition-all duration-arrow',
                        sortInfo?.field !== ActivityLogSortableColumn.timestamp &&
                          'opacity-0 group-hover:opacity-50',
                      )}
                    />
                  }
                  className="group flex h-9 w-full items-center justify-start gap-2 border-0 px-name-column-x"
                  onPress={() => {
                    const nextDirection =
                      sortInfo?.field === ActivityLogSortableColumn.timestamp ?
                        nextSortDirection(sortInfo.direction)
                      : SortDirection.ascending
                    if (nextDirection == null) {
                      setSortInfo(null)
                    } else {
                      setSortInfo({
                        field: ActivityLogSortableColumn.timestamp,
                        direction: nextDirection,
                      })
                    }
                  }}
                >
                  <Text weight="bold">{getText('timestamp')}</Text>
                </Button>
              </ActivityLogHeaderCell>
            </tr>
          </thead>
          <tbody className="select-text">
            {isLoading ?
              <tr className="h-9">
                <td colSpan={4} className="rounded-full bg-transparent">
                  <div className="flex justify-center">
                    <StatelessSpinner size={32} state="loading-medium" />
                  </div>
                </td>
              </tr>
            : sortedLogs.map((log, i) => {
                const kind = log.lambdaKind == null ? null : normalizeLambdaKind(log.lambdaKind)
                const user = usersByEmail.get(log.userEmail)
                return (
                  <tr key={i} className="h-9">
                    <ActivityLogTableCell>
                      <div className="flex items-center">
                        <Icon
                          icon={
                            kind?.valid === true ? EVENT_TYPE_ICON[kind.kind] : DEFAULT_EVENT_ICON
                          }
                        />
                      </div>
                    </ActivityLogTableCell>
                    <ActivityLogTableCell>
                      {kind?.valid === true ?
                        getText(EVENT_TYPE_NAME_ID[kind.kind])
                      : (kind?.invalidKind ?? '(unknown)')}
                    </ActivityLogTableCell>
                    <ActivityLogTableCell>
                      {user ?
                        <div className="flex w-48">
                          <UserWithPopover user={user} />
                        </div>
                      : log.userEmail}
                    </ActivityLogTableCell>
                    <ActivityLogTableCell>
                      {log.timestamp ? toReadableIsoString(new Date(log.timestamp)) : ''}
                    </ActivityLogTableCell>
                  </tr>
                )
              })
            }
          </tbody>
        </table>
      </Scroller>
    </>
  )
}

/** Props for a {@link ActivityLogHeaderCell}. */
export interface ActivityLogHeaderCellProps extends Readonly<React.PropsWithChildren> {
  readonly className?: string
}

/** A styled table cell for an {@link ActivityLogSettingsSection}. */
function ActivityLogHeaderCell(props: ActivityLogHeaderCellProps) {
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

/** Props for a {@link ActivityLogTableCell}. */
type ActivityLogTableCellProps = Readonly<React.PropsWithChildren>

/** A styled table cell for an {@link ActivityLogSettingsSection}. */
function ActivityLogTableCell(props: ActivityLogTableCellProps) {
  const { children } = props

  return (
    <td className="border-x-2 border-transparent bg-clip-padding px-name-column-x first:rounded-l-full last:rounded-r-full last:border-r-0">
      {children}
    </td>
  )
}
