/** @file Settings tab for viewing and editing account information. */
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { Icon } from '#/components/Icon'
import { IconDisplay } from '#/components/IconDisplay'
import { ComboBox } from '#/components/Inputs/ComboBox'
import { DatePicker } from '#/components/Inputs/DatePicker'
import { Scroller } from '#/components/Scroller'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import { Text } from '#/components/Text'
import { UserWithPopover } from '#/components/UserWithPopover'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { iconIdFor, nextSortDirection, type SortInfo } from '#/utilities/sorting'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { getLocalTimeZone, today, ZonedDateTime } from '@internationalized/date'
import { useInfiniteQuery, useQuery } from '@tanstack/react-query'
import type { AuditLogEvent, Backend, EmailAddress } from 'enso-common/src/services/Backend'
import { MINUTE_MS, toReadableIsoString, toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import * as React from 'react'
import * as z from 'zod'
import {
  DEFAULT_EVENT_ICON,
  EVENT_TYPE_ICON,
  EVENT_TYPE_NAME_ID,
  LAMBDA_KINDS,
  normalizeLambdaKind,
  SELECTABLE_LAMBDA_KINDS,
} from './lambdaKinds'

/** Create the schema for this form. */
function createActivityLogSchema() {
  return z.object({
    userEmail: z.custom<EmailAddress>((s) => typeof s === 'string').optional(),
    type: z.string().optional(),
    startDate: z.instanceof(ZonedDateTime).optional(),
    endDate: z.instanceof(ZonedDateTime).optional(),
  })
}

/** Sortable columns in an activity log table. */
enum ActivityLogSortableColumn {
  type = 'type',
  user = 'user',
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
  const [sortInfo, setSortInfo] = React.useState<SortInfo<ActivityLogSortableColumn> | null>(null)
  const { data: usersRaw = [] } = useQuery(backendQueryOptions(backend, 'listUsers', []))
  const users = [...usersRaw].sort((a, b) => a.name.localeCompare(b.name))
  const allEmails = users.map((user) => user.email)
  const usersByEmail = new Map(users.map((user) => [user.email, user]))
  const isDescending = sortInfo?.direction === 'descending'

  const scrollerRef = React.useRef<HTMLDivElement | null>(null)
  const lambdaKindsByName = new Map(
    SELECTABLE_LAMBDA_KINDS.map((kind) => [getText(EVENT_TYPE_NAME_ID[kind]), kind]),
  )
  const endpointNames = [...lambdaKindsByName.keys()].sort((a, b) => a.localeCompare(b))

  const pageSize = useFeatureFlag('getLogEventsPageSize')
  const form = Form.useForm({
    schema: createActivityLogSchema(),
  })
  const typeRaw = form.watch('type')
  const lambdaKind = typeRaw != null ? lambdaKindsByName.get(typeRaw) : null
  const userEmail = form.watch('userEmail')
  const startDate = form.watch('startDate')
  const endDate = form.watch('endDate')
  const maxDate = today(getLocalTimeZone())

  const getLogEventsArgs = [
    {
      userEmail,
      lambdaKind,
      startDate: startDate && toRfc3339(startDate.toDate()),
      endDate: endDate && toRfc3339(endDate.toDate()),
      pageSize,
    },
  ] satisfies Parameters<typeof backend.getLogEvents>
  const getLogEventsOptions = backendQueryOptions(backend, 'getLogEvents', getLogEventsArgs, {
    queryKey: [{ infinite: true }],
  })
  const logsPages = useInfiniteQuery({
    queryKey: getLogEventsOptions.queryKey,
    queryFn: ({ pageParam }) => backend.getLogEvents({ from: pageParam, ...getLogEventsArgs[0] }),
    initialPageParam: 0,
    getPreviousPageParam: (currentPage, allPages) => (allPages.indexOf(currentPage) - 1) * pageSize,
    getNextPageParam: (currentPage, allPages) => (allPages.indexOf(currentPage) + 1) * pageSize,
    staleTime: MINUTE_MS,
    meta: { persist: false },
  })
  const logs = logsPages.data?.pages.flat()
  const fetchNextLogsPage = logsPages.fetchNextPage
  const isFetching = logsPages.isLoading || logsPages.isFetchingNextPage

  React.useEffect(() => {
    const scrollerEl = scrollerRef.current
    if (!scrollerEl) return
    if (scrollerEl.scrollTop + scrollerEl.clientHeight >= scrollerEl.scrollHeight) {
      void fetchNextLogsPage()
    }
  }, [fetchNextLogsPage, logsPages.data?.pages])

  const sortedLogs = (() => {
    const filteredLogs = logs?.filter((log) => {
      if (log.lambdaKind == null) {
        return false
      }
      const kind = normalizeLambdaKind(log.lambdaKind)
      return lambdaKind == null || !kind.valid || kind.kind === lambdaKind
    })

    if (sortInfo == null || filteredLogs == null) {
      return filteredLogs
    } else {
      let compare: (a: AuditLogEvent, b: AuditLogEvent) => number
      const multiplier = sortInfo.direction === 'ascending' ? 1 : -1
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
        case ActivityLogSortableColumn.user: {
          compare = (a, b) => {
            const aName = usersByEmail.get(a.userEmail)?.name ?? a.userEmail
            const bName = usersByEmail.get(b.userEmail)?.name ?? b.userEmail
            return multiplier * aName.localeCompare(bName)
          }
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

  return (
    <>
      <Form form={form} className="flex flex-row flex-wrap gap-3">
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('startDate')}</Text>
          <DatePicker form={form} name="startDate" maxValue={maxDate} className="w-36" />
        </div>
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('endDate')}</Text>
          <DatePicker form={form} name="endDate" maxValue={maxDate} className="w-36" />
        </div>
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('type')}</Text>
          <ComboBox
            form={form}
            name="type"
            aria-label={getText('type')}
            items={endpointNames}
            toTextValue={(otherType) => otherType ?? ''}
            className="w-60"
          >
            {(otherType) => {
              if (otherType == null) {
                return null
              }
              const otherLambdaKind = lambdaKindsByName.get(otherType)
              if (otherLambdaKind == null) {
                return otherType
              }
              return (
                <div className="flex w-full">
                  <IconDisplay align="left" icon={EVENT_TYPE_ICON[otherLambdaKind]}>
                    {otherType}
                  </IconDisplay>
                </div>
              )
            }}
          </ComboBox>
        </div>
        <div className="flex items-center gap-2">
          <Text className="whitespace-nowrap">{getText('user')}</Text>
          <ComboBox
            form={form}
            name="userEmail"
            aria-label={getText('user')}
            items={allEmails}
            toTextValue={(email) => {
              if (email == null) {
                return ''
              }
              const name = usersByEmail.get(email)?.name
              if (name == null) {
                return email
              }
              return `${name} (${email})`
            }}
            className="w-96"
          >
            {(email) => {
              if (email == null) {
                return null
              }
              const user = usersByEmail.get(email)
              if (!user) {
                return null
              }

              return (
                <UserWithPopover
                  user={{ ...user, name: `${user.name} (${user.email})` }}
                  className="pointer-events-none"
                />
              )
            }}
          </ComboBox>
        </div>
      </Form>
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
                      : 'ascending'
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
                    sortInfo?.field !== ActivityLogSortableColumn.user ? getText('sortByEmail')
                    : isDescending ?
                      getText('stopSortingByEmail')
                    : getText('sortByEmailDescending')
                  }
                  addonEnd={
                    <Icon
                      icon={iconIdFor(
                        sortInfo?.direction,
                        sortInfo?.field === ActivityLogSortableColumn.user,
                      )}
                      className={twMerge(
                        'ml-1 transition-all duration-arrow',
                        sortInfo?.field !== ActivityLogSortableColumn.user &&
                          'opacity-0 group-hover:opacity-50',
                      )}
                    />
                  }
                  className="group flex h-9 w-full items-center justify-start gap-2 border-0 px-name-column-x"
                  onPress={() => {
                    const nextDirection =
                      sortInfo?.field === ActivityLogSortableColumn.user ?
                        nextSortDirection(sortInfo.direction)
                      : 'ascending'
                    if (nextDirection == null) {
                      setSortInfo(null)
                    } else {
                      setSortInfo({
                        field: ActivityLogSortableColumn.user,
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
                      : 'ascending'
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
            {sortedLogs?.map((log, i) => {
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
