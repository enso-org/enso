/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import { fromDate, getLocalTimeZone, today, ZonedDateTime } from '@internationalized/date'
import * as z from 'zod'

import DataUploadIcon from '#/assets/data_upload.svg'
import KeyIcon from '#/assets/key.svg'
import Play2Icon from '#/assets/play2.svg'
import SortAscendingIcon from '#/assets/sort_ascending.svg'
import TrashIcon from '#/assets/trash.svg'
import { Button, DatePicker, Dropdown, Form, Text } from '#/components/AriaComponents'
import StatelessSpinner, { SpinnerState } from '#/components/StatelessSpinner'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'
import { useBackendQuery } from '#/hooks/backendHooks'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { EVENT_TYPES, EventType, type Event } from '#/services/Backend'
import { formatDateTime } from '#/utilities/dateTime'
import { nextSortDirection, SortDirection, type SortInfo } from '#/utilities/sorting'
import { twMerge } from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const EVENT_TYPE_ICON: Record<EventType, string> = {
  [EventType.GetSecret]: KeyIcon,
  [EventType.DeleteAssets]: TrashIcon,
  [EventType.ListSecrets]: KeyIcon,
  [EventType.OpenProject]: Play2Icon,
  [EventType.UploadFile]: DataUploadIcon,
}

const EVENT_TYPE_NAME: Record<EventType, string> = {
  [EventType.GetSecret]: 'Get Secret',
  [EventType.DeleteAssets]: 'Delete Assets',
  [EventType.ListSecrets]: 'List Secrets',
  [EventType.OpenProject]: 'Open Project',
  [EventType.UploadFile]: 'Upload File',
}

/** Create the schema for this form. */
function createActivityLogSchema() {
  return z.object({
    startDate: z.instanceof(ZonedDateTime).optional(),
    endDate: z.instanceof(ZonedDateTime).optional(),
  })
}

// =================================
// === ActivityLogSortableColumn ===
// =================================

/** Sortable columns in an activity log table. */
enum ActivityLogSortableColumn {
  type = 'type',
  email = 'email',
  timestamp = 'timestamp',
}

// ==================================
// === ActivityLogSettingsSection ===
// ==================================

/** Props for a {@link ActivityLogSettingsSection}. */
export interface ActivityLogSettingsSectionProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization members. */
export default function ActivityLogSettingsSection(props: ActivityLogSettingsSectionProps) {
  const { backend } = props
  const { getText } = useText()
  const [types, setTypes] = React.useState<readonly EventType[]>([])
  const [typeIndices, setTypeIndices] = React.useState<readonly number[]>(() => [])
  const [emails, setEmails] = React.useState<readonly string[]>([])
  const [emailIndices, setEmailIndices] = React.useState<readonly number[]>(() => [])
  const [sortInfo, setSortInfo] = React.useState<SortInfo<ActivityLogSortableColumn> | null>(null)
  const { data: users } = useBackendQuery(backend, 'listUsers', [])
  const allEmails = React.useMemo(() => (users ?? []).map((user) => user.email), [users])
  const logsQuery = useBackendQuery(backend, 'getLogEvents', [])
  const logs = logsQuery.data

  const form = Form.useForm({ schema: createActivityLogSchema() })
  const startDate = form.watch('startDate')
  const endDate = form.watch('endDate')
  const maxDate = today(getLocalTimeZone())

  const filteredLogs = React.useMemo(() => {
    const typesSet = new Set(types.length > 0 ? types : EVENT_TYPES)
    const emailsSet = new Set(emails.length > 0 ? emails : allEmails)
    return logs == null ? null : (
        logs.filter((log) => {
          const date = log.timestamp == null ? null : fromDate(new Date(log.timestamp), 'UTC')
          return (
            typesSet.has(log.metadata.type) &&
            emailsSet.has(log.userEmail) &&
            (date == null ||
              ((startDate == null || date >= startDate) && (endDate == null || date <= endDate)))
          )
        })
      )
  }, [logs, types, emails, startDate, endDate, allEmails])

  const sortedLogs = React.useMemo(() => {
    if (sortInfo == null || filteredLogs == null) {
      return filteredLogs
    } else {
      let compare: (a: Event, b: Event) => number
      const multiplier = sortInfo.direction === SortDirection.ascending ? 1 : -1
      switch (sortInfo.field) {
        case ActivityLogSortableColumn.type: {
          compare = (a, b) =>
            multiplier *
            (a.metadata.type < b.metadata.type ? -1
            : a.metadata.type > b.metadata.type ? 1
            : 0)
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
  }, [filteredLogs, sortInfo])
  const isDescending = sortInfo?.direction === SortDirection.descending
  const isLoading = sortedLogs == null

  return (
    <div className="flex flex-col gap-4">
      <FocusArea direction="horizontal">
        {(innerProps) => (
          <Form form={form} className="flex flex-row flex-wrap gap-3" {...innerProps}>
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
              <DatePicker
                form={form}
                name="endDate"
                size="small"
                maxValue={maxDate}
                className="w-36"
              />
            </div>
            <div className="flex items-center gap-2">
              <Text className="whitespace-nowrap">{getText('types')}</Text>
              <Dropdown
                aria-label={getText('types')}
                multiple
                items={EVENT_TYPES}
                selectedIndices={typeIndices}
                renderMultiple={({ items }) =>
                  items.length === 0 || items.length === EVENT_TYPES.length ?
                    'All'
                  : (items[0] != null ? EVENT_TYPE_NAME[items[0]] : '') +
                    (items.length <= 1 ? '' : ` (+${items.length - 1})`)
                }
                onChange={(items, indices) => {
                  setTypes(items)
                  setTypeIndices(indices)
                }}
              >
                {({ item }) => EVENT_TYPE_NAME[item]}
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
        )}
      </FocusArea>
      <table className="table-fixed self-start rounded-rows">
        <thead>
          <tr className="h-table-row">
            <ActivityLogHeaderCell className="w-8" />
            <ActivityLogHeaderCell className="w-32">
              <Button
                size="custom"
                variant="custom"
                aria-label={
                  sortInfo?.field !== ActivityLogSortableColumn.type ? getText('sortByName')
                  : isDescending ?
                    getText('stopSortingByName')
                  : getText('sortByNameDescending')
                }
                className="group flex h-table-row w-full items-center justify-start gap-icon-with-text border-0 px-name-column-x"
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
                <Text className="text-sm">{getText('type')}</Text>
                <img
                  alt={
                    sortInfo?.field === ActivityLogSortableColumn.type && isDescending ?
                      getText('sortDescending')
                    : getText('sortAscending')
                  }
                  src={SortAscendingIcon}
                  className={twMerge(
                    'transition-all duration-arrow',
                    sortInfo?.field !== ActivityLogSortableColumn.type &&
                      'opacity-0 group-hover:opacity-50',
                    sortInfo?.field === ActivityLogSortableColumn.type &&
                      isDescending &&
                      'rotate-180',
                  )}
                />
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
                className="group flex h-table-row w-full items-center justify-start gap-icon-with-text border-0 px-name-column-x"
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
                <Text className="text-sm">{getText('email')}</Text>
                <img
                  alt={
                    sortInfo?.field === ActivityLogSortableColumn.email && isDescending ?
                      getText('sortDescending')
                    : getText('sortAscending')
                  }
                  src={SortAscendingIcon}
                  className={twMerge(
                    'transition-all duration-arrow',
                    sortInfo?.field !== ActivityLogSortableColumn.email &&
                      'opacity-0 group-hover:opacity-50',
                    sortInfo?.field === ActivityLogSortableColumn.email &&
                      isDescending &&
                      'rotate-180',
                  )}
                />
              </Button>
            </ActivityLogHeaderCell>
            <ActivityLogHeaderCell className="w-36">
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
                className="group flex h-table-row w-full items-center justify-start gap-icon-with-text border-0 px-name-column-x"
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
                <Text className="text-sm">{getText('timestamp')}</Text>
                <img
                  alt={
                    sortInfo?.field === ActivityLogSortableColumn.timestamp && isDescending ?
                      getText('sortDescending')
                    : getText('sortAscending')
                  }
                  src={SortAscendingIcon}
                  className={twMerge(
                    'transition-all duration-arrow',
                    sortInfo?.field !== ActivityLogSortableColumn.timestamp &&
                      'opacity-0 group-hover:opacity-50',
                    sortInfo?.field === ActivityLogSortableColumn.timestamp &&
                      isDescending &&
                      'rotate-180',
                  )}
                />
              </Button>
            </ActivityLogHeaderCell>
          </tr>
        </thead>
        <tbody className="select-text">
          {isLoading ?
            <tr className="h-table-row">
              <td colSpan={4} className="rounded-full bg-transparent">
                <div className="flex justify-center">
                  <StatelessSpinner size={32} state={SpinnerState.loadingMedium} />
                </div>
              </td>
            </tr>
          : sortedLogs.map((log, i) => (
              <tr key={i} className="h-table-row">
                <ActivityLogTableCell>
                  <div className="flex items-center">
                    <SvgMask src={EVENT_TYPE_ICON[log.metadata.type]} />
                  </div>
                </ActivityLogTableCell>
                <ActivityLogTableCell>{EVENT_TYPE_NAME[log.metadata.type]}</ActivityLogTableCell>
                <ActivityLogTableCell>{log.userEmail}</ActivityLogTableCell>
                <ActivityLogTableCell>
                  {log.timestamp ? formatDateTime(new Date(log.timestamp)) : ''}
                </ActivityLogTableCell>
              </tr>
            ))
          }
        </tbody>
      </table>
    </div>
  )
}

// =============================
// === ActivityLogHeaderCell ===
// =============================

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

// ============================
// === ActivityLogTableCell ===
// ============================

/** Props for a {@link ActivityLogTableCell}. */
export type ActivityLogTableCellProps = Readonly<React.PropsWithChildren>

/** A styled table cell for an {@link ActivityLogSettingsSection}. */
function ActivityLogTableCell(props: ActivityLogTableCellProps) {
  const { children } = props

  return (
    <td className="border-x-2 border-transparent bg-clip-padding px-name-column-x first:rounded-l-full last:rounded-r-full last:border-r-0">
      {children}
    </td>
  )
}
