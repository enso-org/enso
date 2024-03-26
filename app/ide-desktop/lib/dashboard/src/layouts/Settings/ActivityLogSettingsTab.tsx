/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DataUploadIcon from 'enso-assets/data_upload.svg'
import KeyIcon from 'enso-assets/key.svg'
import Play2Icon from 'enso-assets/play2.svg'
import SortAscendingIcon from 'enso-assets/sort_ascending.svg'
import TrashIcon from 'enso-assets/trash.svg'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'

import DateInput from '#/components/DateInput'
import Dropdown from '#/components/Dropdown'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'
import * as sorting from '#/utilities/sorting'

// =========================
// === ActivityLogColumn ===
// =========================

/** Sortable columns in an activity log table. */
enum ActivityLogSortableColumn {
  type = 'type',
  email = 'email',
  timestamp = 'timestamp',
}

// ==============================
// === ActivityLogSettingsTab ===
// ==============================

const EVENT_TYPE_ICON: Record<backendModule.EventType, string> = {
  [backendModule.EventType.GetSecret]: KeyIcon,
  [backendModule.EventType.DeleteAssets]: TrashIcon,
  [backendModule.EventType.ListSecrets]: KeyIcon,
  [backendModule.EventType.OpenProject]: Play2Icon,
  [backendModule.EventType.UploadFile]: DataUploadIcon,
}

const EVENT_TYPE_NAME: Record<backendModule.EventType, string> = {
  [backendModule.EventType.GetSecret]: 'Get Secret',
  [backendModule.EventType.DeleteAssets]: 'Delete Assets',
  [backendModule.EventType.ListSecrets]: 'List Secrets',
  [backendModule.EventType.OpenProject]: 'Open Project',
  [backendModule.EventType.UploadFile]: 'Upload File',
}

/** Settings tab for viewing and editing organization members. */
export default function ActivityLogSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const [startDate, setStartDate] = React.useState<Date | null>(null)
  const [endDate, setEndDate] = React.useState<Date | null>(null)
  const [types, setTypes] = React.useState<readonly backendModule.EventType[]>([])
  const [typeIndices, setTypeIndices] = React.useState<readonly number[]>(() => [])
  const [emails, setEmails] = React.useState<readonly string[]>([])
  const [emailIndices, setEmailIndices] = React.useState<readonly number[]>(() => [])
  const [sortInfo, setSortInfo] =
    React.useState<sorting.SortInfo<ActivityLogSortableColumn> | null>(null)
  const users = asyncEffectHooks.useAsyncEffect([], () => backend.listUsers(), [backend])
  const allEmails = React.useMemo(() => users.map(user => user.email), [users])
  const logs = asyncEffectHooks.useAsyncEffect(null, () => backend.getLogEvents(), [backend])
  const filteredLogs = React.useMemo(() => {
    const typesSet = new Set(types.length > 0 ? types : backendModule.EVENT_TYPES)
    const emailsSet = new Set(emails.length > 0 ? emails : allEmails)
    return logs == null
      ? null
      : logs.filter(log => {
          const date = log.timestamp == null ? null : dateTime.toDate(new Date(log.timestamp))
          return (
            typesSet.has(log.metadata.type) &&
            emailsSet.has(log.userEmail) &&
            (date == null ||
              ((startDate == null || date >= startDate) && (endDate == null || date <= endDate)))
          )
        })
  }, [logs, types, emails, startDate, endDate, allEmails])
  const sortedLogs = React.useMemo(() => {
    if (sortInfo == null || filteredLogs == null) {
      return filteredLogs
    } else {
      let compare: (a: backendModule.Event, b: backendModule.Event) => number
      const multiplier = sortInfo.direction === sorting.SortDirection.ascending ? 1 : -1
      switch (sortInfo.field) {
        case ActivityLogSortableColumn.type: {
          compare = (a, b) =>
            multiplier *
            (a.metadata.type < b.metadata.type ? -1 : a.metadata.type > b.metadata.type ? 1 : 0)
          break
        }
        case ActivityLogSortableColumn.email: {
          compare = (a, b) =>
            multiplier * (a.userEmail < b.userEmail ? -1 : a.userEmail > b.userEmail ? 1 : 0)
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
  const isDescending = sortInfo?.direction === sorting.SortDirection.descending
  const isLoading = sortedLogs == null

  return (
    <div className="flex flex-col gap-settings-subsection">
      <div className="flex flex-col gap-settings-section-header">
        <h3 className="settings-subheading">Activity Log</h3>
        <FocusArea direction="horizontal">
          {(ref, innerProps) => (
            <div ref={ref} className="flex gap-activity-log-filters" {...innerProps}>
              <div className="flex items-center gap-activity-log-filter">
                Start Date
                <DateInput date={startDate} onInput={setStartDate} />
              </div>
              <div className="flex items-center gap-activity-log-filter">
                End Date
                <DateInput date={endDate} onInput={setEndDate} />
              </div>
              <div className="flex items-center gap-activity-log-filter">
                Types
                <Dropdown
                  multiple
                  items={backendModule.EVENT_TYPES}
                  selectedIndices={typeIndices}
                  render={props => EVENT_TYPE_NAME[props.item]}
                  renderMultiple={props =>
                    props.items.length === 0 ||
                    props.items.length === backendModule.EVENT_TYPES.length
                      ? 'All'
                      : (props.items[0] != null ? EVENT_TYPE_NAME[props.items[0]] : '') +
                        (props.items.length <= 1 ? '' : ` (+${props.items.length - 1})`)
                  }
                  onClick={(items, indices) => {
                    setTypes(items)
                    setTypeIndices(indices)
                  }}
                />
              </div>
              <div className="flex items-center gap-activity-log-filter">
                Users
                <Dropdown
                  multiple
                  items={allEmails}
                  selectedIndices={emailIndices}
                  render={props => props.item}
                  renderMultiple={props =>
                    props.items.length === 0 || props.items.length === allEmails.length
                      ? 'All'
                      : (props.items[0] ?? '') +
                        (props.items.length <= 1 ? '' : `(+${props.items.length - 1})`)
                  }
                  onClick={(items, indices) => {
                    setEmails(items)
                    setEmailIndices(indices)
                  }}
                />
              </div>
            </div>
          )}
        </FocusArea>
        <table className="table-fixed self-start rounded-rows">
          <thead>
            <tr className="h-row">
              <th className="w-activity-log-icon-column border-x-2 border-transparent bg-clip-padding pl-cell-x pr-icon-column-r text-left text-sm font-semibold last:border-r-0" />
              <th className="w-activity-log-type-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                <button
                  title={
                    sortInfo?.field !== ActivityLogSortableColumn.type
                      ? 'Sort by name'
                      : isDescending
                        ? 'Stop sorting by name'
                        : 'Sort by name descending'
                  }
                  className="group flex h-drive-table-heading w-full items-center gap-icon-with-text px-name-column-x"
                  onClick={event => {
                    event.stopPropagation()
                    const nextDirection =
                      sortInfo?.field === ActivityLogSortableColumn.type
                        ? sorting.nextSortDirection(sortInfo.direction)
                        : sorting.SortDirection.ascending
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
                  <span className="text-header">Type</span>
                  <img
                    alt={
                      sortInfo?.field === ActivityLogSortableColumn.type && isDescending
                        ? 'Sort Descending'
                        : 'Sort Ascending'
                    }
                    src={SortAscendingIcon}
                    className={`transition-all duration-arrow ${
                      sortInfo?.field === ActivityLogSortableColumn.type
                        ? 'selectable active'
                        : 'transparent group-hover:selectable'
                    } ${isDescending ? 'rotate-180' : ''}`}
                  />
                </button>
              </th>
              <th className="w-activity-log-email-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                <button
                  title={
                    sortInfo?.field !== ActivityLogSortableColumn.email
                      ? 'Sort by email'
                      : isDescending
                        ? 'Stop sorting by email'
                        : 'Sort by email descending'
                  }
                  className="group flex h-drive-table-heading w-full items-center gap-icon-with-text px-name-column-x"
                  onClick={event => {
                    event.stopPropagation()
                    const nextDirection =
                      sortInfo?.field === ActivityLogSortableColumn.email
                        ? sorting.nextSortDirection(sortInfo.direction)
                        : sorting.SortDirection.ascending
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
                  <span className="text-header">Email</span>
                  <img
                    alt={
                      sortInfo?.field === ActivityLogSortableColumn.email && isDescending
                        ? 'Sort Descending'
                        : 'Sort Ascending'
                    }
                    src={SortAscendingIcon}
                    className={`transition-all duration-arrow ${
                      sortInfo?.field === ActivityLogSortableColumn.email
                        ? 'selectable active'
                        : 'transparent group-hover:selectable'
                    } ${isDescending ? 'rotate-180' : ''}`}
                  />
                </button>
              </th>
              <th className="w-activity-log-timestamp-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                <button
                  title={
                    sortInfo?.field !== ActivityLogSortableColumn.timestamp
                      ? 'Sort by timestamp'
                      : isDescending
                        ? 'Stop sorting by timestamp'
                        : 'Sort by timestamp descending'
                  }
                  className="group flex h-drive-table-heading w-full items-center gap-icon-with-text px-name-column-x"
                  onClick={event => {
                    event.stopPropagation()
                    const nextDirection =
                      sortInfo?.field === ActivityLogSortableColumn.timestamp
                        ? sorting.nextSortDirection(sortInfo.direction)
                        : sorting.SortDirection.ascending
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
                  <span className="text-header">Timestamp</span>
                  <img
                    alt={
                      sortInfo?.field === ActivityLogSortableColumn.timestamp && isDescending
                        ? 'Sort Descending'
                        : 'Sort Ascending'
                    }
                    src={SortAscendingIcon}
                    className={`transition-all duration-arrow ${
                      sortInfo?.field === ActivityLogSortableColumn.timestamp
                        ? 'selectable active'
                        : 'transparent group-hover:selectable'
                    } ${isDescending ? 'rotate-180' : ''}`}
                  />
                </button>
              </th>
            </tr>
          </thead>
          <tbody className="select-text">
            {isLoading ? (
              <tr className="h-row">
                <td colSpan={4} className="rounded-full bg-transparent">
                  <div className="flex justify-center">
                    <StatelessSpinner
                      size={32}
                      state={statelessSpinner.SpinnerState.loadingMedium}
                    />
                  </div>
                </td>
              </tr>
            ) : (
              sortedLogs.map((log, i) => (
                <tr key={i} className="h-row">
                  <td className="border-x-2 border-transparent bg-clip-padding pl-cell-x pr-icon-column-r first:rounded-l-full last:rounded-r-full last:border-r-0">
                    <div className="flex items-center">
                      <SvgMask src={EVENT_TYPE_ICON[log.metadata.type]} />
                    </div>
                  </td>
                  <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                    {EVENT_TYPE_NAME[log.metadata.type]}
                  </td>
                  <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                    {log.userEmail}
                  </td>
                  <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                    {log.timestamp ? dateTime.formatDateTime(new Date(log.timestamp)) : ''}
                  </td>
                </tr>
              ))
            )}
          </tbody>
        </table>
      </div>
    </div>
  )
}
