/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DataUploadIcon from 'enso-assets/data_upload.svg'
import KeyIcon from 'enso-assets/key.svg'
import Play2Icon from 'enso-assets/play2.svg'
import TrashIcon from 'enso-assets/trash.svg'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'

import DateInput from '#/components/DateInput'
import Dropdown from '#/components/Dropdown'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'

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
  const isLoading = filteredLogs == null

  return (
    <div className="flex flex-col gap-8">
      <div className="flex flex-col gap-2.5">
        <h3 className="h-9.5 py-0.5 text-xl font-bold">Activity Log</h3>
        <div className="flex gap-3">
          <div className="flex items-center gap-2">
            Start Date
            <DateInput date={startDate} onInput={setStartDate} />
          </div>
          <div className="flex items-center gap-2">
            End Date
            <DateInput date={endDate} onInput={setEndDate} />
          </div>
          <div className="flex items-center gap-2">
            Types
            <Dropdown
              multiple
              items={backendModule.EVENT_TYPES}
              selectedIndices={typeIndices}
              render={props => EVENT_TYPE_NAME[props.item]}
              renderMultiple={props =>
                props.items.length === 0 || props.items.length === backendModule.EVENT_TYPES.length
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
          <div className="flex items-center gap-2">
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
        <table className="table-fixed self-start rounded-rows">
          <thead>
            <tr className="h-8">
              <th className="w-8 border-x-2 border-transparent bg-clip-padding pl-2 pr-1.5 text-left text-sm font-semibold last:border-r-0" />
              <th className="w-32 border-x-2 border-transparent bg-clip-padding px-2 text-left text-sm font-semibold last:border-r-0">
                Type
              </th>
              <th className="w-48 border-x-2 border-transparent bg-clip-padding px-2 text-left text-sm font-semibold last:border-r-0">
                Email
              </th>
              <th className="w-36 border-x-2 border-transparent bg-clip-padding px-2 text-left text-sm font-semibold last:border-r-0">
                Timestamp
              </th>
            </tr>
          </thead>
          <tbody className="select-text">
            {isLoading ? (
              <tr className="h-8">
                <td colSpan={4} className="rounded-full">
                  <div className="flex justify-center">
                    <StatelessSpinner
                      size={32}
                      state={statelessSpinner.SpinnerState.loadingMedium}
                    />
                  </div>
                </td>
              </tr>
            ) : (
              filteredLogs.map((log, i) => (
                <tr key={i} className="h-8">
                  <td className="border-x-2 border-transparent bg-clip-padding pl-2 pr-1.5 first:rounded-l-full last:rounded-r-full last:border-r-0">
                    <div className="flex items-center">
                      <SvgMask src={EVENT_TYPE_ICON[log.metadata.type]} />
                    </div>
                  </td>
                  <td className="border-x-2 border-transparent bg-clip-padding px-2 first:rounded-l-full last:rounded-r-full last:border-r-0">
                    {EVENT_TYPE_NAME[log.metadata.type]}
                  </td>
                  <td className="border-x-2 border-transparent bg-clip-padding px-2 first:rounded-l-full last:rounded-r-full last:border-r-0">
                    {log.userEmail}
                  </td>
                  <td className="border-x-2 border-transparent bg-clip-padding px-2 first:rounded-l-full last:rounded-r-full last:border-r-0">
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
