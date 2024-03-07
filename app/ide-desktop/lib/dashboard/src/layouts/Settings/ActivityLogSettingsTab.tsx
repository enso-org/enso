/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DataUploadIcon from 'enso-assets/data_upload.svg'
import KeyIcon from 'enso-assets/key.svg'
import Play2Icon from 'enso-assets/play2.svg'
import TrashIcon from 'enso-assets/trash.svg'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

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
  const users = asyncEffectHooks.useAsyncEffect([], () => backend.listUsers(), [backend])
  const logs = asyncEffectHooks.useAsyncEffect(null, () => backend.getLogEvents(), [backend])
  const isLoading = logs == null

  return (
    <div className="flex flex-col gap-8">
      <div className="flex flex-col gap-2.5">
        <h3 className="font-bold text-xl h-9.5 py-0.5">Activity Log</h3>
        <div className="flex gap-2.5">
          <button
            className="flex items-center bg-frame rounded-full h-8 px-2.5"
            onClick={event => {
              event.stopPropagation()
            }}
          >
            <span className="font-semibold whitespace-nowrap leading-5 h-6 py-px">
              Filter By Type (TODO)
            </span>
          </button>
        </div>
        <table className="self-start table-fixed rounded-rows">
          <thead>
            <tr className="h-8">
              <th className="text-left bg-clip-padding border-transparent border-x-2 last:border-r-0 text-sm font-semibold w-8 pl-2 pr-1.5" />
              <th className="text-left bg-clip-padding border-transparent border-x-2 last:border-r-0 text-sm font-semibold w-32 px-2">
                Event
              </th>
              <th className="text-left bg-clip-padding border-transparent border-x-2 last:border-r-0 text-sm font-semibold w-48 px-2">
                Email
              </th>
            </tr>
          </thead>
          <tbody className="select-text">
            {isLoading ? (
              <tr className="h-8">
                <td colSpan={3}>
                  <div className="flex justify-center">
                    <StatelessSpinner
                      size={32}
                      state={statelessSpinner.SpinnerState.loadingMedium}
                    />
                  </div>
                </td>
              </tr>
            ) : (
              logs.map((log, i) => (
                <tr key={i} className="h-8">
                  <td className="pl-2 pr-1.5 bg-clip-padding border-transparent border-x-2 last:border-r-0 first:rounded-l-full last:rounded-r-full">
                    <div className="flex items-center">
                      <SvgMask src={EVENT_TYPE_ICON[log.metadata.type]} />
                    </div>
                  </td>
                  <td className="px-2 bg-clip-padding border-transparent border-x-2 last:border-r-0 first:rounded-l-full last:rounded-r-full">
                    {EVENT_TYPE_NAME[log.metadata.type]}
                  </td>
                  <td className="px-2 bg-clip-padding border-transparent border-x-2 last:border-r-0 first:rounded-l-full last:rounded-r-full">
                    {log.userEmail}
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
