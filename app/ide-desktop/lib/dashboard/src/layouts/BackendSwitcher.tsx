/** @file Switcher for choosing the project management backend. */
import * as React from 'react'

import CloudIcon from 'enso-assets/cloud.svg'
import NotCloudIcon from 'enso-assets/not_cloud.svg'

import * as backendProvider from '#/providers/BackendProvider'

import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

// =======================
// === BackendSwitcher ===
// =======================

/** Props for a {@link BackendSwitcher}. */
export interface BackendSwitcherProps {
  readonly setBackendType: (backendType: backendModule.BackendType) => void
}

/** Switcher for choosing the project management backend. */
export default function BackendSwitcher(props: BackendSwitcherProps) {
  const { setBackendType } = props
  const { backend } = backendProvider.useBackend()
  const isCloud = backend.type === backendModule.BackendType.remote

  return (
    <div className="flex shrink-0 gap-px">
      <button
        disabled={isCloud}
        className={`selectable ${
          isCloud ? 'active' : ''
        } flex flex-col items-start first:rounded-l-full last:rounded-r-full px-selector-x py-selector-y w-backend-switcher-option text-primary bg-selected-frame disabled:text-cloud`}
        onClick={() => {
          setBackendType(backendModule.BackendType.remote)
        }}
      >
        <div className="flex items-center gap-icon-with-text">
          <SvgMask src={CloudIcon} />
          <span className="text">Cloud</span>
        </div>
      </button>
      <button
        disabled={!isCloud}
        className={`selectable ${
          !isCloud ? 'active' : ''
        } flex flex-col items-start first:rounded-l-full last:rounded-r-full px-selector-x py-selector-y w-backend-switcher-option bg-selected-frame text-primary disabled:text-cloud`}
        onClick={() => {
          setBackendType(backendModule.BackendType.local)
        }}
      >
        <div className="flex items-center gap-icon-with-text">
          <SvgMask src={NotCloudIcon} />
          <span className="text">Local</span>
        </div>
      </button>
    </div>
  )
}
