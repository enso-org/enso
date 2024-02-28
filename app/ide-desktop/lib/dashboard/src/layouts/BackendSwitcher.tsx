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
  return (
    <div className="flex shrink-0 gap-px">
      <button
        disabled={backend.type === backendModule.BackendType.remote}
        className="flex flex-col items-start first:rounded-l-full last:rounded-r-full px-selector-x py-selector-y w-backend-switcher-option bg-frame text-black disabled:bg-selected-frame disabled:text-cloud"
        onClick={() => {
          setBackendType(backendModule.BackendType.remote)
        }}
      >
        <div
          className={`flex items-center gap-selector-icon-with-text ${
            backend.type === backendModule.BackendType.remote ? '' : 'opacity-30'
          }`}
        >
          <SvgMask src={CloudIcon} />
          <span className="text">Cloud</span>
        </div>
      </button>
      <button
        disabled={backend.type === backendModule.BackendType.local}
        className="flex flex-col items-start first:rounded-l-full last:rounded-r-full px-selector-x py-selector-y w-backend-switcher-option bg-frame text-black disabled:bg-selected-frame disabled:text-cloud"
        onClick={() => {
          setBackendType(backendModule.BackendType.local)
        }}
      >
        <div
          className={`flex items-center gap-selector-icon-with-text ${
            backend.type === backendModule.BackendType.local ? '' : 'opacity-30'
          }`}
        >
          <SvgMask src={NotCloudIcon} />
          <span className="text">Local</span>
        </div>
      </button>
    </div>
  )
}
