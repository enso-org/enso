/** @file Switcher for choosing the project management backend. */
import * as React from 'react'

import CloudIcon from 'enso-assets/cloud.svg'
import NotCloudIcon from 'enso-assets/not_cloud.svg'

import SvgMask from '#/components/SvgMask'

import * as backendModule from '#/services/Backend'

// =======================
// === BackendSwitcher ===
// =======================

/** Props for a {@link BackendSwitcher}. */
export interface BackendSwitcherProps {
  readonly backendType: backendModule.BackendType
  readonly setBackendType: (backendType: backendModule.BackendType) => void
}

/** Switcher for choosing the project management backend. */
export default function BackendSwitcher(props: BackendSwitcherProps) {
  const { backendType, setBackendType } = props

  return (
    <div className="flex shrink-0 gap-px">
      <button
        disabled={backendType === backendModule.BackendType.remote}
        title="Switch to cloud drive"
        className={`rounded-l-full px-2.5 py-1 ${
          backendType === backendModule.BackendType.remote
            ? 'bg-frame-selected text-cloud'
            : 'bg-frame text-black'
        }`}
        onClick={() => {
          setBackendType(backendModule.BackendType.remote)
        }}
      >
        <div
          className={`flex items-center gap-2 ${
            backendType === backendModule.BackendType.remote ? '' : 'opacity-30'
          }`}
        >
          <SvgMask src={CloudIcon} />
          <span className="leading-5 h-6 py-px">Cloud</span>
        </div>
      </button>
      <button
        disabled={backendType === backendModule.BackendType.local}
        title="Switch to local drive"
        className={`rounded-r-full px-2.5 py-1 ${
          backendType === backendModule.BackendType.local
            ? 'bg-frame-selected text-cloud'
            : 'bg-frame text-black'
        }`}
        onClick={() => {
          setBackendType(backendModule.BackendType.local)
        }}
      >
        <div
          className={`flex items-center gap-2 ${
            backendType === backendModule.BackendType.local ? '' : 'opacity-30'
          }`}
        >
          <SvgMask src={NotCloudIcon} />
          <span className="leading-5 h-6 py-px">Local</span>
        </div>
      </button>
    </div>
  )
}
