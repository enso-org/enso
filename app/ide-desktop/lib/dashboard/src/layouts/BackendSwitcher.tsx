/** @file Switcher for choosing the project management backend. */
import * as React from 'react'

import CloudIcon from 'enso-assets/cloud.svg'
import NotCloudIcon from 'enso-assets/not_cloud.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

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
  const rootRef = React.useRef<HTMLDivElement>(null)
  const navigator2D = navigator2DProvider.useNavigator2D()
  const isCloud = backend.type === backendModule.BackendType.remote

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      return navigator2D.register(root)
    }
  }, [navigator2D])

  return (
    <div ref={rootRef} className="flex shrink-0 gap-px">
      <button
        disabled={isCloud}
        className="flex w-backend-switcher-option flex-col items-start bg-selected-frame px-selector-x py-selector-y text-primary selectable first:rounded-l-full last:rounded-r-full disabled:text-cloud disabled:active"
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
        className="flex w-backend-switcher-option flex-col items-start bg-selected-frame px-selector-x py-selector-y text-primary selectable first:rounded-l-full last:rounded-r-full disabled:text-cloud disabled:active"
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
