/** @file Switcher for choosing the project management backend. */
import * as React from 'react'

import CloudIcon from 'enso-assets/cloud.svg'
import NotCloudIcon from 'enso-assets/not_cloud.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'

import SvgMask from '../../authentication/components/svgMask'

// =======================
// === BackendSwitcher ===
// =======================

/** Props for a {@link BackendSwitcher}. */
export interface BackendSwitcherProps {
    setBackendType: (backendType: backendModule.BackendType) => void
}

/** Switcher for choosing the project management backend. */
export default function BackendSwitcher(props: BackendSwitcherProps) {
    const { setBackendType } = props
    const { backend } = backendProvider.useBackend()
    return (
        <div className="flex shrink-0 gap-px">
            <div
                className={`rounded-l-full px-2.5 py-1 ${
                    backend.type === backendModule.BackendType.remote
                        ? 'bg-frame-selected-bg'
                        : 'bg-frame-bg'
                }`}
            >
                <button
                    onClick={() => {
                        setBackendType(backendModule.BackendType.remote)
                    }}
                    disabled={backend.type === backendModule.BackendType.remote}
                    className={`flex items-center gap-2 ${
                        backend.type === backendModule.BackendType.remote
                            ? 'text-cloud'
                            : 'text-black opacity-30'
                    }`}
                >
                    <SvgMask src={CloudIcon} />
                    <span className="leading-5 h-6 py-px">Cloud</span>
                </button>
            </div>
            <div
                className={`rounded-r-full px-2.5 py-1 ${
                    backend.type === backendModule.BackendType.local
                        ? 'bg-frame-selected-bg'
                        : 'bg-frame-bg'
                }`}
            >
                <button
                    onClick={() => {
                        setBackendType(backendModule.BackendType.local)
                    }}
                    disabled={backend.type === backendModule.BackendType.local}
                    className={`flex items-center gap-2 ${
                        backend.type === backendModule.BackendType.local
                            ? 'text-cloud'
                            : 'text-black opacity-30'
                    }`}
                >
                    <SvgMask src={NotCloudIcon} />
                    <span className="leading-5 h-6 py-px">Local</span>
                </button>
            </div>
        </div>
    )
}
