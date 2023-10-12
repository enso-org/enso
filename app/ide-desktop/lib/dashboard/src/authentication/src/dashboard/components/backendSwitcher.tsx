/** @file Switcher for choosing the project management backend. */
import * as React from 'react'

import CloudIcon from 'enso-assets/cloud.svg'
import NotCloudIcon from 'enso-assets/not_cloud.svg'

import SvgMask from '../../authentication/components/svgMask'
import * as backendProvider from '../../providers/backend'
import * as backendModule from '../backend'

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
            <button
                disabled={backend.type === backendModule.BackendType.remote}
                className={`rounded-l-full px-2.5 py-1 ${
                    backend.type === backendModule.BackendType.remote
                        ? 'bg-frame-selected text-cloud'
                        : 'bg-frame text-black'
                }`}
                onClick={() => {
                    setBackendType(backendModule.BackendType.remote)
                }}
            >
                <div
                    className={`flex items-center gap-2 ${
                        backend.type === backendModule.BackendType.remote ? '' : 'opacity-30'
                    }`}
                >
                    <SvgMask src={CloudIcon} />
                    <span className="h-6 py-px leading-5">Cloud</span>
                </div>
            </button>
            <button
                disabled={backend.type === backendModule.BackendType.local}
                className={`rounded-r-full px-2.5 py-1 ${
                    backend.type === backendModule.BackendType.local
                        ? 'bg-frame-selected text-cloud'
                        : 'bg-frame text-black'
                }`}
                onClick={() => {
                    setBackendType(backendModule.BackendType.local)
                }}
            >
                <div
                    className={`flex items-center gap-2 ${
                        backend.type === backendModule.BackendType.local ? '' : 'opacity-30'
                    }`}
                >
                    <SvgMask src={NotCloudIcon} />
                    <span className="h-6 py-px leading-5">Local</span>
                </div>
            </button>
        </div>
    )
}
