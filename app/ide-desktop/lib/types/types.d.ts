/** @file Interfaces common to multiple modules. */
import React from 'react'

// ======================================
// === Globally accessible interfaces ===
// ======================================

/** A configuration in which values may be strings or nested configurations. */
interface StringConfig {
    readonly [key: string]: StringConfig | string
}

interface AppProps {
    config: StringConfig | null
    projectId: string
    metadata?: object
    logEvent(message: string, projectId?: string | undefined, metadata?: object): void
}

/** The value passed from the entrypoint to the dashboard, which enables the dashboard to
 * open a new IDE instance. */
type AppRunner = React.ComponentType<AppProps>
