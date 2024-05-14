/** @file Interfaces common to multiple modules. */
import React from 'react'

// ======================================
// === Globally accessible interfaces ===
// ======================================

/** A configuration in which values may be strings or nested configurations. */
interface StringConfig {
    readonly [key: string]: StringConfig | string
}

interface EditorProps {
    config: StringConfig | null
    projectId: string
    hidden?: boolean
    ignoreParamsRegex?: RegExp
    logEvent(message: string, projectId?: string | undefined, metadata?: object): void
}

/** The value passed from the entrypoint to the dashboard, which enables the dashboard to
 * open a new IDE instance. */
type EditorRunner = React.ComponentType<EditorProps>
