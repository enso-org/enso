/** @file Interfaces common to multiple modules. */
import type * as React from 'react'

import type Backend from './services/Backend'

// ======================================
// === Globally accessible interfaces ===
// ======================================

/** A configuration in which values may be strings or nested configurations. */
interface StringConfig {
    readonly [key: string]: StringConfig | string
}

/** Props for GUI editor root component. */
interface EditorProps {
    readonly config: StringConfig | null
    readonly projectId: string
    readonly hidden: boolean
    readonly ignoreParamsRegex?: RegExp
    readonly logEvent: (
        message: string,
        projectId?: string | null,
        metadata?: object | null
    ) => void
    readonly renameProject: (newName: string) => void
    readonly backend: Backend | null
}

/** The value passed from the entrypoint to the dashboard, which enables the dashboard to
 * open a new IDE instance. */
type EditorRunner = React.ComponentType<EditorProps>
