/** @file Interfaces common to multiple modules. */

// ======================================
// === Globally accessible interfaces ===
// ======================================

/** A configuration in which values may be strings or nested configurations. */
interface StringConfig {
    readonly [key: string]: StringConfig | string
}

/** The value passed from the entrypoint to the dashboard, which enables the dashboard to
 * open a new IDE instance. */
interface AppRunner {
    readonly stopApp: () => void
    readonly runApp: (
        config: StringConfig | null,
        accessToken: string | null,
        metadata?: object
    ) => Promise<void>
}
