/** @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code.
 *
 * This file MUST `export {}` for the globals to be visible to other files. */

interface StringConfig {
    [key: string]: StringConfig | string
}

interface Enso {
    main: (inputConfig?: StringConfig) => Promise<void>
}

interface BuildInfo {
    commit: string
    version: string
    engineVersion: string
    name: string
}

declare global {
    interface Window {
        liveReloadListening?: boolean
        enso: Enso
    }

    namespace NodeJS {
        interface ProcessEnv {
            /* eslint-disable @typescript-eslint/naming-convention */
            APPLEID: string
            APPLEIDPASS: string
            /* eslint-enable @typescript-eslint/naming-convention */
        }
    }

    // These are used in other files (because they're globals)
    /* eslint-disable @typescript-eslint/naming-convention */
    const BUNDLED_ENGINE_VERSION: string
    const PROJECT_MANAGER_IN_BUNDLE_PATH: string
    const BUILD_INFO: BuildInfo
    const PROJECT_MANAGER_IN_BUNDLE_PATH: string | undefined
    /* eslint-disable @typescript-eslint/naming-convention */
}

export {}
