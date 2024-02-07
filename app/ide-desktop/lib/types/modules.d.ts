/** @file Type definitions for modules that currently don't have typings on DefinitelyTyped.
 *
 * This file MUST NOT `export {}` for the modules to be visible to other files. */

// ===========================
// === Module declarations ===
// ===========================

// Required because this is a build artifact, which does not exist on a clean repository.
declare module '*/build.json' {
    /** Build metadata generated by the build CLI. */
    export interface BuildInfo {
        readonly commit: string
        readonly version: string
        readonly engineVersion: string
        readonly name: string
    }

    const BUILD_INFO: BuildInfo
    export default BUILD_INFO
}

declare module '*/gui/config.yaml' {
    /** Content of the GUI config file. */
    interface Config {
        readonly windowAppScopeName: string
        readonly windowAppScopeConfigName: string
        readonly windowAppScopeThemeName: string
        readonly projectManagerEndpoint: string
        readonly projectManagerHttpEndpoint: string
        readonly minimumSupportedVersion: string
        readonly engineVersionSupported: string
        readonly languageEditionSupported: string
    }

    const DATA: Config
    export default DATA
}

declare module '@eslint/js' {
    /** A set of configurations. */
    export interface Config {
        readonly rules: Record<string, unknown>
    }

    /** Preset configurations defined by ESLint. */
    export interface EslintConfigs {
        readonly all: Config
        readonly recommended: Config
    }

    /** The default export of the module. */
    export interface Default {
        readonly configs: EslintConfigs
    }

    const DEFAULT: Default
    export default DEFAULT
}

declare module 'eslint-plugin-jsdoc' {
    const DEFAULT: unknown
    export default DEFAULT
}

declare module 'eslint-plugin-react' {
    /** An ESLint configuration. */
    interface Config {
        readonly plugins: string[]
        readonly rules: Record<string, number>
        readonly parserOptions: object
    }

    // The names come from a third-party API and cannot be changed.
    /* eslint-disable @typescript-eslint/naming-convention */
    /** Configurations defined by this ESLint plugin. */
    interface Configs {
        readonly recommended: Config
        readonly all: Config
        readonly 'jsx-runtime': Config
    }

    /** Deprecated rules contained in this ESLint plugin. */
    interface DeprecatedRules {
        readonly 'jsx-sort-default-props': object
        readonly 'jsx-space-before-closing': object
    }
    /* eslint-enable @typescript-eslint/naming-convention */

    /** The default export of this ESLint plugin. */
    export interface Default {
        readonly rules: Record<string, object>
        readonly configs: Configs
        readonly deprecatedRules: DeprecatedRules
    }

    // The names come from a third-party API and cannot be changed.
    // eslint-disable-next-line no-restricted-syntax
    export const deprecatedRules: DeprecatedRules

    const DEFAULT: Default
    export default DEFAULT
}

declare module 'eslint-plugin-react-hooks' {
    /** An ESLint configuration. */
    interface Config {
        readonly plugins: string[]
        readonly rules: Record<string, string>
    }

    /** Configurations defined by this ESLint plugin. */
    interface Configs {
        readonly recommended: Config
    }

    /** Rules defined by this ESLint plugin. */
    interface ReactHooksRules {
        // The names come from a third-party API and cannot be changed.
        /* eslint-disable @typescript-eslint/naming-convention */
        readonly 'rules-of-hooks': object
        readonly 'exhaustive-deps': object
        /* eslint-enable @typescript-eslint/naming-convention */
    }

    /** The default export of this ESLint plugin. */
    export interface Default {
        readonly configs: Configs
        readonly rules: ReactHooksRules
    }

    // The names come from a third-party API and cannot be changed.
    /* eslint-disable no-restricted-syntax */
    export const configs: Configs
    export const rules: ReactHooksRules
    /* eslint-enable no-restricted-syntax */

    const DEFAULT: Default
    export default DEFAULT
}

declare module 'esbuild-plugin-time' {
    import type * as esbuild from 'esbuild'

    export default function (name?: string): esbuild.Plugin
}

declare module 'tailwindcss/nesting/index.js' {
    import type * as nested from 'postcss-nested'

    const DEFAULT: nested.Nested
    export default DEFAULT
}

declare module 'create-servers' {
    import type * as http from 'node:http'

    /** Configuration options for `create-servers`. */
    interface CreateServersOptions {
        readonly http: number
        readonly handler: http.RequestListener
    }

    /** An error passed to a callback when a HTTP request fails. */
    interface HttpError {
        readonly http: string
    }

    /** Created server instances of various types. */
    interface CreatedServers {
        readonly http?: http.Server
    }

    export default function (
        option: CreateServersOptions,
        // The types come from a third-party API and cannot be changed.
        // eslint-disable-next-line no-restricted-syntax
        handler: (err: HttpError | undefined, servers: CreatedServers) => void
    ): unknown
}

declare module 'wasm_rust_glue' {
    const DEFAULT: unknown
    export default DEFAULT
}
