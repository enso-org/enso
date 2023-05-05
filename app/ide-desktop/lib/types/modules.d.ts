/** @file Type definitions for modules that currently don't have typings on DefinitelyTyped.
 *
 * This file MUST NOT `export {}` for the modules to be visible to other files. */

// Required because this is a build artifact and so would not otherwise work on a clean repository.
declare module '*/build.json' {
    interface BuildInfo {
        commit: string
        version: string
        engineVersion: string
        name: string
    }

    const BUILD_INFO: BuildInfo
    export default BUILD_INFO
}

// Required because this is a build artifact and so would not otherwise work on a clean repository.
declare module '*/ensogl-pack/linked-dist' {
    /* eslint-disable @typescript-eslint/consistent-type-imports, no-restricted-syntax, @typescript-eslint/no-shadow */
    export type App = import('runner/index').App
    export const App: typeof import('runner/index').App
    export type Consumer = import('runner/index').Consumer
    export const Consumer: typeof import('runner/index').Consumer
    export type LogLevel = import('runner/index').LogLevel
    export const LogLevel: typeof import('runner/index').LogLevel
    export type Logger = import('runner/index').Logger
    export const Logger: typeof import('runner/index').Logger
    export type Option<T> = import('runner/index').Option<T>
    export const Option: typeof import('runner/index').Option
    export const log: typeof import('runner/index').log
    export const logger: typeof import('runner/index').logger
    export const urlParams: typeof import('runner/index').urlParams
    export namespace config {
        export type Option<T> = import('runner/index').config.Option<T>
        export const Option: typeof import('runner/index').config.Option
        export type Group<
            Options extends OptionsRecord,
            Groups extends GroupsRecord
        > = typeof import('runner/index').config.Group<Options, Groups>
        export const Group: typeof import('runner/index').config.Group
        export const objectToGroup: typeof import('runner/index').config.objectToGroup
        export const objectToOption: typeof import('runner/index').config.objectToOption
        export const options: typeof import('runner/index').config.options
    }
    /* eslint-enable @typescript-eslint/consistent-type-imports, no-restricted-syntax, @typescript-eslint/no-shadow */
}

declare module '*/gui/config.yaml' {
    interface Config {
        windowAppScopeName: string
        windowAppScopeConfigName: string
        windowAppScopeThemeName: string
        projectManagerEndpoint: string
        minimumSupportedVersion: string
        engineVersionSupported: string
        languageEditionSupported: string
    }

    const DATA: Config
    export default DATA
}

declare module '@eslint/js' {
    interface Config {
        rules: Record<string, string>
    }

    interface EslintConfigs {
        all: Config
        recommended: Config
    }

    interface Default {
        configs: EslintConfigs
    }

    const DEFAULT: Default
    export default DEFAULT
}

declare module 'eslint-plugin-jsdoc' {
    const DEFAULT: unknown
    export default DEFAULT
}

declare module 'esbuild-plugin-time' {
    import * as esbuild from 'esbuild'

    export default function (name?: string): esbuild.Plugin
}

declare module 'tailwindcss/nesting/index.js' {
    import * as nested from 'postcss-nested'

    const DEFAULT: nested.Nested
    export default DEFAULT
}

declare module 'create-servers' {
    import * as http from 'node:http'

    interface CreateServersOptions {
        http: number
        handler: http.RequestListener
    }
    interface HttpError {
        http: string
    }
    export default function (
        option: CreateServersOptions,
        // This is a third-party module which we have no control over.
        // eslint-disable-next-line no-restricted-syntax
        errorHandler: (err: HttpError | undefined) => void
    ): unknown
}

declare module 'wasm_rust_glue' {
    const DEFAULT: unknown
    export default DEFAULT
}
