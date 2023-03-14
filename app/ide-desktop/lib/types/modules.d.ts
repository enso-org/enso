/** @file Type definitions for modules that currently don't have typings on DefinitelyTyped.
 *
 * This file MUST NOT `export {}` for the modules to be visible to other files. */

declare module '*.yaml' {
    const DATA: unknown
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

declare module 'create-servers' {
    import * as http from 'node:http'

    interface CreateServersOptions {
        http: number
        handler: http.RequestListener<http.IncomingMessage, http.ServerResponse>
    }
    interface HttpError {
        http: string
    }
    export default function (
        option: CreateServersOptions,
        errorHandler: (err: HttpError | undefined) => void
    ): unknown
}

declare module 'wasm_rust_glue' {
    const DEFAULT: unknown
    export default DEFAULT
}
