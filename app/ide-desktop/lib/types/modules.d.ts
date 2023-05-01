/** @file Type definitions for modules that currently don't have typings on DefinitelyTyped.
 *
 * This file MUST NOT `export {}` for the modules to be visible to other files. */

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

declare module 'eslint-plugin-react' {
    interface Config {
        plugins: string[]
        rules: Record<string, number>
        parserOptions: object
    }

    // The names come from a third-party API and cannot be changed.
    /* eslint-disable @typescript-eslint/naming-convention */
    interface Configs {
        recommended: Config
        all: Config
        'jsx-runtime': Config
    }

    interface DeprecatedRules {
        'jsx-sort-default-props': object
        'jsx-space-before-closing': object
    }
    /* eslint-enable @typescript-eslint/naming-convention */

    interface Default {
        rules: Record<string, object>
        configs: Configs
        deprecatedRules: DeprecatedRules
    }

    // The names come from a third-party API and cannot be changed.
    // eslint-disable-next-line no-restricted-syntax
    export const deprecatedRules: DeprecatedRules

    const DEFAULT: Default
    export default DEFAULT
}

declare module 'eslint-plugin-react-hooks' {
    interface Config {
        plugins: string[]
        rules: Record<string, string>
    }

    interface Configs {
        recommended: Config
    }

    interface ReactHooksRules {
        // The names come from a third-party API and cannot be changed.
        /* eslint-disable @typescript-eslint/naming-convention */
        'rules-of-hooks': object
        'exhaustive-deps': object
        /* eslint-enable @typescript-eslint/naming-convention */
    }

    interface Default {
        configs: Configs
        rules: ReactHooksRules
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
        handler: http.RequestListener<http.IncomingMessage, http.ServerResponse>
    }
    interface HttpError {
        http: string
    }
    export default function (
        option: CreateServersOptions,
        // The types come from a third-party API and cannot be changed.
        // eslint-disable-next-line no-restricted-syntax
        errorHandler: (err: HttpError | undefined) => void
    ): unknown
}

declare module 'wasm_rust_glue' {
    const DEFAULT: unknown
    export default DEFAULT
}
