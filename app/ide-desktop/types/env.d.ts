/** @file typescript declaration on dev environment  */
declare module '*.yaml' {
    const data: any
    export default data
}
declare module 'esbuild-plugin-time' {
    export default function (name?: string): import('esbuild').Plugin
}
declare module 'create-servers' {
    import Http from 'http'
    interface CreateServersOptions {
        http: number
        handler: RequestListener<Http.IncomingMessage, Http.ServerResponse>
    }
    export default function (
        option: CreateServersOptions,
        errorHandler: (err: { http: string } | null) => void
    ): any
}

declare const BUNDLED_ENGINE_VERSION: string
declare const PROJECT_MANAGER_IN_BUNDLE_PATH: string
declare const BUILD_INFO: {
    default: {
        commit: string
        version: string
        engineVersion: string
        name: string
    }
}
