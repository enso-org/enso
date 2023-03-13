/** @file typescript declaration on dev environment  */
import * as esbuild from 'esbuild'

declare global {
    namespace NodeJS {
        interface ProcessEnv {
            /* eslint-disable @typescript-eslint/naming-convention */
            APPLEID: string
            APPLEIDPASS: string
            /* eslint-enable @typescript-eslint/naming-convention */
        }
    }
}

declare module '*.yaml' {
    const DATA: unknown
    export default DATA
}
declare module 'esbuild-plugin-time' {
    export default function (name?: string): esbuild.Plugin
}
declare module 'create-servers' {
    import Http from 'http'
    interface CreateServersOptions {
        http: number
        handler: RequestListener<Http.IncomingMessage, Http.ServerResponse>
    }
    export default function(
        option: CreateServersOptions,
        errorHandler: (err: { http: string } | null) => void
    ): unknown
}

declare global {
    // These are used in other files (because they're globals)
    // eslint-disable-next-line @typescript-eslint/naming-convention
    const BUNDLED_ENGINE_VERSION: string
    // eslint-disable-next-line @typescript-eslint/naming-convention
    const PROJECT_MANAGER_IN_BUNDLE_PATH: string
    // eslint-disable-next-line @typescript-eslint/naming-convention
    const BUILD_INFO: {
        default: {
            commit: string
            version: string
            engineVersion: string
            name: string
        }
    }
}
