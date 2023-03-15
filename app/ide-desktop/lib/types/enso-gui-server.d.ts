/** @file Type definitions for the `enso-gui-server` module. */
declare module 'enso-gui-server' {
    export const DEFAULT_PORT: string
    export const LIVE_RELOAD_LISTENER_PATH: string

    interface StartParams {
        root?: string
        assets?: string
        port?: number
    }
    interface ExectionInfo {
        port: number
        reload: () => void
    }
    export function start(params: StartParams): Promise<ExectionInfo>
}
