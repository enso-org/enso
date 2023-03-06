/** @file declaration enso-gui-server */
declare module 'enso-gui-server' {
    export const DEFAULT_PORT: string
    export const LIVE_RELOAD_LISTENER_PATH: string

    interface StartParams {
        root?: string
        assets?: string
        port?: number
    }
    export function start(params: StartParams): Promise<{
        port: number
        reload(): void
    }>
}
