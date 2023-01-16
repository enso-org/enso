/** @file Log router allowing hiding all logs in the web console and showing them on demand. It is
 * used for disabling logs in the production build in order to prevent cluttering the console. */

import host from 'runner/host'

// ==============
// === Router ===
// ==============

const consoleLogNames = [
    'log',
    'info',
    'debug',
    'warn',
    'error',
    'group',
    'groupCollapsed',
    'groupEnd',
] satisfies (keyof Console)[]

// FIXME: fix Rust `autoFlush` handling
class Router {
    private buffer: { name: string; args: unknown[] }[]
    private readonly console: Record<string, (...args: unknown[]) => void>
    autoFlush: boolean

    constructor() {
        this.buffer = []
        this.console = {}
        this.autoFlush = true
        for (const name of consoleLogNames) {
            this.console[name] = console[name]
            console[name] = (...args: unknown[]) => {
                this.consume(name, args)
            }
        }
    }

    private autoFlushOn() {
        this.autoFlush = true
        for (const { name, args } of this.buffer) {
            const fn = this.console[name]
            if (fn) {
                fn(...args)
            } else {
                console.error(`Unknown log name '${name}'.`)
            }
        }
        this.buffer = []
    }

    private consume(name: string, args: unknown[]) {
        if (this.autoFlush) {
            const fn = this.console[name]
            if (fn) {
                fn(...args)
            } else {
                console.error(`Unknown log name '${name}'.`)
            }
        } else {
            this.buffer.push({ name, args })
        }
    }

    hideLogs() {
        console.log('All subsequent logs will be hidden. Eval `showLogs()` to reveal them.')
        this.autoFlush = false
    }

    showLogs() {
        this.autoFlushOn()
    }
}

export const router = new Router()

host.exportGlobal({
    hideLogs: router.hideLogs.bind(router),
    showLogs: router.showLogs.bind(router),
})
