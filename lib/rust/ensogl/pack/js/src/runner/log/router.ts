/** @file Log router allowing hiding all logs in the web console and showing them on demand. It is
 * used for disabling logs in the production build in order to prevent cluttering the console. */

import host from '../host.js'

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
/** Router for logs. It is used to hide the incoming logs and show them on demand. It is used to
 * unclutter the logs view when the app is run by end-user. */
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

    /** set the auto-flush to on. All subsequent logs will not be buffered and will be immediately
     * redirected to log consumers. */
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

    /** Consume a new message. Add it to the message buffer if auto-flush is off. Print it with
     * logger otherwise. */
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

    /** Hide all subsequent logs until the `showLogs` method is called. */
    hideLogs() {
        console.log('All subsequent logs will be hidden. Eval `showLogs()` to reveal them.')
        this.autoFlush = false
    }

    /** Display all hidden logs and do not hide any subsequent logs. */
    showLogs() {
        this.autoFlushOn()
    }
}

// ===============
// === Exports ===
// ===============

export const router = new Router()

host.exportGlobal({
    hideLogs: router.hideLogs.bind(router),
    showLogs: router.showLogs.bind(router),
})
