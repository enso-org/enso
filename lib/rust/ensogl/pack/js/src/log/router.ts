import host from 'system/host'

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
    private buffer: { name: string; args: any[] }[]
    private readonly console: any
    autoFlush: boolean

    constructor() {
        this.buffer = []
        this.console = {}
        this.autoFlush = true
        for (const name of consoleLogNames) {
            this.console[name] = console[name]
            console[name] = (...args: any[]) => {
                this.consume(name, args)
            }
        }
    }

    private auto_flush_on() {
        this.autoFlush = true
        for (const { name, args } of this.buffer) {
            this.console[name](...args)
        }
        this.buffer = []
    }

    private consume(name: string, args: any[]) {
        if (this.autoFlush) {
            this.console[name](...args)
        } else {
            this.buffer.push({ name, args })
        }
    }

    hideLogs() {
        console.log('All subsequent logs will be hidden. Eval `showLogs()` to reveal them.')
        this.autoFlush = false
    }

    showLogs() {
        this.auto_flush_on()
    }
}

export const logRouter = new Router()

host.exportGlobal({ logRouter, hideLogs: logRouter.hideLogs, showLogs: logRouter.showLogs })
