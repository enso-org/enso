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

    private autoFlushOn() {
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
        this.autoFlushOn()
    }
}

export const router = new Router()

host.exportGlobal({ hideLogs: router.hideLogs, showLogs: router.showLogs })
