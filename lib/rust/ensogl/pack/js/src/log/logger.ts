import host from 'system/host'

// =============
// === Utils ===
// =============

function isObject(value: any): boolean {
    return typeof value === 'object' && !Array.isArray(value) && value !== null
}

// ==============
// === Colors ===
// ==============

class Colors {
    static resetCode = '\x1b[0m'
    static redCode = '\x1b[31m'
    static orangeCode = '\x1b[33m'
    static red(text: string): string {
        return Colors.redCode + text + Colors.resetCode
    }
    static orange(text: string): string {
        return Colors.orangeCode + text + Colors.resetCode
    }
    static bold_start(): string {
        return '\x1b[1m'
    }
    static reset(): string {
        return Colors.resetCode
    }
    static level(level: number, text: string): string {
        return Colors.levelStart(level) + text + Colors.resetCode
    }
    static levelStart(level: number): string {
        switch (level) {
            case 0:
                return `\x1b[38;5;155m`
            case 1:
                return `\x1b[38;5;85m`
            case 2:
                return `\x1b[38;5;51m`
            default:
                return `\x1b[38;5;64m`
        }
    }
}

// ================
// === Consumer ===
// ================

export type LogLevel = 'trace' | 'log' | 'warn' | 'error'

export abstract class Consumer {
    abstract message(fn: LogLevel, ...args: any[]): void

    /** Start a group and log a message. */
    abstract group(...args: any[]): void

    /** Start a group and log a message. */
    abstract groupCollapsed(...args: any[]): void

    /** Log a message and end the last opened group. */
    abstract groupEnd(...args: any[]): void

    /** Log a message. */
    log(...args: any[]) {
        this.message('log', ...args)
    }

    /** Log a warning. */
    warn(...args: any[]) {
        this.message('warn', ...args)
    }

    /** Log an error. */
    error(...args: any[]) {
        this.message('error', ...args)
    }

    with<T>(message: string, f: () => T): T {
        this.group(message)
        const out = f()
        this.groupEnd()
        return out
    }

    withCollapsed<T>(message: string, f: () => T): T {
        this.groupCollapsed(message)
        const out = f()
        this.groupEnd()
        return out
    }

    async asyncWith<T>(message: string, f: () => Promise<T>): Promise<T> {
        this.group(message)
        const out = await f()
        this.groupEnd()
        return out
    }

    async asyncWithCollapsed<T>(message: string, f: () => Promise<T>): Promise<T> {
        this.groupCollapsed(message)
        const out = await f()
        this.groupEnd()
        return out
    }
}

// ==============
// === Logger ===
// ==============

export class Logger extends Consumer {
    private consumers: Consumer[] = []

    addConsumer(consumer: Consumer) {
        this.consumers.push(consumer)
    }

    message(fn: LogLevel, ...args: any[]) {
        for (const consumer of this.consumers) {
            consumer.message(fn, ...args)
        }
    }

    group(...args: any[]) {
        for (const consumer of this.consumers) {
            consumer.group(...args)
        }
    }

    groupCollapsed(...args: any[]) {
        for (const consumer of this.consumers) {
            consumer.groupCollapsed(...args)
        }
    }

    groupEnd(...args: any[]) {
        for (const consumer of this.consumers) {
            consumer.groupEnd(...args)
        }
    }
}

// ===============
// === Console ===
// ===============

function replacer(key: string, value: any) {
    if (value instanceof Map) {
        return {
            dataType: 'Map',
            value: Array.from(value.entries()),
        }
    } else {
        return value
    }
}

export class Console extends Consumer {
    private indent_lvl = 0

    message(fn: LogLevel, ...args: any[]) {
        const strArgs = args.map(arg => {
            if (isObject(arg)) {
                return JSON.stringify(arg, replacer, 2)
            } else {
                return arg.toString()
            }
        })
        const c: any = console
        if (host.browser) {
            c[fn](...strArgs)
        } else {
            let color: null | string
            switch (fn) {
                case 'warn':
                    color = 'orange'
                    break
                case 'error':
                    color = 'red'
                    break
                default:
                    color = null
                    break
            }
            //@ts-ignore
            const coloredArgs = color ? strArgs.map(arg => Colors[color](arg)) : strArgs
            if (this.indent_lvl > 0) {
                const indent = this.indent()
                const indentedArgs = coloredArgs.map(arg => arg.replaceAll('\n', `\n${indent}`))
                c.log(this.indent_shorter(), ...indentedArgs)
            } else {
                c.log(...strArgs)
            }
        }
    }

    group(...args: any[]) {
        if (host.browser) {
            console.group(...args)
        } else {
            const styleStart = `${Colors.bold_start()}${Colors.levelStart(this.indent_lvl)}`
            console.log(`${this.indent()}${styleStart}╭`, ...args, Colors.reset())
        }
        this.indent_lvl += 1
    }

    groupCollapsed(...args: any[]) {
        if (host.browser) {
            console.groupCollapsed(...args)
            this.indent_lvl += 1
        } else {
            this.group(...args)
        }
    }

    groupEnd(...args: any[]) {
        if (this.indent_lvl > 0) {
            this.indent_lvl -= 1
            if (host.browser) {
                if (args.length > 0) {
                    console.log(...args)
                }
                console.groupEnd()
            } else {
                const styleStart = `${Colors.levelStart(this.indent_lvl)}`
                console.log(`${this.indent()}${styleStart}╰`, ...args)
            }
        } else {
            this.log(...args)
        }
    }

    private indent(): string {
        let out = ''
        for (let i = 0; i < this.indent_lvl; i++) {
            const box = Colors.level(i, '│')
            out += `${box} `
        }
        return out
    }

    private indent_shorter(): string {
        return this.indent().slice(0, -1)
    }
}

// ===============================
// === Default Logger Instance ===
// ===============================

export const logger = new Logger()
logger.addConsumer(new Console())
