/** @file Logger capable of displaying nicely formatted logs in the browser and in the console.
 * Also, it allows redirecting logs to different outputs, so you can plug in external sinks for the
 * logs. */

import host from 'runner/host'

// =============
// === Utils ===
// =============

/** Checks whether the provided value is an Object. It is used to determine whether the value should
 * be printed using `JSON.stringify`. */
function isObject(value: any): boolean {
    return typeof value === 'object' && !Array.isArray(value) && value !== null
}

// ==============
// === Colors ===
// ==============

/* eslint @typescript-eslint/no-extraneous-class: "off" */
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
    static boldStart(): string {
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

/** Consumer interface for `Logger`. It can be used to redirect logs to external sinks. */
export abstract class Consumer {
    abstract message(fn: LogLevel, ...args: unknown[]): void

    /** Start a group and log a message. */
    abstract group(...args: unknown[]): void

    /** Start a group and log a message. */
    abstract groupCollapsed(...args: unknown[]): void

    /** Log a message and end the last opened group. */
    abstract groupEnd(...args: unknown[]): void

    /** Log a message. */
    log(...args: unknown[]) {
        this.message('log', ...args)
    }

    /** Log a warning. */
    warn(...args: unknown[]) {
        this.message('warn', ...args)
    }

    /** Log an error. */
    error(...args: unknown[]) {
        this.message('error', ...args)
    }

    /** Log an internal error. */
    internalError(...args: unknown[]) {
        this.message('error', 'Internal error.', ...args)
    }

    /** Start a group, log a message, evaluate the provided function, and end the group. */
    with<T>(message: string, f: () => T): T {
        this.group(message)
        const out = f()
        this.groupEnd()
        return out
    }

    /** Start a collapsed group, log a message, evaluate the provided function, and end the
     * group. */
    withCollapsed<T>(message: string, f: () => T): T {
        this.groupCollapsed(message)
        const out = f()
        this.groupEnd()
        return out
    }

    /** Start a group, log a message, evaluate the provided async function, and end the group. */
    async asyncWith<T>(message: string, f: () => Promise<T>): Promise<T> {
        this.group(message)
        const out = await f()
        this.groupEnd()
        return out
    }

    /** Start a collapsed group, log a message, evaluate the provided async function, and end the
     * group. */
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

/** Logger capable of displaying nicely formatted logs in the browser and in the console. */
export class Logger extends Consumer {
    private consumers: Consumer[] = []

    /** Add a new consumer. All logs will be redirected to every attached consumer. */
    addConsumer(consumer: Consumer) {
        this.consumers.push(consumer)
    }

    /** Generic logging function. The first parameters is used to determine the log level. */
    message(fn: LogLevel, ...args: unknown[]) {
        for (const consumer of this.consumers) {
            consumer.message(fn, ...args)
        }
    }

    /** Start a log group. */
    group(...args: unknown[]) {
        for (const consumer of this.consumers) {
            consumer.group(...args)
        }
    }

    /** Start a collapsed log group. */
    groupCollapsed(...args: unknown[]) {
        for (const consumer of this.consumers) {
            consumer.groupCollapsed(...args)
        }
    }

    /** End the last log group. */
    groupEnd(...args: unknown[]) {
        for (const consumer of this.consumers) {
            consumer.groupEnd(...args)
        }
    }
}

// ===============
// === Console ===
// ===============

/** Pretty-printer for some object types for `JSON.stringify`. */
function replacer(key: string, value: unknown): unknown {
    if (value instanceof Map) {
        return {
            dataType: 'Map',
            value: Array.from(value.entries()),
        }
    } else {
        return value
    }
}

/** The console log consumer. If attached to `Logger`, it prints the incoming logs to the
 * console. */
export class Console extends Consumer {
    private indentLvl = 0

    message(fn: LogLevel, ...args: unknown[]) {
        const strArgs = args.map(arg => {
            if (isObject(arg)) {
                return JSON.stringify(arg, replacer, 2)
            } else {
                return String(arg)
            }
        })
        const c: globalThis.Console = console
        if (host.browser) {
            c[fn](...strArgs)
        } else {
            let color: null | 'orange' | 'red'
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
            /* eslint @typescript-eslint/no-unsafe-return: "off" */
            /* eslint @typescript-eslint/no-unsafe-call: "off" */
            /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
            // @ts-expect-error
            const coloredArgs: string[] = color ? strArgs.map(arg => Colors[color](arg)) : strArgs
            if (this.indentLvl > 0) {
                const indent = this.indent()
                const indentedArgs = coloredArgs.map(arg => arg.replaceAll('\n', `\n${indent}`))
                c.log(this.indentShorter(), ...indentedArgs)
            } else {
                c.log(...strArgs)
            }
        }
    }

    group(...args: unknown[]) {
        if (host.browser) {
            console.group(...args)
        } else {
            const styleStart = `${Colors.boldStart()}${Colors.levelStart(this.indentLvl)}`
            console.log(`${this.indent()}${styleStart}╭`, ...args, Colors.reset())
        }
        this.indentLvl += 1
    }

    groupCollapsed(...args: unknown[]) {
        if (host.browser) {
            console.groupCollapsed(...args)
            this.indentLvl += 1
        } else {
            this.group(...args)
        }
    }

    groupEnd(...args: unknown[]) {
        if (this.indentLvl > 0) {
            this.indentLvl -= 1
            if (host.browser) {
                if (args.length > 0) {
                    console.log(...args)
                }
                console.groupEnd()
            } else {
                const styleStart = `${Colors.levelStart(this.indentLvl)}`
                console.log(`${this.indent()}${styleStart}╰`, ...args)
            }
        } else {
            this.log(...args)
        }
    }

    private indent(): string {
        let out = ''
        for (let i = 0; i < this.indentLvl; i++) {
            const box = Colors.level(i, '│')
            out += `${box} `
        }
        return out
    }

    private indentShorter(): string {
        return this.indent().slice(0, -1)
    }
}

// ===============================
// === Default Logger Instance ===
// ===============================

export const logger = new Logger()
logger.addConsumer(new Console())
