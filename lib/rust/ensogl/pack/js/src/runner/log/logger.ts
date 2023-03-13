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
    static blackCode = '\x1b[30m'
    static redCode = '\x1b[31m'
    static greenCode = '\x1b[32m'
    static yellowCode = '\x1b[33m'
    static blueCode = '\x1b[34m'
    static magentaCode = '\x1b[35m'
    static cyanCode = '\x1b[36m'
    static whiteCode = '\x1b[37m'
    static black(text: string): string {
        return Colors.blackCode + text + Colors.resetCode
    }
    static red(text: string): string {
        return Colors.redCode + text + Colors.resetCode
    }
    static green(text: string): string {
        return Colors.greenCode + text + Colors.resetCode
    }
    static yellow(text: string): string {
        return Colors.yellowCode + text + Colors.resetCode
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
                return Colors.greenCode
            case 1:
                return Colors.cyanCode
            case 2:
                return Colors.blueCode
            default:
                return Colors.magentaCode
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
    abstract startGroup(...args: unknown[]): void

    /** Start a group and log a message. */
    abstract startGroupCollapsed(...args: unknown[]): void

    /** Log a message and end the last opened group. */
    abstract groupEnd(...args: unknown[]): void

    /** The width of the terminal in columns. In case there is no terminal, e.g. when the output
     * stream is redirected to a file, this results in a `undefined`. */
    terminalWidth(): number | undefined {
        return process.stdout.columns
    }

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
    group<T>(message: string, f: () => T): T {
        this.startGroup(message)
        const out = f()
        this.groupEnd()
        return out
    }

    /** Start a collapsed group, log a message, evaluate the provided function, and end the
     * group. */
    groupCollapsed<T>(message: string, f: () => T): T {
        this.startGroupCollapsed(message)
        const out = f()
        this.groupEnd()
        return out
    }

    /** Start a group, log a message, evaluate the provided async function, and end the group. */
    async asyncGroup<T>(message: string, f: () => Promise<T>): Promise<T> {
        this.startGroup(message)
        const out = await f()
        this.groupEnd()
        return out
    }

    /** Start a collapsed group, log a message, evaluate the provided async function, and end the
     * group. */
    async asyncGroupCollapsed<T>(message: string, f: () => Promise<T>): Promise<T> {
        this.startGroupCollapsed(message)
        const out = await f()
        this.groupEnd()
        return out
    }

    /** Start a group, log a message, evaluate the provided function, end the group, and log the
     * total operation time. */
    groupMeasured<T>(message: string, f: () => T): T {
        return Task.run(message, f)
    }

    /** Start a group, log a message, evaluate the provided async function, end the group, and log
     * the total operation time. */
    async asyncGroupMeasured<T>(message: string, f: () => Promise<T>): Promise<T> {
        return await Task.asyncRun(message, f)
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
    startGroup(...args: unknown[]) {
        for (const consumer of this.consumers) {
            consumer.startGroup(...args)
        }
    }

    /** Start a collapsed log group. */
    startGroupCollapsed(...args: unknown[]) {
        for (const consumer of this.consumers) {
            consumer.startGroupCollapsed(...args)
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
            let color: null | 'yellow' | 'red'
            switch (fn) {
                case 'warn':
                    color = 'yellow'
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
                c.log(...coloredArgs)
            }
        }
    }

    startGroup(...args: unknown[]) {
        if (host.browser) {
            console.group(...args)
        } else {
            const styleStart = `${Colors.boldStart()}${Colors.levelStart(this.indentLvl)}`
            console.log(`${this.indent()}${styleStart}╭`, ...args, Colors.reset())
        }
        this.indentLvl += 1
    }

    startGroupCollapsed(...args: unknown[]) {
        if (host.browser) {
            console.groupCollapsed(...args)
            this.indentLvl += 1
        } else {
            this.startGroup(...args)
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
                console.log(`${this.indent()}${styleStart}╰`, ...args, Colors.reset())
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

// ============
// === Task ===
// ============

/** A logging utility which groups subsequent operations in nicely formatted groups and logs their
 * evaluation time. */
export class Task {
    startTime = 0
    endTime = 0
    constructor(public message: string) {}

    private startBody() {
        this.startTime = performance.now()
    }

    private endBody(): [number, number] {
        this.endTime = performance.now()
        const ms = this.endTime - this.startTime
        let msRounded = Math.round(ms * 10) / 10
        if (msRounded == 0) {
            msRounded = Math.round(ms * 100) / 100
        }
        if (msRounded == 0) {
            msRounded = Math.round(ms * 1000) / 1000
        }
        return [ms, msRounded]
    }

    /** Start the task. You have to explicitly call the `end` method to finish this task. If
     * possible, use the `with` method instead. */
    start() {
        logger.startGroup(`${this.message}`)
        this.startBody()
    }

    /** Start the task and display subsequent logs in a collapsed group. You have to explicitly call
     * the `end` method to finish this task. If possible, use the `withCollapsed` method instead. */
    startCollapsed() {
        logger.startGroupCollapsed(`${this.message}`)
        this.startBody()
    }

    /** Start the task but do not group subsequent logs. You have to explicitly call the
     * `endNoGroup` method to finish this task. If possible, use the `withNoGroup` method
     * instead. */
    startNoGroup() {
        logger.log(`Started ${this.message}.`)
        this.startBody()
    }

    /** End the previously started task. If possible use the `with*` function family instead. */
    end(): number {
        const [ms, msRounded] = this.endBody()
        logger.groupEnd(`Done in ${msRounded} ms.`)
        return ms
    }

    /** End the previously started no-group task. If possible use the `with*` function family
     * instead. */
    endNoGroup(): number {
        const [ms, msRounded] = this.endBody()
        logger.log(`Finished ${this.message} in ${msRounded} ms.`)
        return ms
    }

    /** Start the task. You have to explicitly call the `end` method to finish this task. If
     * possible, use the `with` method instead. */
    static start(message: string): Task {
        const task = new Task(message)
        task.start()
        return task
    }

    /** Start the task and display subsequent logs in a collapsed group. You have to explicitly call
     * the `end` method to finish this task. If possible, use the `withCollapsed` method instead. */
    static startCollapsed(message: string): Task {
        const task = new Task(message)
        task.startCollapsed()
        return task
    }

    /** Start the task but do not group subsequent logs. You have to explicitly call the
     * `endNoGroup` method to finish this task. If possible, use the `withNoGroup` method
     * instead. */
    static startNoGroup(message: string): Task {
        const task = new Task(message)
        task.startNoGroup()
        return task
    }

    /** Start the task, evaluate the provided function, and end the task. */
    static run<T>(message: string, f: () => T): T {
        const task = Task.start(message)
        const out = f()
        task.end()
        return out
    }

    /** Start the task, hide all subsequent logs in a collapsed group, evaluate the provided
     * function, and end the task. */
    static runCollapsed<T>(message: string, f: () => T): T {
        const task = Task.startCollapsed(message)
        const out = f()
        task.end()
        return out
    }

    /** Start the task, evaluate the provided async function, and end the task. */
    static async asyncRun<T>(message: string, f: () => Promise<T>): Promise<T> {
        const task = Task.start(message)
        const out = await f()
        task.end()
        return out
    }

    /** Start the task, hide all subsequent logs in a collapsed group, evaluate the provided
     * async function, and end the task. */
    static async asyncRunCollapsed<T>(message: string, f: () => Promise<T>): Promise<T> {
        const task = Task.startCollapsed(message)
        const out = await f()
        task.end()
        return out
    }

    /** Start the task, evaluate the provided async function, and end the task. Do not group
     * subsequent logs. */
    static async asyncRunNoGroup<T>(message: string, f: () => Promise<T>): Promise<T> {
        const task = Task.startNoGroup(message)
        const out = await f()
        task.endNoGroup()
        return out
    }

    /** Start the task, evaluate the provided function, and end the task. Return the function result
     * together with the time information. */
    static runTimed<T>(message: string, f: () => T): [number, T] {
        const task = Task.start(message)
        const out = f()
        const ms = task.end()
        return [ms, out]
    }

    /** Start the task, hide all subsequent logs in a collapsed group, evaluate the provided
     * function, and end the task. Return the function result together with the time information. */
    static runCollapsedTimed<T>(message: string, f: () => T): [number, T] {
        const task = Task.startCollapsed(message)
        const out = f()
        const ms = task.end()
        return [ms, out]
    }

    /** Start the task, evaluate the provided async function, and end the task. Return the function
     * result together with the time information. */
    static async asyncRunTimed<T>(message: string, f: () => Promise<T>): Promise<[number, T]> {
        const task = Task.start(message)
        const out = await f()
        const ms = task.end()
        return [ms, out]
    }

    /** Start the task, hide all subsequent logs in a collapsed group, evaluate the provided async
     * function, and end the task. Return the function result together with the time information. */
    static async asyncRunCollapsedTimed<T>(
        message: string,
        f: () => Promise<T>
    ): Promise<[number, T]> {
        const task = Task.startCollapsed(message)
        const out = await f()
        const ms = task.end()
        return [ms, out]
    }
}

// ===============================
// === Default Logger Instance ===
// ===============================

export const logger = new Logger()
logger.addConsumer(new Console())
