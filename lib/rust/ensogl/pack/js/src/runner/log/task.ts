/** @file A logging utility which groups subsequent operations in nicely formatted groups and logs
 * their evaluation time. */

import { logger } from 'runner/log/logger'

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
        logger.group(`${this.message}`)
        this.startBody()
    }

    /** Start the task and display subsequent logs in a collapsed group. You have to explicitly call
     * the `end` method to finish this task. If possible, use the `withCollapsed` method instead. */
    startCollapsed() {
        logger.groupCollapsed(`${this.message}`)
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
