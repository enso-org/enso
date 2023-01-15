import { logger } from 'runner/log/logger'

// ============
// === Task ===
// ============

export class Task {
    message: string
    startTime = 0
    endTime = 0
    constructor(message: string) {
        this.message = message
    }

    start() {
        logger.group(`${this.message}`)
        this.startBody()
    }

    startCollapsed() {
        logger.groupCollapsed(`${this.message}`)
        this.startBody()
    }

    startNoGroup() {
        logger.log(`Started ${this.message}.`)
        this.startBody()
    }

    end(): number {
        const [ms, msRounded] = this.endBody()
        logger.groupEnd(`Done in ${msRounded} ms.`)
        return ms
    }

    endNoGroup(): number {
        const [ms, msRounded] = this.endBody()
        logger.log(`Finished ${this.message} in ${msRounded} ms.`)
        return ms
    }

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

    static start(message: string): Task {
        const task = new Task(message)
        task.start()
        return task
    }

    static startCollapsed(message: string): Task {
        const task = new Task(message)
        task.startCollapsed()
        return task
    }

    static startNoGroup(message: string): Task {
        const task = new Task(message)
        task.startNoGroup()
        return task
    }

    static with<T>(message: string, f: () => T): T {
        const task = Task.start(message)
        const out = f()
        task.end()
        return out
    }

    static withCollapsed<T>(message: string, f: () => T): T {
        const task = Task.startCollapsed(message)
        const out = f()
        task.end()
        return out
    }

    static async asyncWith<T>(message: string, f: () => Promise<T>): Promise<T> {
        const task = Task.start(message)
        const out = await f()
        task.end()
        return out
    }

    static async asyncWithCollapsed<T>(message: string, f: () => Promise<T>): Promise<T> {
        const task = Task.startCollapsed(message)
        const out = await f()
        task.end()
        return out
    }

    static async asyncNoGroupWith<T>(message: string, f: () => Promise<T>): Promise<T> {
        const task = Task.startNoGroup(message)
        const out = await f()
        task.endNoGroup()
        return out
    }

    static withTimed<T>(message: string, f: () => T): [number, T] {
        const task = Task.start(message)
        const out = f()
        const ms = task.end()
        return [ms, out]
    }

    static withCollapsedTimed<T>(message: string, f: () => T): [number, T] {
        const task = Task.startCollapsed(message)
        const out = f()
        const ms = task.end()
        return [ms, out]
    }

    static async asyncWithTimed<T>(message: string, f: () => Promise<T>): Promise<[number, T]> {
        const task = Task.start(message)
        const out = await f()
        const ms = task.end()
        return [ms, out]
    }
}
