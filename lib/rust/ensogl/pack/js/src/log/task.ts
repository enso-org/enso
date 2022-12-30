import { Logger } from 'log/logger'

export class Task {
    message: string
    startTime: number = 0
    endTime: number = 0
    constructor(message: string) {
        this.message = message
    }

    start() {
        Logger.group(`${this.message}`)
        this.startTime = performance.now()
    }

    end(): number {
        this.endTime = performance.now()
        const ms = this.endTime - this.startTime
        let ms_rounded = Math.round(ms * 10) / 10
        if (ms_rounded == 0) {
            ms_rounded = Math.round(ms * 100) / 100
        }
        if (ms_rounded == 0) {
            ms_rounded = Math.round(ms * 1000) / 1000
        }
        Logger.groupEnd(`Done in ${ms_rounded} ms.`)
        return ms
    }

    static start(message: string): Task {
        const task = new Task(message)
        task.start()
        return task
    }

    static with<T>(message: string, f: () => T): T {
        const task = Task.start(message)
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

    static withTimed<T>(message: string, f: () => T): [number, T] {
        const task = Task.start(message)
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
