import Logger from './logger'

export default class Task {
    message: string
    startTime: number = 0
    endTime: number = 0
    constructor(message: string) {
        this.message = message
    }

    startx() {
        Logger.group(`${this.message}`)
        this.startTime = performance.now()
    }

    end() {
        this.endTime = performance.now()
        let ms = Math.round((this.endTime - this.startTime) * 100) / 100
        Logger.groupEnd(`Done in ${ms} ms.`)
    }

    static start(message: string): Task {
        const task = new Task(message)
        task.startx()
        return task
    }

    static with<T>(message: string, f: () => T): T {
        let task = Task.start(message)
        let out = f()
        task.end()
        return out
    }

    static async asyncWith<T>(message: string, f: () => Promise<T>): Promise<T> {
        let task = Task.start(message)
        let out = await f()
        task.end()
        return out
    }
}
