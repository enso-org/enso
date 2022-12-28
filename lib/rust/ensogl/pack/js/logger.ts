import host from './host'

export default class Logger {
    private static indent_lvl: number = 0

    private static message(fn: string, ...args: any[]) {
        const c: any = console
        if (host.browser) {
            c[fn](...args)
        } else {
            if (Logger.indent_lvl > 0) {
                c[fn](Logger.indent_shorter(), ...args)
            } else {
                c[fn](...args)
            }
        }
    }

    private static indent(): string {
        return '│ '.repeat(Logger.indent_lvl)
    }

    private static indent_shorter(): string {
        return Logger.indent().slice(0, -1)
    }

    /** Log a message. */
    static log(...args: any[]) {
        Logger.message('log', ...args)
    }

    /** Log a warning. */
    static warn(...args: any[]) {
        Logger.message('warn', ...args)
    }

    /** Log an error. */
    static error(...args: any[]) {
        Logger.message('error', ...args)
    }

    /** Start a group and log a message. */
    static group(...args: any[]) {
        if (host.browser) {
            console.group(...args)
        } else {
            console.log(`${Logger.indent()}╭`, ...args)
        }
        Logger.indent_lvl += 1
    }

    /** Log a message and end the last opened group. */
    static groupEnd(...args: any[]) {
        if (Logger.indent_lvl > 0) {
            Logger.indent_lvl -= 1
            if (host.browser) {
                if (args.length > 0) {
                    console.log(...args)
                }
                console.groupEnd()
            } else {
                console.log(`${Logger.indent()}╰`, ...args)
            }
        } else {
            Logger.log(...args)
        }
    }
}
