import host from 'system/host'

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

export class Logger {
    private static indent_lvl: number = 0

    private static message(fn: string, color: string | null, ...args: any[]) {
        const strArgs = args.map(arg => arg.toString())
        const c: any = console
        if (host.browser) {
            c[fn](...strArgs)
        } else {
            //@ts-ignore
            const coloredArgs = color ? strArgs.map(arg => Colors[color](arg)) : strArgs
            if (Logger.indent_lvl > 0) {
                let indent = Logger.indent()
                const indentedArgs = coloredArgs.map(arg => arg.replaceAll('\n', `\n${indent}    `))
                c.log(Logger.indent_shorter(), ...indentedArgs)
            } else {
                c.log(...strArgs)
            }
        }
    }

    private static indent(): string {
        let out = ''
        for (let i = 0; i < Logger.indent_lvl; i++) {
            let box = Colors.level(i, '│')
            out += `${box} `
        }
        return out
    }

    private static indent_shorter(): string {
        return Logger.indent().slice(0, -1)
    }

    /** Log a message. */
    static log(...args: any[]) {
        Logger.message('log', null, ...args)
    }

    /** Log a warning. */
    static warn(...args: any[]) {
        Logger.message('warn', 'orange', ...args)
    }

    /** Log an error. */
    static error(...args: any[]) {
        Logger.message('error', 'red', ...args)
    }

    /** Start a group and log a message. */
    static group(...args: any[]) {
        if (host.browser) {
            console.group(...args)
        } else {
            const styleStart = `${Colors.bold_start()}${Colors.levelStart(Logger.indent_lvl)}`
            console.log(`${Logger.indent()}${styleStart}╭`, ...args, Colors.reset())
        }
        Logger.indent_lvl += 1
    }

    /** Start a group and log a message. */
    static groupCollapsed(...args: any[]) {
        if (host.browser) {
            console.groupCollapsed(...args)
            Logger.indent_lvl += 1
        } else {
            Logger.group(...args)
        }
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
                const styleStart = `${Colors.levelStart(Logger.indent_lvl)}`
                console.log(`${Logger.indent()}${styleStart}╰`, ...args)
            }
        } else {
            Logger.log(...args)
        }
    }

    static with<T>(message: string, f: () => T): T {
        Logger.group(message)
        let out = f()
        Logger.groupEnd()
        return out
    }

    static withCollapsed<T>(message: string, f: () => T): T {
        Logger.groupCollapsed(message)
        let out = f()
        Logger.groupEnd()
        return out
    }

    static async asyncWith<T>(message: string, f: () => Promise<T>): Promise<T> {
        Logger.group(message)
        let out = await f()
        Logger.groupEnd()
        return out
    }

    static async asyncWithCollapsed<T>(message: string, f: () => Promise<T>): Promise<T> {
        Logger.groupCollapsed(message)
        let out = await f()
        Logger.groupEnd()
        return out
    }
}
