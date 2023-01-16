import * as util from 'node:util'

// ==========================
// === Naming Conversions ===
// ==========================

function camelToKebabCase(name: string) {
    return name
        .split('')
        .map((letter, idx) => {
            return letter.toUpperCase() === letter
                ? `${idx !== 0 ? '-' : ''}${letter.toLowerCase()}`
                : letter
        })
        .join('')
}

// ==============
// === Option ===
// ==============

class Option<T> {
    'default': T | undefined
    value: T | undefined
    description: string
    type: 'string' | 'boolean'
    constructor(description: string, def?: T) {
        this.default = def
        this.description = description
        if (def === true || def === false) {
            this.type = 'boolean'
        } else {
            this.type = 'string'
        }
    }
}

// ============
// === Args ===
// ============

interface ParseArgsOptionConfig {
    type: 'string' | 'boolean'
    multiple?: boolean | undefined
    short?: string | undefined
    default?: string | boolean | string[] | boolean[] | undefined
}

export class Args2 {
    [key: string]: Option<string | boolean>
    help = new Option('Print help message.', false)
    extractShaders = new Option<string>(
        'Extract non-optimized shaders code for static EnsoGL shape definitions. The argument is ' +
            'the directory the shaders will be written to.'
    )
}

export class Args {
    args = new Args2()

    parse() {
        const optionToFieldNameMap = new Map<string, string>()
        const options: Record<string, ParseArgsOptionConfig> = {}
        for (const [fieldName, option] of Object.entries(this.args)) {
            const optionName = camelToKebabCase(fieldName)
            optionToFieldNameMap.set(optionName, fieldName)
            options[optionName] = { type: option.type, default: option.default }
        }
        try {
            const out = util.parseArgs({ options })
            for (const [optionName, optionValue] of Object.entries(out.values)) {
                const fieldName = optionToFieldNameMap.get(optionName)
                if (fieldName) {
                    // @ts-expect-error
                    this.args[fieldName].value = optionValue
                } else {
                    console.error(`Unknown option: ${optionName}`)
                    process.exit(1)
                }
            }
        } catch (error) {
            const msg = error instanceof Error ? `${error.message}. ` : ''
            console.error(`${msg}Use --help to learn about possible options.`)
            process.exit(1)
        }
        if (this.args.help.value) {
            this.printHelpAndExit(0)
        }
    }

    printHelp() {
        console.log(`Options:`)
        for (const [fieldName, option] of Object.entries(this.args)) {
            const optionName = camelToKebabCase(fieldName)
            let header = `--${optionName}`
            if (option.type == 'string') {
                const def = option.default != null ? `[${option.default}]` : '<value>'
                header += `=${def}`
            }
            console.log()
            console.log(header)
            console.log(option.description)
        }
    }

    printHelpAndExit(exitCode: number) {
        this.printHelp()
        process.exit(exitCode)
    }
}

export function parseArgs(): Args {
    const argParser = new Args()
    argParser.parse()
    return argParser
}
