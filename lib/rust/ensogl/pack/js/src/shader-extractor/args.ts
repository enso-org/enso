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
    type: string
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

export class Args {
    help = new Option('Print help message.', false)
    extractShaders = new Option<string>(
        'Extract non-optimized shaders code for static EnsoGL shape definitions. The argument is ' +
            'the directory the shaders will be written to.'
    )

    parse() {
        const optionToFieldNameMap: Map<string, string> = new Map()
        const options: any = {}
        for (const [fieldName, option] of Object.entries(this)) {
            const optionName = camelToKebabCase(fieldName)
            optionToFieldNameMap.set(optionName, fieldName)
            options[optionName] = { type: option.type, default: option.default }
        }
        try {
            const nodeUtil = require('node:util')
            const out = nodeUtil.parseArgs({ options })
            for (const [optionName, optionValue] of Object.entries(out.values)) {
                const fieldName = optionToFieldNameMap.get(optionName)
                const self: any = this
                if (fieldName) {
                    self[fieldName].value = optionValue
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
        if (this.help.value) {
            this.printHelpAndExit(0)
        }
    }

    printHelp() {
        console.log(`Options:`)
        for (const [fieldName, option] of Object.entries(this)) {
            const optionName = camelToKebabCase(fieldName)
            let header = `--${optionName}`
            if (option.type == 'string') {
                const def = option.default ? `[${option.default}]` : '<value>'
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
