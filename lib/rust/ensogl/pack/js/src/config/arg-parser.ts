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

class Option {
    value: string | boolean
    description: string
    type: string
    constructor(value: string | boolean, description: string) {
        this.value = value
        this.description = description
        if (value === true || value === false) {
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
    help = new Option(false, 'Print help message.')
    genShadersCode = new Option(
        false,
        'Generate a non-optimized shader code for static EnsoGL shape definitions.'
    )

    parse() {
        const optionToFieldNameMap: Map<string, string> = new Map()
        const options: any = {}
        for (const [fieldName, option] of Object.entries(this)) {
            const optionName = camelToKebabCase(fieldName)
            optionToFieldNameMap.set(optionName, fieldName)
            options[optionName] = { type: option.type, default: option.value }
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
            this.printHelpAndExit()
        }
    }

    printHelp() {
        console.log(`Options:`)
        for (const [fieldName, option] of Object.entries(this)) {
            const optionName = camelToKebabCase(fieldName)
            console.log()
            console.log(`--${optionName}`)
            console.log(option.description)
        }
    }

    printHelpAndExit() {
        this.printHelp()
        process.exit(0)
    }
}

export function parseArgs(): Args {
    const argParser = new Args()
    argParser.parse()
    return argParser
}
