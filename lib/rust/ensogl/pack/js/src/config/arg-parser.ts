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
        console.log('PARSE!!!')
        const optionToFieldNameMap: Map<string, string> = new Map()
        const options: any = {}
        for (let [fieldName, option] of Object.entries(this)) {
            let optionName = camelToKebabCase(fieldName)
            optionToFieldNameMap.set(optionName, fieldName)
            options[optionName] = { type: option.type, default: option.value }
        }
        try {
            const nodeUtil = require('node:util')
            let out = nodeUtil.parseArgs({ options })
            for (let [optionName, optionValue] of Object.entries(out.values)) {
                let fieldName = optionToFieldNameMap.get(optionName)
                let self: any = this
                if (fieldName) {
                    self[fieldName].value = optionValue
                } else {
                    console.error(`Unknown option: ${optionName}`)
                    process.exit(1)
                }
            }
        } catch (error) {
            let msg = error instanceof Error ? `${error.message}. ` : ''
            console.error(`${msg}Use --help to learn about possible options.`)
            process.exit(1)
        }
        if (this.help.value) {
            this.printHelpAndExit()
        }
    }

    printHelp() {
        console.log(`Options:`)
        for (let [fieldName, option] of Object.entries(this)) {
            let optionName = camelToKebabCase(fieldName)
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
