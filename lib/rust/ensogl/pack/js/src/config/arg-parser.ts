import * as nodeUtil from 'node:util'

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

export class Args {
    help = new Option(false, 'Print help message.')
    genShadersCode = new Option(
        false,
        'Generate a non-optimized shader code for static EnsoGL shape definitions.'
    )

    parse() {
        const optionToFieldNameMap: Map<string, string> = new Map()
        const options: any = {}
        for (let [fieldName, option] of Object.entries(this)) {
            let optionName = camelToKebabCase(fieldName)
            optionToFieldNameMap.set(optionName, fieldName)
            options[optionName] = { type: option.type, default: option.value }
        }
        try {
            let out = nodeUtil.parseArgs({ options })
            for (let [optionName, optionValue] of Object.entries(out.values)) {
                let fieldName = optionToFieldNameMap.get(optionName)
                // @ts-ignore
                this[fieldName].value = optionValue
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
