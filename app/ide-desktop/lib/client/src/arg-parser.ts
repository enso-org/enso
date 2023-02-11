import chalk from 'chalk'
import buildCfg from '../../../build.json'
import * as config from './config'
const yargs = require('yargs')
import * as naming from './naming'
import stringLength from 'string-length'
import { hideBin } from 'yargs/helpers'

// ============
// === Help ===
// ============

const FULL_HELP_OPTION = 'full-help'

let usage = chalk.bold(
    `
Enso ${buildCfg.version} command line interface.
Usage: enso [options] [--] [backend args]`
)

class Section {
    entries: any[] = []
    description = ''
    constructor(entries: any[] = []) {
        this.entries = entries
    }
}

/** We use custom help printer because Yargs has many issues:
 * 1. The option ordering is random and there is no way to enforce it.
 * 2. The option groups ordering is random and there is no way to enforce it.
 * 3. Every option has a `[type`] annotation and there is no API to disable it.
 * 4. There is no option to print commands with single dash instead of double-dash.
 * 5. Help coloring is not supported, and they do not want to support it:
 *    https://github.com/yargs/yargs/issues/251
 */
function printHelp(cfg: {
    args: config.Args
    groupsOrdering: string[]
    secondaryGroups: string[]
    fullHelp: boolean
}) {
    console.log(usage)
    const terminalWidth = yargs.terminalWidth()
    const indentSize = 0
    const optionPrefix = '-'
    const spacing = 2
    const sections: { [key: string]: Section } = {}
    for (const groupName of cfg.groupsOrdering) {
        if (cfg.fullHelp || !cfg.secondaryGroups.includes(groupName)) {
            sections[groupName] = new Section()
        }
    }
    let maxOptionLength = 0

    for (const [groupName, group] of Object.entries(cfg.args.groups)) {
        let section = sections[groupName]
        if (section == null) {
            section = new Section()
            sections[groupName] = section
        }
        section.description = group.description
        for (const option of group.optionsRecursive()) {
            const cmdOption = naming.camelToKebabCase(option.qualifiedName())
            maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
            const entry = [cmdOption, option]
            section.entries.push(entry)
        }
    }

    for (const [optionName, option] of Object.entries(cfg.args.options)) {
        const cmdOption = naming.camelToKebabCase(optionName)
        maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
        const entry = [cmdOption, option]
        const section = sections[option.name]
        if (section != null) {
            section.entries.unshift(entry)
        }
        // sections['global'].entries.push(entry)
    }

    const borderStyle = (s: string) => chalk.gray(chalk.bold(s))

    const leftWidth = maxOptionLength + indentSize + stringLength(optionPrefix) + spacing
    const rightWidth = terminalWidth - leftWidth

    for (const [groupName, section] of Object.entries(sections)) {
        console.log('\n\n')
        const groupTitle = chalk.bold(`${naming.camelCaseToTitle(groupName)} Options `)
        console.log(groupTitle)
        const description = wordWrap(section.description, terminalWidth).join('\n')
        console.log(description)
        console.log()
        for (const [cmdOption, option] of section.entries) {
            if (cfg.fullHelp || option.primary) {
                const indent = ' '.repeat(indentSize)
                let left = indent + chalk.bold(chalk.green(optionPrefix + cmdOption))
                const spaces = ' '.repeat(leftWidth - stringLength(left))
                left = left + spaces

                let firstSentenceSplit = option.description.indexOf('. ')
                let firstSentence =
                    firstSentenceSplit == -1
                        ? option.description
                        : option.description.slice(0, firstSentenceSplit + 1)
                let otherSentences = option.description.slice(firstSentence.length)

                const def = option.defaultDescription ?? option.default
                let defaults = ''
                if (def != null && def !== '') {
                    defaults = ` Defaults to ${chalk.green(def)}.`
                }
                let description = firstSentence + defaults + chalk.gray(otherSentences)
                const lines = wordWrap(description, rightWidth).map(
                    line => line + ' '.repeat(rightWidth - stringLength(line))
                )
                const right = lines.join('\n' + ' '.repeat(leftWidth))
                console.log(left + right)
            }
        }
    }
}

function wordWrap(str: string, width: number): string[] {
    if (width <= 0) {
        return []
    }
    let line = ''
    const lines = []
    const inputLines = str.split('\n')
    for (const inputLine of inputLines) {
        for (let word of inputLine.split(' ')) {
            if (stringLength(word) > width) {
                if (line.length > 0) {
                    lines.push(line)
                    line = ''
                }
                const wordChunks = []
                while (stringLength(word) > width) {
                    wordChunks.push(word.slice(0, width))
                    word = word.slice(width)
                }
                wordChunks.push(word)
                for (const wordChunk of wordChunks) {
                    lines.push(wordChunk)
                }
            } else {
                if (stringLength(line) + stringLength(word) >= width) {
                    lines.push(line)
                    line = ''
                }
                if (line.length != 0) {
                    line += ' '
                }
                line += word
            }
        }
    }
    if (line) {
        lines.push(line)
    }
    return lines
}

// =====================
// === Option Parser ===
// =====================

export function parseArgs() {
    const args = config.my_args
    let argv = hideBin(process.argv)

    const yargOptions = args.optionsRecursive().reduce((opts: { [key: string]: any }, option) => {
        const yargsParam = Object.assign({}, option)
        // @ts-ignore
        yargsParam.requiresArg = ['string', 'array'].includes(yargsParam.type)
        // @ts-ignore
        yargsParam.default = undefined
        // @ts-ignore
        opts[naming.camelToKebabCase(option.qualifiedName())] = yargsParam
        return opts
    }, {})

    let optParser = yargs()
        .version(false)
        .parserConfiguration({
            'short-option-groups': false,
            'dot-notation': false,
            // Makes all flags passed after '--' be one string.
            'populate--': true,
        })
        .strict()
        .options(yargOptions)

    // === Parsing ===

    let xargs = optParser.parse(argv, {}, (err: any, argsDict: any, help: string) => {
        console.log('!!!', err, help)
        if (help) {
            printHelp({
                args,
                groupsOrdering: [],
                secondaryGroups: ['Electron Options'],
                fullHelp: argsDict[FULL_HELP_OPTION],
            })
            process.exit()
        }
    })

    for (const option of args.optionsRecursive()) {
        const arg = xargs[naming.camelToKebabCase(option.qualifiedName())]
        if (arg != null) {
            option.value = arg
            option.setByUser = true
        }
    }

    let windowSize = config.WindowSize.default()
    const parsedWindowSize = config.WindowSize.parse(args.groups.window.options.size.value)

    if (parsedWindowSize instanceof Error) {
        throw 'wrong window size'
    } else {
        windowSize = parsedWindowSize
    }

    if (args.options.help.value || args.options.fullHelp.value) {
        printHelp({
            args,
            groupsOrdering: [],
            secondaryGroups: ['Electron Options'],
            fullHelp: args.options.fullHelp.value,
        })
        process.exit()
    }

    const backendOptions = xargs['--']

    return { args, windowSize, backendOptions }
}
