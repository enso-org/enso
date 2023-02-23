/** @file Command line options parser. */

import chalk from 'chalk'
import buildCfg from '../../../../build.json'
import * as config from 'config'
import yargs from 'yargs/yargs'
import * as naming from 'naming'
import stringLength from 'string-length'
import { hideBin } from 'yargs/helpers'
import { logger } from 'enso-content-config'
import Electron from 'electron'

// ============
// === Help ===
// ============

let usage =
    chalk.bold(`\nEnso ${buildCfg.version} command line interface.` + `Usage: enso [options]`) +
    `\n\nBoth single-dash and double-dash prefixes are accepted for all options. For ` +
    `instance, the help message can be displayed by entering either '--help' or '-help'. The ` +
    `'-no-' prefix may be utilized to disable a specific option. For example, to connect to ` +
    `the application from a web-browser, the creation of a window can be suppressed by ` +
    `entering either '-window=false' or '-no-window'.`

class Section {
    entries: any[] = []
    description = ''
    constructor(entries: any[] = []) {
        this.entries = entries
    }
}

/** Command line help printer. The `groupsOrdering` parameter specifies the order in which the
 * option groups should be printed. Groups not specified will be printed in the definition order.
 *
 * We use custom help printer because Yargs has several issues:
 * 1. The option ordering is random and there is no way to enforce it.
 * 2. The option groups ordering is random and there is no way to enforce it.
 * 3. Every option has a `[type`] annotation and there is no API to disable it.
 * 4. There is no option to print commands with single dash instead of double-dash.
 * 5. Help coloring is not supported, and they do not want to support it:
 *    https://github.com/yargs/yargs/issues/251
 */
function printHelp(cfg: { args: config.Args; groupsOrdering: string[]; helpExtended: boolean }) {
    console.log(usage)
    const totalWidth = logger.terminalWidth() ?? 80
    const indentSize = 0
    const optionPrefix = '-'
    const spacing = 2
    const sections: { [key: string]: Section } = {}
    const topLevelSection = new Section()
    topLevelSection.description =
        'General application switches. For fine-grained control, see the available option groups.'
    sections['topLevel'] = topLevelSection
    for (const groupName of cfg.groupsOrdering) {
        sections[groupName] = new Section()
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
            if (option.primary || cfg.helpExtended) {
                const cmdOption = naming.camelToKebabCase(option.qualifiedName())
                maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
                const entry = [cmdOption, option]
                section.entries.push(entry)
            }
        }
    }

    for (const [optionName, option] of Object.entries(cfg.args.options)) {
        if (option.primary || cfg.helpExtended) {
            const cmdOption = naming.camelToKebabCase(optionName)
            maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
            const entry = [cmdOption, option]
            const section = sections[option.name]
            if (section != null) {
                section.entries.unshift(entry)
            } else {
                topLevelSection.entries.push(entry)
            }
        }
    }

    const leftWidth = maxOptionLength + indentSize + stringLength(optionPrefix) + spacing
    const rightWidth = totalWidth - leftWidth

    for (const [groupName, section] of Object.entries(sections)) {
        if (section.entries.length > 0) {
            console.log('\n\n')
            const groupTitle = chalk.bold(`${naming.camelCaseToTitle(groupName)} Options `)
            console.log(groupTitle)
            const description = wordWrap(section.description, totalWidth).join('\n')
            console.log(description)
            console.log()
            section.entries.sort()
            for (const [cmdOption, option] of section.entries) {
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
                const defIsEmptyArray = Array.isArray(def) && def.length === 0
                let defaults = ''
                if (def != null && def !== '' && !defIsEmptyArray) {
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

/** Wraps the text to a specific output width. If a word is longer than the output width, it will be
 * split. */
function wordWrap(str: string, width: number): string[] {
    if (width <= 0) {
        logger.error(`Cannot perform word wrap. The output width is set to '${width}'.`)
        return []
    }
    let firstLine = true
    let line = ''
    const lines = []
    const inputLines = str.split('\n')
    for (const inputLine of inputLines) {
        if (!firstLine) {
            lines.push(line)
            line = ''
        }
        firstLine = false
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

// ======================
// === Chrome Options ===
// ======================

export class ChromeOption {
    constructor(public name: string, public value?: string) {}

    display(): string {
        const value = this.value == null ? '' : `=${this.value}`
        return `--${this.name}${value}`
    }
}

/** Replaces `-no-...` with `--no-...`. This is a hotfix for Yargs bug:
 * https://github.com/yargs/yargs-parser/issues/468 */
function fixArgvNoPrefix(argv: string[]): string[] {
    const singleDashPrefix = '-no-'
    const doubleDashPrefix = '--no-'
    return argv.map(arg => {
        if (arg.startsWith(singleDashPrefix)) {
            return doubleDashPrefix + arg.slice(singleDashPrefix.length)
        } else {
            return arg
        }
    })
}

/** Parse the given list of arguments into two distinct sets: regular arguments and those specific
 * to Chrome. */
function argvAndChromeOptions(processArgs: string[]): {
    argv: string[]
    chromeOptions: ChromeOption[]
} {
    const chromeOptionRegex = /--?chrome.([^=]*)(=(.*))?/
    const argv = []
    const chromeOptions: ChromeOption[] = []
    for (let i = 0; i < processArgs.length; i++) {
        const processArg = processArgs[i]
        if (processArg != null) {
            const match = processArg.match(chromeOptionRegex)
            if (match != null) {
                const optionName = match[1] as string
                const optionValue = match[3]
                if (optionValue != null) {
                    chromeOptions.push(new ChromeOption(optionName, optionValue))
                } else {
                    const nextArgValue = processArgs[i + 1]
                    if (nextArgValue != null && !nextArgValue.startsWith('-')) {
                        chromeOptions.push(new ChromeOption(optionName, nextArgValue))
                        i++
                    } else {
                        chromeOptions.push(new ChromeOption(optionName, undefined))
                    }
                }
            } else {
                argv.push(processArg)
            }
        }
    }
    return { argv, chromeOptions }
}

// =====================
// === Option Parser ===
// =====================

export function parseArgs() {
    const args = config.config
    const { argv, chromeOptions } = argvAndChromeOptions(fixArgvNoPrefix(hideBin(process.argv)))

    const yargsOptions = args.optionsRecursive().reduce((opts: { [key: string]: any }, option) => {
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
            // Allow single-dash arguments, like `-help`.
            'short-option-groups': false,
            // Treat dot-arguments as string keys, like `foo.bar`.
            'dot-notation': false,
            // Makes all flags passed after '--' be one string.
            'populate--': true,
            // Do not expand `--foo-bar` to `--fooBar`. This prevents an error when both the former
            // and later argument are reported as invalid at the same time.
            'camel-case-expansion': false,
        })
        .help(false)
        .strict()
        .options(yargsOptions)

    // === Parsing ===

    let parseError = null as null | Error
    let parsedArgs = optParser.parse(argv, {}, (err: Error | undefined) => {
        if (err != null) {
            parseError = err
        }
    }) as { [key: string]: any }
    const unexpectedArgs = parsedArgs['--']

    for (const option of args.optionsRecursive()) {
        const arg = parsedArgs[naming.camelToKebabCase(option.qualifiedName())]
        const isArray = Array.isArray(arg)
        // Yargs parses missing array options as `[undefined]`.
        const isInvalidArray = isArray && arg.length == 1 && arg[0] == null
        if (arg != null && !isInvalidArray) {
            option.value = arg
            option.setByUser = true
        }
    }

    let windowSize = config.WindowSize.default()
    const providedWindowSize = args.groups.window.options.size.value
    const parsedWindowSize = config.WindowSize.parse(providedWindowSize)

    if (parsedWindowSize instanceof Error) {
        throw new Error(`Wrong window size provided: '${providedWindowSize}'.`)
    } else {
        windowSize = parsedWindowSize
    }

    const printHelpAndExit = (exitCode?: number) => {
        printHelp({
            args,
            groupsOrdering: [
                args.groups.loader.name,
                args.groups.startup.name,
                args.groups.style.name,
                args.groups.featurePreview.name,
                args.groups.window.name,
                args.groups.server.name,
                args.groups.engine.name,
                args.groups.performance.name,
                args.groups.debug.name,
                args.groups.profile.name,
                args.groups.authentication.name,
                args.groups.dataCollection.name,
                args.groups.chrome.name,
            ],
            helpExtended: args.options.helpExtended.value,
        })
        process.exit(exitCode)
    }

    const helpRequested = args.options.help.value || args.options.helpExtended.value
    if (helpRequested) {
        printHelpAndExit()
    } else if (parseError != null) {
        logger.error(parseError.message)
        printHelpAndExit(1)
    } else if (unexpectedArgs != null) {
        logger.error(`Unexpected arguments found: '${unexpectedArgs}'.`)
        printHelpAndExit(1)
    }

    return { args, windowSize, chromeOptions }
}
