/** @file Command line options parser. */

import chalk from 'chalk'
import stringLength from 'string-length'

import yargs from 'yargs/yargs'
import yargsModule from 'yargs'

import * as contentConfig from 'enso-content-config'

import * as config from 'config'
import * as fileAssociations from 'file-associations'
import * as naming from 'naming'
import BUILD_INFO from '../../../../build.json' assert { type: 'json' }

const logger = contentConfig.logger

// =================
// === Constants ===
// =================

const DEFAULT_TERMINAL_WIDTH = 80
/** Returned by {@link String.indexOf} when the substring was not found. */
const NOT_FOUND = -1
const INDENT_SIZE = 0
const OPTION_PREFIX = '-'
const SPACING = 2

// ============
// === Help ===
// ============

const USAGE =
    chalk.bold(`\nEnso ${BUILD_INFO.version} command line interface.` + `Usage: enso [options]`) +
    `\n\nBoth single-dash and double-dash prefixes are accepted for all options. For ` +
    `instance, the help message can be displayed by entering either '--help' or '-help'. The ` +
    `'-no-' prefix may be utilized to disable a specific option. For example, to connect to ` +
    `the application from a web-browser, the creation of a window can be suppressed by ` +
    `entering either '-window=false' or '-no-window'.`

/** Contains information for a category of command line options and the options
 * it is comprised of. */
class Section<T> {
    description = ''
    entries: (readonly [cmdOption: string, option: config.Option<T>])[] = []
}

/** Configuration options controlling how the help information is displayed. */
interface PrintHelpConfig {
    args: config.Args
    groupsOrdering: string[]
    helpExtended: boolean
}

/** Command line help printer. The `groupsOrdering` parameter specifies the order in which the
 * option groups should be printed. Groups not specified will be printed in the definition order.
 *
 * We use a custom help printer because Yargs has several issues:
 * 1. The option ordering is random and there is no way to enforce it.
 * 2. The option groups ordering is random and there is no way to enforce it.
 * 3. Every option has a `[type`] annotation and there is no API to disable it.
 * 4. There is no option to print commands with single dash instead of double-dash.
 * 5. Help coloring is not supported, and they do not want to support it:
 * https://github.com/yargs/yargs/issues/251. */
function printHelp(cfg: PrintHelpConfig) {
    console.log(USAGE)
    const totalWidth = logger.terminalWidth() ?? DEFAULT_TERMINAL_WIDTH
    const sections: Record<string, Section<unknown>> = {}
    const topLevelSection = new Section()
    topLevelSection.description =
        'General application switches. For fine-grained control, see the available option groups.'
    sections.topLevel = topLevelSection
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
                section.entries.push([cmdOption, option])
            }
        }
    }

    for (const [optionName, option] of Object.entries(cfg.args.options)) {
        if (option.primary || cfg.helpExtended) {
            const cmdOption = naming.camelToKebabCase(optionName)
            maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
            const section = sections[option.name]
            if (section != null) {
                section.entries.unshift([cmdOption, option])
            } else {
                topLevelSection.entries.push([cmdOption, option])
            }
        }
    }

    const leftWidth = maxOptionLength + INDENT_SIZE + stringLength(OPTION_PREFIX) + SPACING
    const rightWidth = totalWidth - leftWidth

    for (const [groupName, section] of Object.entries(sections)) {
        if (section.entries.length > 0) {
            console.log('\n\n')
            const groupTitle = chalk.bold(`${naming.camelCaseToTitle(groupName)} Options `)
            console.log(groupTitle)
            const sectionDescription = wordWrap(section.description, totalWidth).join('\n')
            console.log(sectionDescription)
            console.log()
            section.entries.sort((a, b) => a[0].localeCompare(b[0]))
            for (const [cmdOption, option] of section.entries) {
                const indent = ' '.repeat(INDENT_SIZE)
                let left = indent + chalk.bold(chalk.green(OPTION_PREFIX + cmdOption))
                const spaces = ' '.repeat(leftWidth - stringLength(left))
                left = left + spaces

                const firstSentenceSplit = option.description.indexOf('. ')
                const firstSentence =
                    firstSentenceSplit === NOT_FOUND
                        ? option.description
                        : option.description.slice(0, firstSentenceSplit + 1)
                const otherSentences = option.description.slice(firstSentence.length)
                // We explicitly set the default for string options to be `null` in `parseArgs`.
                // eslint-disable-next-line no-restricted-syntax
                const def = option.defaultDescription ?? (option.default as string | null)
                const defIsEmptyArray = Array.isArray(def) && def.length === 0
                let defaults = ''
                if (def != null && def !== '' && !defIsEmptyArray) {
                    defaults = ` Defaults to ${chalk.green(def)}.`
                }
                const description = firstSentence + defaults + chalk.gray(otherSentences)
                const lines = wordWrap(description, rightWidth).map(
                    line => line + ' '.repeat(rightWidth - stringLength(line))
                )
                const right = lines.join('\n' + ' '.repeat(leftWidth))
                console.log(left + right)
            }
        }
    }
}

/** Wrap the text to a specific output width. If a word is longer than the output width, it will be
 * split. */
function wordWrap(str: string, width: number): string[] {
    if (width <= 0) {
        logger.error(`Cannot perform word wrap. The output width is set to '${width}'.`)
        return []
    } else {
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
            for (const originalWord of inputLine.split(' ')) {
                let word = originalWord
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
                    if (line.length !== 0) {
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
}

// ======================
// === Chrome Options ===
// ======================

/** Represents a command line option to be passed to the Chrome instance powering Electron. */
export class ChromeOption {
    /** Create a {@link ChromeOption}. */
    constructor(public name: string, public value?: string) {}

    /** Return the option as it would appear on the command line. */
    display(): string {
        const value = this.value == null ? '' : `=${this.value}`
        return `--${this.name}${value}`
    }
}

/** Replace `-no-...` with `--no-...`. This is a hotfix for a Yargs bug:
 * https://github.com/yargs/yargs-parser/issues/468. */
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

/** Command line options, split into regular arguments and Chrome options. */
interface ArgvAndChromeOptions {
    argv: string[]
    chromeOptions: ChromeOption[]
}

/** Parse the given list of arguments into two distinct sets: regular arguments and those specific
 * to Chrome. */
function argvAndChromeOptions(processArgs: string[]): ArgvAndChromeOptions {
    const chromeOptionRegex = /--?chrome.([^=]*)(?:=(.*))?/
    const argv = []
    const chromeOptions: ChromeOption[] = []
    for (let i = 0; i < processArgs.length; i++) {
        const processArg = processArgs[i]
        if (processArg != null) {
            const match = processArg.match(chromeOptionRegex)
            if (match?.[1] != null) {
                const optionName = match[1]
                const optionValue = match[2]
                if (optionValue != null) {
                    chromeOptions.push(new ChromeOption(optionName, optionValue))
                } else {
                    const nextArgValue = processArgs[i + 1]
                    if (nextArgValue != null && !nextArgValue.startsWith('-')) {
                        chromeOptions.push(new ChromeOption(optionName, nextArgValue))
                        i++
                    } else {
                        chromeOptions.push(new ChromeOption(optionName))
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

/** Parse command line arguments. */
export function parseArgs(clientArgs: string[] = fileAssociations.CLIENT_ARGUMENTS) {
    const args = config.CONFIG
    const { argv, chromeOptions } = argvAndChromeOptions(fixArgvNoPrefix(clientArgs))
    const yargsOptions = args
        .optionsRecursive()
        .reduce((opts: Record<string, yargsModule.Options>, option) => {
            opts[naming.camelToKebabCase(option.qualifiedName())] = {
                ...option,
                requiresArg: ['string', 'array'].includes(option.type),
                default: null,
                // Required because yargs defines `defaultDescription`
                // as `string | undefined`, not `string | null`.
                // eslint-disable-next-line no-restricted-syntax
                defaultDescription: option.defaultDescription ?? undefined,
            }
            return opts
        }, {})

    const optParser = yargs()
        .version(false)
        .parserConfiguration({
            // We don't control the naming of this third-party API.
            /* eslint-disable @typescript-eslint/naming-convention */
            // Allow single-dash arguments, like `-help`.
            'short-option-groups': false,
            // Treat dot-arguments as string keys, like `foo.bar`.
            'dot-notation': false,
            // Adds all flags passed after '--' to an array.
            'populate--': true,
            // Do not expand `--foo-bar` to `--fooBar`. This prevents an error when both the former
            // and later argument are reported as invalid at the same time.
            'camel-case-expansion': false,
            /* eslint-enable @typescript-eslint/naming-convention */
        })
        .help(false)
        .strict()
        .options(yargsOptions)

    // === Parsing ===

    /** Command line arguments after being parsed by `yargs`. */
    interface YargsArgs {
        // We don't control the naming of this third-party API.
        /* eslint-disable @typescript-eslint/naming-convention */
        [key: string]: string[] | string
        _: string[]
        // Exists only when the `populate--` option is enabled.
        '--'?: string[]
        $0: string
        /* eslint-enable @typescript-eslint/naming-convention */
    }

    // Required otherwise TypeScript thinks it's always `null`.
    // eslint-disable-next-line no-restricted-syntax
    let parseError = null as Error | null
    // The type assertion is required since `parse` may return a `Promise`
    // when an async middleware has been registered, but we are not doing that.
    // eslint-disable-next-line no-restricted-syntax
    const { '--': unexpectedArgs, ...parsedArgs } = optParser.parse(
        argv,
        {},
        // @ts-expect-error Yargs' typings are wrong.
        (err: Error | null) => {
            if (err != null) {
                parseError = err
            }
        }
    ) as YargsArgs

    for (const option of args.optionsRecursive()) {
        const arg = parsedArgs[naming.camelToKebabCase(option.qualifiedName())]
        const isArray = Array.isArray(arg)
        // Yargs parses missing array options as `[undefined]`.
        const isInvalidArray = isArray && arg.length === 1 && arg[0] == null
        if (arg != null && !isInvalidArray) {
            option.value = arg
            option.setByUser = true
        }
    }

    let windowSize = config.WindowSize.default()
    const providedWindowSize = args.groups.window.options.size.value
    const parsedWindowSize = config.WindowSize.parse(providedWindowSize)

    if (parsedWindowSize instanceof Error) {
        logger.error(`Wrong window size provided: '${providedWindowSize}'.`)
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
        const unexpectedArgsString = unexpectedArgs.map(arg => JSON.stringify(arg)).join(' ')
        logger.error(`Unexpected arguments found: '${unexpectedArgsString}'.`)
        printHelpAndExit(1)
    }

    return { args, windowSize, chromeOptions }
}
