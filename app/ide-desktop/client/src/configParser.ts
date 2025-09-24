/** @file Command line options parser. */

import yargs, { type Options } from 'yargs'

import * as config from '@/config'
import * as fileAssociations from '@/fileAssociations'
import * as naming from '@/naming'

// ======================
// === Chrome Options ===
// ======================

/** Represents a command line option to be passed to the Chrome instance powering Electron. */
export class ChromeOption {
  /** Create a {@link ChromeOption}. */
  constructor(
    public name: string,
    public value?: string,
  ) {}

  /** Return the option as it would appear on the command line. */
  display(): string {
    const value = this.value == null ? '' : `=${this.value}`
    return `--${this.name}${value}`
  }
}

/**
 * Replace `-no-...` with `--no-...`. This is a hotfix for a Yargs bug:
 * https://github.com/yargs/yargs-parser/issues/468.
 */
function fixArgvNoPrefix(argv: readonly string[]): readonly string[] {
  const singleDashPrefix = '-no-'
  const doubleDashPrefix = '--no-'
  return argv.map((arg) => {
    if (arg.startsWith(singleDashPrefix)) {
      return doubleDashPrefix + arg.slice(singleDashPrefix.length)
    } else {
      return arg
    }
  })
}

/** Command line options, split into regular arguments and Chrome options. */
interface ArgvAndChromeOptions {
  readonly argv: readonly string[]
  readonly chromeOptions: ChromeOption[]
}

/**
 * Parse the given list of arguments into two distinct sets: regular arguments and those specific
 * to Chrome.
 */
function argvAndChromeOptions(processArgs: readonly string[]): ArgvAndChromeOptions {
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
export function parseArgs(clientArgs: readonly string[] = fileAssociations.CLIENT_ARGUMENTS) {
  const args = config.CONFIG
  const { argv, chromeOptions } = argvAndChromeOptions(fixArgvNoPrefix(clientArgs))
  const yargsOptions = args.optionsRecursive().reduce((opts: Record<string, Options>, option) => {
    opts[naming.camelToKebabCase(option.qualifiedName())] = {
      ...option,
      requiresArg: ['string', 'array'].includes(option.type),
      default: null,
      // Required because yargs defines `defaultDescription`
      // as `string | undefined`, not `string | null`.
      defaultDescription: option.defaultDescription ?? undefined,
    }
    return opts
  }, {})

  const optParser = yargs()
    .version(false)
    .parserConfiguration({
      // Allow single-dash arguments, like `-help`.
      'short-option-groups': false,
      // Treat dot-arguments as string keys, like `foo.bar`.
      'dot-notation': false,
      // Do not expand `--foo-bar` to `--fooBar`. This prevents an error when both the former
      // and later argument are reported as invalid at the same time.
      'camel-case-expansion': false,
    })
    .strict()
    .wrap(yargs().terminalWidth())
    .options(yargsOptions)

  // === Parsing ===

  /** Command line arguments after being parsed by `yargs`. */
  interface YargsArgs {
    readonly [key: string]: string[] | string
    readonly _: string[]
    readonly $0: string
  }

  // The type assertion is required since `parse` may return a `Promise`
  // when an async middleware has been registered, but we are not doing that.
  const { ...parsedArgs } = optParser.parse(argv, {}, (_err, _argv, output) => {
    process.stdout.write(output)
  }) as YargsArgs

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
    console.error(`Wrong window size provided: '${providedWindowSize}'.`)
  } else {
    windowSize = parsedWindowSize
  }

  return { args, windowSize, chromeOptions }
}
