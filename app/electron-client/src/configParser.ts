/** @file Command line options parser. */

import { Command, InvalidArgumentError } from 'commander'
import {
  defaultOptions,
  flattenObject,
  OptionsSchema,
  unflattenObject,
  type Options,
} from 'enso-common/src/options'

// CLI-only metadata defining flags and descriptions.
const OPTIONS_META: Readonly<Record<string, { flag: string; description: string }>> = {
  version: { flag: '-v, --version', description: 'Show version and exit' },
  headless: {
    flag: '--headless',
    description:
      'Run in headless mode (single execution). Requires `--startup.project <project path or url>`.',
  },
  displayWindow: {
    flag: '--no-window',
    description: 'Run server only, no GUI',
  },
  useServer: {
    flag: '--no-server',
    description: 'Do not start server; connect to external server at --server.port',
  },
  engineEnabled: { flag: '--no-engine', description: 'Do not start engine process' },
  useJvm: { flag: '--jvm', description: 'Run engine in JVM mode' },
  'startup.project': {
    flag: '--startup.project <name>',
    description: 'Project to open or create at startup',
  },
  'authentication.enabled': {
    flag: '--authentication.enabled',
    description: 'Enable user authentication (always true in cloud)',
  },
  'authentication.email': {
    flag: '--authentication.email <email>',
    description: 'User email for authentication',
  },
  'server.port': {
    flag: '--server.port',
    description: 'Port to use (fallbacks to next available if busy)',
  },
  'engine.ydocUrl': {
    flag: '--engine.ydocUrl <url>',
    description: 'Ydoc server URL',
  },
  'debug.info': {
    flag: '--debug.info',
    description: 'Print system debug info and exit',
  },
  'debug.verbose': {
    flag: '--debug.verbose',
    description: 'Increase backend log verbosity',
  },
  'debug.devTools': {
    flag: '--debug.devTools',
    description: 'Enable development mode',
  },
  'debug.profile': {
    flag: '--debug.profile',
    description: 'Start backend profiler and log to profiling.npss',
  },
  'debug.profileTime': {
    flag: '--debug.profileTime <seconds>',
    description: 'Duration (s) to collect backend profiling data',
  },
} as const

/** Parse command line arguments to validated options. */
export function parseArgs(argv: readonly string[]): Options {
  const command = new Command()

  command.usage('[flags]')

  const options = flattenObject(defaultOptions())
  const optionPathToAttrName = new Map<string, string>()
  for (const [key, meta] of Object.entries(OPTIONS_META)) {
    const def = options[key]
    if (def == null) {
      throw new Error(`Option ${key} provided in CLI, but not defined in the schema.`)
    }
    const opt = command.createOption(meta.flag, meta.description)
    if (typeof def === 'number') opt.argParser(parseNumber)
    opt.default(def)
    command.addOption(opt)
    optionPathToAttrName.set(key, opt.attributeName())
  }

  command.parse(argv, { from: 'user' })
  const raw = command.opts()

  for (const [key, attr] of optionPathToAttrName.entries()) {
    if (attr in raw) options[key] = raw[attr]
  }
  return OptionsSchema.parse(unflattenObject(options))
}

/** Parse an integer value from input string. */
function parseNumber(value: string, _: number): number {
  const parsed = parseInt(value)
  if (isNaN(parsed)) {
    throw new InvalidArgumentError('expected a number')
  }
  return parsed
}
