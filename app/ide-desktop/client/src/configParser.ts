/** @file Command line options parser. */

import { Command, InvalidArgumentError } from 'commander'
import {
  OPTIONS,
  buildWebAppURLSearchParamsFromArgs as _buildWebAppURLSearchParamsFromArgs,
  collectWebAppOptionsFromArgs as _collectWebAppOptionsFromArgs,
  iterateOptions,
  type AnyOption,
  type ArgsFromOptions,
  type OptionsTree,
} from 'enso-common/src/options'

// =====================
// === Types & Utils ===
// =====================

export type ParsedArgs = ArgsFromOptions<typeof OPTIONS>

function joinPath(path: string[]) {
  return path.join('.')
}

/** Parse command line arguments. */
export function parseArgs(argv: readonly string[]): ParsedArgs {
  const command = new Command()

  function parseNumber(value: string, _: number): number {
    const parsed = parseInt(value)
    if (isNaN(parsed)) {
      throw new InvalidArgumentError('expected a number')
    }
    return parsed
  }

  // Map from logical option path (e.g. "window.size") to Commander attribute name used in opts().
  const optionPathToAttrName = new Map<string, string>()

  // Register Commander options using the iterator
  iterateOptions(OPTIONS, (path: string[], node: AnyOption) => {
    const opt = command.createOption(node.flag, node.description)
    if (node.type === 'number') opt.argParser(parseNumber)
    opt.default(node.defaultValue)
    command.addOption(opt)
    optionPathToAttrName.set(joinPath(path), opt.attributeName())
  })

  command.parse(argv, { from: 'user' })
  const raw = command.opts<Record<string, unknown>>()

  function buildArgs<T extends OptionsTree>(node: T, path: string[] = []): ArgsFromOptions<T> {
    if ('type' in (node as any)) {
      const attrName = optionPathToAttrName.get(joinPath(path))
      const value = attrName ? raw[attrName] : undefined
      switch ((node as any).type) {
        case 'boolean':
          return (typeof value === 'boolean' ? value : (
            (node as any).defaultValue
          )) as unknown as ArgsFromOptions<T>
        case 'number':
          return (typeof value === 'number' ? value : (
            (node as any).defaultValue
          )) as unknown as ArgsFromOptions<T>
        case 'string':
          return (typeof value === 'string' ? value : (
            (node as any).defaultValue
          )) as unknown as ArgsFromOptions<T>
      }
    }
    const out: Record<string, unknown> = {}
    const group = node as unknown as Record<string, OptionsTree>
    for (const [key, child] of Object.entries(group) as [string, OptionsTree][]) {
      out[key] = buildArgs(child, [...path, key])
    }
    return out as ArgsFromOptions<T>
  }

  return buildArgs(OPTIONS)
}

// ==============================
// === Web Options (URL sync) ===
// ==============================

// Compute union of dotted paths to leaf options that are passToWebApplication: true
type WebOptionPaths<T, Prefix extends string = ''> =
  T extends AnyOption<infer P> ?
    P extends true ?
      Prefix
    : never
  : {
      [K in keyof T & string]: WebOptionPaths<T[K], Prefix extends '' ? `${K}` : `${Prefix}.${K}`>
    }[keyof T & string]

// Value at dotted path P in object T
type PathValue<T, P extends string> =
  P extends `${infer K}.${infer Rest}` ?
    K extends keyof T ?
      PathValue<T[K], Rest>
    : never
  : P extends keyof T ? T[P]
  : never

// All passToWebApplication option keys and their values (based on original ArgsFromOptions types)
export type WebOptionKey = WebOptionPaths<typeof OPTIONS>
export type WebOptionsRecord = {
  [K in WebOptionKey]: PathValue<ArgsFromOptions<typeof OPTIONS>, K>
}

/** Collect non-default values of passToWebApplication options from parsed args. */
export function collectWebAppOptions(args: ParsedArgs): Partial<WebOptionsRecord> {
  return _collectWebAppOptionsFromArgs(args as any, OPTIONS)
}

/** Build URLSearchParams for non-default passToWebApplication options. */
export function buildWebAppURLSearchParams(args: ParsedArgs): URLSearchParams {
  return _buildWebAppURLSearchParamsFromArgs(args as any, OPTIONS)
}
