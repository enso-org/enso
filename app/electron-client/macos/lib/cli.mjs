export function parseArgs(argv, options = {}) {
  const { boolean = [], string = [], defaults = {} } = options

  const booleanKeys = new Set(boolean)
  const stringKeys = new Set(string)
  const parsed = { ...defaults }

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index]

    if (arg === '--help' || arg === '-h') {
      parsed.help = true
      continue
    }

    if (!arg.startsWith('--')) {
      throw new Error(`Unexpected argument: ${arg}`)
    }

    if (arg.startsWith('--no-')) {
      const key = arg.slice('--no-'.length)
      if (!booleanKeys.has(key)) {
        throw new Error(`Unknown boolean argument: --no-${key}`)
      }
      parsed[key] = false
      continue
    }

    const eqIndex = arg.indexOf('=')
    if (eqIndex >= 0) {
      const key = arg.slice(2, eqIndex)
      const value = arg.slice(eqIndex + 1)
      if (booleanKeys.has(key)) {
        if (value === 'true' || value === '1') {
          parsed[key] = true
        } else if (value === 'false' || value === '0') {
          parsed[key] = false
        } else {
          throw new Error(`Invalid boolean value for --${key}: ${value}`)
        }
      } else if (stringKeys.has(key)) {
        parsed[key] = value
      } else {
        throw new Error(`Unknown argument: --${key}`)
      }
      continue
    }

    const key = arg.slice(2)
    if (booleanKeys.has(key)) {
      parsed[key] = true
      continue
    }

    if (stringKeys.has(key)) {
      const value = argv[index + 1]
      if (value == null || value.startsWith('--')) {
        throw new Error(`Missing value for argument: --${key}`)
      }
      parsed[key] = value
      index += 1
      continue
    }

    throw new Error(`Unknown argument: --${key}`)
  }

  return parsed
}
