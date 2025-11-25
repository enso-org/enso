import type { Identifier } from 'ydoc-shared/ast'

/** Transforms the given string into a valid function name. */
export function normalizeFunctionName(name: string): Identifier {
  return (toLowerSnakeCase(name) || 'user_created_component') as Identifier
}

/** Transforms the given string into a valid function name. */
export function normalizeArgumentName(name: string): Identifier {
  return (toLowerSnakeCase(name) || 'arg') as Identifier
}

function toLowerSnakeCase(name: string): string {
  if (!name) return ''
  let result = ''
  let lastWasUpper = true

  for (let i = 0; i < name.length; i++) {
    const c = name.charAt(i)
    if (/^[A-Z]$/.test(c)) {
      if (!lastWasUpper) result += '_'
      result += c.toLowerCase()
      lastWasUpper = true
    } else if (/^[a-z]$/.test(c)) {
      result += c
      lastWasUpper = false
    } else if (/^[0-9]$/.test(c)) {
      if (i == 0) result += '_'
      result += c
      lastWasUpper = false
    } else if (c == '_' || c == ' ') {
      result += '_'
      lastWasUpper = false
    }
  }
  // Replace multiple underscores with a single underscore and remove trailing underscores
  return result.replaceAll(/__+/g, '_').replace(/_$/, '')
}
