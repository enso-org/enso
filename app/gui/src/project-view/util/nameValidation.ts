/**
 * @file Name validation utilities for Enso project names.
 *
 * This module copies implementation from NameValidation.scala module in the backend.
 */

import { isIdentifier, type Identifier } from '@/util/qualifiedName'

/**
 * Transforms the given string into a valid package name.
 */
export function normalizeName(name: string): Identifier {
  const starting =
    (
      name.length === 0 ||
      name
        .split('')
        .filter((c) => c !== '_')
        .every((c) => !isAllowedNameCharacter(c))
    ) ?
      'Project'
    : !name[0]?.match(/[a-zA-Z]/) ? 'Project_' + name
    : name

  const startingWithUppercase = starting.charAt(0).toUpperCase() + starting.slice(1)
  const onlyAlphanumeric = startingWithUppercase.split('').filter(isAllowedNameCharacter).join('')
  if (!isIdentifier(onlyAlphanumeric)) {
    throw new Error(`Project name normalization failed: ${name}`)
  }

  return onlyAlphanumeric
}

/**
 * Transforms the given string into a valid function name.
 */
export function normalizeFunctionName(name: string): Identifier {
  return (toLowerSnakeCase(name) || 'user_created_component') as Identifier
}

/**
 * Transforms the given string into a valid function name.
 */
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

/**
 * Checks if a character is allowed in a project name.
 */
function isAllowedNameCharacter(char: string): boolean {
  return /[a-zA-Z0-9_]/.test(char)
}
