/**
 * @file Name validation utilities for Enso project names.
 *
 * This module copies implementation from NameValidation.scala module in the backend.
 */

import { Identifier, isIdentifier } from '@/util/qualifiedName'

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
 * Checks if a character is allowed in a project name.
 */
function isAllowedNameCharacter(char: string): boolean {
  return /[a-zA-Z0-9_]/.test(char)
}
