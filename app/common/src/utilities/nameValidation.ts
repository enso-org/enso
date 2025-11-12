/**
 * @file Name validation utilities for Enso project names.
 *
 * This module copies implementation from NameValidation.scala module in the backend.
 */

/** Transform the given string into a valid package name. */
export function normalizeName(name: string): string {
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
  return startingWithUppercase.split('').filter(isAllowedNameCharacter).join('')
}

/** Checks if a character is allowed in a project name. */
function isAllowedNameCharacter(char: string): boolean {
  return /[a-zA-Z0-9_]/.test(char)
}
