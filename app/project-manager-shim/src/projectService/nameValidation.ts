export type InvalidNameError =
  | { type: 'Empty' }
  | { type: 'ShouldStartWithCapitalLetter' }
  | { type: 'ContainsInvalidCharacters'; characters: Set<string> }

/**
 * Checks if a character is allowed in a project name.
 * @param char the char to validate
 * @returns true if it's allowed, false otherwise
 */
function isAllowedNameCharacter(char: string): boolean {
  return /^[a-zA-Z0-9_]$/.test(char)
}

/**
 * Transforms the given string into a valid package name (i.e. a CamelCased identifier).
 * @param name the original name.
 * @returns the transformed name conforming to the specification.
 */
export function normalizedName(name: string): string {
  let starting: string
  if (
    name.length === 0 ||
    name
      .replace(/_/g, '')
      .split('')
      .every((c) => !isAllowedNameCharacter(c))
  ) {
    starting = 'Project'
  } else if (!/^[a-zA-Z]/.test(name)) {
    starting = 'Project_' + name
  } else {
    starting = name
  }
  // Capitalize first letter
  const startingWithUppercase = starting.charAt(0).toUpperCase() + starting.slice(1)
  // Filter to only alphanumeric characters
  const onlyAlphanumeric = startingWithUppercase.split('').filter(isAllowedNameCharacter).join('')

  return onlyAlphanumeric
}

/**
 * Validate the project name.
 * @param name the project name to validate
 * @returns either a validation error or a project name if it's valid
 */
export function validateName(name: string): InvalidNameError | string {
  if (name.length === 0) {
    return { type: 'Empty' }
  }

  if (!/^[A-Z]/.test(name)) {
    return { type: 'ShouldStartWithCapitalLetter' }
  }

  const invalidChars = name.split('').filter((c) => !isAllowedNameCharacter(c))
  if (invalidChars.length > 0) {
    return {
      type: 'ContainsInvalidCharacters',
      characters: new Set(invalidChars),
    }
  }

  return name
}
