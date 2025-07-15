/** @file Validation patterns for text inputs. */
import { doesTitleContainInvalidCharacters } from '../services/Backend'

// The Project Manager has restrictions on names of projects.
/**
 * Regex pattern for valid names for local projects.
 *
 * Validation rules:
 * - allow any non-empty string
 */
export const LOCAL_PROJECT_NAME_PATTERN = '.*\\S.*'

/**
 * Match only valid names for titles. The following substrings are disallowed:
 * - `/` - folder separator (non-Windows)
 * - `\` - folder separator (Windows)
 * - `..` - parent directory
 */
export const DIRECTORY_NAME_REGEX = /^(?:[^/\\.]|[.](?=[^.]|$))+$/

/**
 * Check if the directory name contains invalid characters.
 * @deprecated Use `doesTitleContainInvalidCharacters` instead.
 */
export function isDirectoryNameContainInvalidCharacters(name: string) {
  return doesTitleContainInvalidCharacters(name)
}

export { doesTitleContainInvalidCharacters } from '../services/Backend'
