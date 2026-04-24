/** @file Various actions, locators, and constants used in end-to-end tests. */
import { getText as originalGetText, type Replacements, type TextId } from 'enso-common/src/text'
// Also necessary as a hack to avoid circular import errors.
import { TEXT } from './utilities'
export * from './utilities'

export const getText = (key: TextId, ...replacements: Replacements[TextId]) => {
  return originalGetText(TEXT, key, ...replacements)
}
