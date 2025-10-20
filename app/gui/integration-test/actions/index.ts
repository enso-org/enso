/** @file Various actions, locators, and constants used in end-to-end tests. */
import { getText as baseGetText, type Replacements, type TextId } from 'enso-common/src/text'
// Also necessary as a hack to avoid circular import errors.
import { TEXT } from './utilities'
export * from './utilities'

export const getText = (key: TextId, ...replacements: Replacements[TextId]) => {
  return baseGetText(TEXT, key, ...replacements)
}
