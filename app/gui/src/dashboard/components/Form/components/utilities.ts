/**
 * @file
 *
 * Helpers to work with forms
 */

import { omit } from 'enso-common/src/utilities/data/object'

const NON_DOM_PROPS = ['isInvalid', 'isRequired', 'isDisabled', 'readOnly', 'invalid'] as const

/**
 * Filter out DOM props from the given object.
 */
export function filterNonDOMFormProps<T extends object>(props: T) {
  // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, no-restricted-syntax, @typescript-eslint/no-explicit-any
  return omit(props, ...(NON_DOM_PROPS as any))
}
