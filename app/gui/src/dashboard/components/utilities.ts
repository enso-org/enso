/** @file Constants for aria components. */

import { identity } from '#/utilities/functions'
import { unsafeKeyValuePair, unsafeKeys } from 'enso-common/src/utilities/data/object'

/** Possible values for the `rounded` variant. */
export function roundedVariants() {
  return unsafeKeys(makeRoundedStyles(''))
}

/** Make a set of rounded styles. UNSAFE when `Key` is not a string literal type. */
export function makeRoundedStyles<Key extends string>(
  key: Key,
  map: (value: string) => string = identity,
) {
  return {
    none: unsafeKeyValuePair(key, map('rounded-none')),
    small: unsafeKeyValuePair(key, map('rounded-sm')),
    medium: unsafeKeyValuePair(key, map('rounded-md')),
    large: unsafeKeyValuePair(key, map('rounded-lg')),
    xlarge: unsafeKeyValuePair(key, map('rounded-xl')),
    xxlarge: unsafeKeyValuePair(key, map('rounded-2xl')),
    xxxlarge: unsafeKeyValuePair(key, map('rounded-3xl')),
    full: unsafeKeyValuePair(key, map('rounded-full')),
  } as const
}
