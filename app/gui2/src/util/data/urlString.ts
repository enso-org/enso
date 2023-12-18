/** @file A URL string. This includes URLs with custom protocols like `data:` URLs, not just `http:`
 * and `https:`. */

declare const urlStringBrand: unique symbol
/** A URL string. They */
export type URLString = string & { [urlStringBrand]: never }

export function isUrlString(value: string): value is URLString {
  return /^\w+:/.test(value)
}
