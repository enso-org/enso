import type { WidgetInput } from '@/providers/widgetRegistry'

/**
 * A class which instances have type `T`. Accepts classes that have a private constructors, such as
 * `AstExtended` or `ArgumentPlaceholder`.
 * */
export type Class<T> = Function & { prototype: T }

/**
 * Check if an object is an instance of a class. This is a type-save curried version of `instanceof`
 * operator, which is convenient to use in widget definitions.
 *
 * When two arguments are provided, returns `true` if the second argument is an instance of the
 * class provided in the first argument.
 * ```
 * const bodyIsElement = isInstance(HTMLElement, document.body) // true
 * ```
 *
 * When only one argument is provided, returns a function that checks if its argument is an instance
 * of a particular class.
 * ```
 * const isHtmlElement = isInstance(HTMLElement) // one-argument predicate function
 * const bodyIsElement = isHtmlElement(document.body) // true
 * ```
 */
export function isInstance<T>(constructor: Class<T>): (obj: unknown) => obj is T
export function isInstance<T>(constructor: Class<T>, obj: unknown): obj is T
export function isInstance(
  constructor: Class<unknown>,
  ...maybeObj: [unknown] | []
): boolean | ((obj: unknown) => boolean) {
  if (maybeObj.length > 0) {
    return maybeObj[0] instanceof constructor
  } else {
    return (obj: unknown) => obj instanceof constructor
  }
}

/**
 * A predicate for widget inputs that will match any input type.
 */
export const anyWidgetInput = (input: WidgetInput): input is WidgetInput => true
