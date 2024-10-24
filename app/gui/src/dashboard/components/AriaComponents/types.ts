/** @file Common types for ARIA components. */

/** Props for adding a test id to a component */
export interface TestIdProps {
  /** @deprecated Use `testId` instead. */
  readonly 'data-testid'?: string | undefined
  readonly testId?: string | undefined
}
