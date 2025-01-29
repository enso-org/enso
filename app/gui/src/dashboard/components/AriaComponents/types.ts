/** @file Common types for ARIA components. */
import type { ReactElement } from 'react'
import type * as reactAria from 'react-aria'

/** Props for adding a test id to a component */
export interface TestIdProps {
  /** @deprecated Use `testId` instead. */
  readonly 'data-testid'?: string | undefined
  readonly testId?: string | undefined
}

/**
 * A type alias for the Placement type from react-aria.
 * This type represents the possible positions where an element can be placed relative to a reference element.
 */
export type Placement = reactAria.Placement

/**
 * Generic type for any icon
 */
export type IconProp<Render> =
  | ReactElement
  | string
  | false
  | ((render: Render) => ReactElement | string | false | null | undefined)
  | null
  | undefined

/**
 * Generic type for any addon
 */
export type Addon<Render> =
  | ReactElement
  | string
  | false
  | ((render: Render) => ReactElement | string | false | null | undefined)
  | null
  | undefined
