/** @file Common types for ARIA components. */
import type { Icon as PossibleIcon } from '@/util/iconMetadata/iconName'
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
 * Type for any icon
 */
export type IconProp<Icon extends string = string, Render = never> =
  | IconPropSvgUse<Render>
  | LegacyIconProp<Icon, Render>

/**
 * A type that represents the possible return values for a legacy icon.
 */
export type LegacyAvialableIconReturn<Icon extends string> =
  | LegacyIcon<Icon>
  | ReactElement
  | false
  | null
  | undefined

/**
 * A type that represents the possible return values for a legacy icon.
 */
export type AvailableIconReturn = ReactElement | SvgUseIcon | false | null | undefined

/**
 * Generic type for any icon
 * @deprecated Prefer defined keys over importing from `#/assets/*.svg
 */
export type LegacyIconProp<Icon extends string, Render> =
  | LegacyAvialableIconReturn<Icon>
  | ((render: Render) => LegacyAvialableIconReturn<Icon>)

/**
 * Generic type for imported from figma icons
 */
export type IconPropSvgUse<Render> = AvailableIconReturn | ((render: Render) => AvailableIconReturn)

/**
 * @deprecated
 */
export type LegacyIcon<T extends string> = Exclude<T, PossibleIcon> & {}

/**
 * Type for any icon imported from figma
 */
export type SvgUseIcon = PossibleIcon
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
