/**
 * @file
 *
 * Reusable types for the Button component
 */
import type * as aria from '#/components/aria'
import type { ExtractFunction } from '#/utilities/tailwindVariants'
import type { ReactElement, ReactNode } from 'react'
import type { Addon, IconProp, TestIdProps } from '../types'
import type { BUTTON_STYLES, ButtonVariants } from './variants'

/**
 * Position of a joined button
 */
export type PrivateJoinedButtonPosition = ButtonVariants['position']

/**
 * Whether the button is joined
 */
export type PrivateJoinedButton = ButtonVariants['isJoined']

/**
 * Props for a joined button unlike other button props,
 */
export interface PrivateJoinedButtonProps {
  readonly position: PrivateJoinedButtonPosition
  readonly isJoined: NonNullable<PrivateJoinedButton>
}

/**
 * Render props for a button.
 */
export interface ButtonRenderProps extends aria.ButtonRenderProps {
  readonly isLoading: boolean
}

/**
 * Render props for a link.
 */
export interface LinkRenderProps extends aria.LinkRenderProps {
  readonly isLoading: boolean
}

/** Props for a Button. */
export type ButtonProps =
  | (BaseButtonProps<ButtonRenderProps> &
      Omit<aria.ButtonProps, 'children' | 'isPending' | 'onPress'> &
      PropsWithoutHref)
  | (BaseButtonProps<LinkRenderProps> &
      Omit<aria.LinkProps, 'children' | 'onPress'> &
      PropsWithHref)

/** Props for a button with an href. */
interface PropsWithHref {
  readonly href: string
}

/** Props for a button without an href. */
interface PropsWithoutHref {
  readonly href?: never
}

/** Base props for a button. */
export interface BaseButtonProps<Render>
  extends Omit<ButtonVariants, 'iconOnly' | 'isJoined' | 'position'>,
    TestIdProps {
  /** If `true`, the loader will not be shown. */
  readonly hideLoader?: boolean
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: ReactElement | string | false | null
  readonly tooltipPlacement?: aria.Placement
  /** The icon to display in the button */
  readonly icon?: IconProp<Render>
  /** When `true`, icon will be shown only when hovered. */
  readonly showIconOnHover?: boolean
  /**
   * Handler that is called when the press is released over the target.
   * If the handler returns a promise, the button will be in a loading state until the promise resolves.
   */
  readonly onPress?: ((event: aria.PressEvent) => Promise<void> | void) | null | undefined
  readonly contentClassName?: string
  readonly isDisabled?: boolean
  readonly formnovalidate?: boolean
  /**
   * Defaults to `full`. When `full`, the entire button will be replaced with the loader.
   * When `icon`, only the icon will be replaced with the loader.
   */
  readonly loaderPosition?: 'full' | 'icon'
  readonly styles?: ExtractFunction<typeof BUTTON_STYLES> | undefined

  readonly children?: ReactNode | ((render: Render) => ReactNode)

  readonly addonStart?: Addon<Render>
  readonly addonEnd?: Addon<Render>
}

/**
 * Props that are shared between buttons in a button group.
 */
export interface ButtonGroupSharedButtonProps extends ButtonVariants {
  readonly isDisabled?: boolean
  readonly isLoading?: boolean
  readonly loaderPosition?: 'full' | 'icon'
}
