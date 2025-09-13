/** @file Types for a `Button`. */
import type {
  ButtonProps as AriaButtonProps,
  ButtonRenderProps as AriaButtonRenderProps,
  LinkRenderProps as AriaLinkRenderProps,
  LinkProps,
  Placement,
  PressEvent,
} from '#/components/aria'
import type { ExtractFunction } from '#/utilities/tailwindVariants'
import type { ReactElement, ReactNode } from 'react'
import type { Addon, IconProp, TestIdProps } from '../types'
import type { BUTTON_STYLES, ButtonVariants } from './variants'

/** Position of a joined button. */
export type PrivateJoinedButtonPosition = ButtonVariants['position']

/** Whether the button is joined. */
export type PrivateJoinedButton = ButtonVariants['isJoined']

/** Props for a joined button unlike other button props. */
export interface PrivateJoinedButtonProps {
  readonly position: PrivateJoinedButtonPosition
  readonly isJoined: NonNullable<PrivateJoinedButton>
}

/** Render props for a button. */
export interface ButtonRenderProps extends AriaButtonRenderProps {
  readonly isLoading: boolean
}

/** Render props for a link. */
export interface LinkRenderProps extends AriaLinkRenderProps {
  readonly isLoading: boolean
}

/** Props for a Button. */
export type ButtonProps<IconType extends string = string> =
  | (BaseButtonProps<IconType, ButtonRenderProps> &
      Omit<AriaButtonProps, 'children' | 'isPending' | 'onPress'> &
      PropsWithoutHref)
  | (BaseButtonProps<IconType, LinkRenderProps> &
      Omit<LinkProps, 'children' | 'onPress'> &
      PropsWithHref)

/** Props for a button with an href. */
export interface PropsWithHref {
  readonly href: string
}

/** Props for a button without an href. */
export interface PropsWithoutHref {
  readonly href?: never
}

/** Base props for a button. */
export interface BaseButtonProps<IconType extends string, Render>
  extends Omit<ButtonVariants, 'iconOnly' | 'isJoined' | 'loading' | 'position'>,
    TestIdProps {
  /** If `true`, the loader will not be shown. */
  readonly hideLoader?: boolean
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: ReactElement | string | false | null
  readonly tooltipPlacement?: Placement
  /** The icon to display in the button */
  readonly icon?: IconProp<IconType, Render>
  /** When `true`, icon will be shown only when hovered. */
  readonly showIconOnHover?: boolean
  /**
   * Handler that is called when the press is released over the target.
   * If the handler returns a promise, the button will be in a loading state until the promise resolves.
   */
  // Prettier is not able to format this line correctly
  // prettier-ignore
  readonly onPress?:
    // eslint-disable-next-line @typescript-eslint/no-redundant-type-constituents
    | ((event: PressEvent) => Promise<unknown> | unknown)
    | null
    | undefined
  readonly contentClassName?: string | undefined
  readonly isDisabled?: boolean | undefined
  readonly formnovalidate?: boolean | undefined
  readonly isLoading?: boolean | undefined
  /**
   * @deprecated Use `isLoading` instead.
   */
  readonly loading?: boolean
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

/** A new type `undefined` added to all properties of a type. */
type WithUndefined<T> = {
  [K in keyof T]: T[K] | undefined
}

/** Props that are shared between buttons in a button group. */
export interface ButtonGroupSharedButtonProps extends WithUndefined<ButtonVariants> {
  readonly isDisabled?: boolean | undefined
  readonly isLoading?: boolean | undefined
  readonly loaderPosition?: 'full' | 'icon' | undefined
}
