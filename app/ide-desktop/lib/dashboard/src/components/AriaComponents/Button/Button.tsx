/** @file A styled button. */
import * as React from 'react'

import clsx from 'clsx'
import * as tailwindMerge from 'tailwind-merge'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Spinner, * as spinnerModule from '#/components/Spinner'
import SvgMask from '#/components/SvgMask'

// ==============
// === Button ===
// ==============

/** Props for a {@link Button}. */
export type ButtonProps =
  | (aria.ButtonProps & BaseButtonProps & PropsWithoutHref)
  | (aria.LinkProps & BaseButtonProps & PropsWithHref)

/**
 * Props for a button with an href.
 */
interface PropsWithHref {
  readonly href: string
}

/**
 * Props for a button without an href.
 */
interface PropsWithoutHref {
  readonly href?: never
}

/**
 * Base props for a button.
 */
export interface BaseButtonProps {
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: React.ReactNode
  readonly loading?: boolean
  /**
   * The variant of the button
   */
  readonly variant: Variant
  /**
   * The icon to display in the button
   */
  readonly icon?: string
  /**
   * The size of the button
   */
  readonly size?: Size
  readonly rounding?: Roundings
  /**
   * Button takes the full width of its container
   */
  readonly fullWidth?: boolean
  /**
   * The position of the icon in the button
   * @default 'start'
   */
  readonly iconPosition?: IconPosition
}

/**
 *
 */
export type Roundings = 'full' | 'large' | 'medium' | 'none' | 'small' | 'xlarge'

/**
 * The size of the button
 */
export type Size = 'custom' | 'hero' | 'large' | 'medium' | 'small' | 'xsmall' | 'xxsmall'

/**
 * The position of the icon in the button
 */
export type IconPosition = 'end' | 'start'
/**
 * The variant of the button
 */
export type Variant =
  | 'cancel'
  | 'custom'
  | 'delete'
  | 'icon'
  | 'link'
  | 'outline'
  | 'primary'
  | 'submit'
  | 'tertiary'

const DEFAULT_CLASSES =
  'flex whitespace-nowrap cursor-pointer border border-transparent transition-[opacity,outline-offset] duration-150 ease-in-out select-none text-center items-center justify-center'
const FOCUS_CLASSES =
  'focus-visible:outline-offset-2 focus:outline-none focus-visible:outline focus-visible:outline-primary'
const EXTRA_CLICK_ZONE_CLASSES = 'flex relative after:inset-[-12px] after:absolute'
const DISABLED_CLASSES = 'disabled:opacity-50 disabled:cursor-not-allowed'
const LOADING_CLASSES = 'cursor-wait'
const FULL_WIDTH_CLASSES = 'w-full'

const CLASSES_FOR_SIZE: Record<Size, string> = {
  hero: 'px-8 py-4 text-lg',
  large: 'px-6 py-3 text-base',
  medium: 'px-4 py-2 text-sm',
  small: 'px-3 py-1 text-xs',
  xsmall: 'px-2 py-1 text-xs',
  xxsmall: 'px-1.5 py-0.5 text-xs',
  custom: '',
}

const CLASSES_FOR_VARIANT: Record<Variant, string> = {
  custom: '',
  link: 'inline-flex px-0 py-0 rounded-sm text-primary hover:text-primary-90 hover:underline',
  primary: 'bg-primary text-white hover:bg-primary-90',
  tertiary: 'bg-share text-white hover:bg-share-90',
  cancel: 'bg-selected-frame opacity-80 hover:opacity-100',
  delete: 'bg-delete text-white',
  icon: 'opacity-50 hover:opacity-100',
  submit: 'bg-invite text-white opacity-80 hover:opacity-100',
  outline: 'border-primary text-primary font-bold hover:border-primary-90',
}

const CLASSES_FOR_ROUNDING: Record<Roundings, string> = {
  full: 'rounded-full',
  large: 'rounded-lg',
  medium: 'rounded-md',
  none: 'rounded-none',
  small: 'rounded-sm',
  xlarge: 'rounded-xl',
}

const ICON_POSITION: Record<IconPosition, string> = {
  start: '',
  end: 'flex-row-reverse',
}

/** A button allows a user to perform an action, with mouse, touch, and keyboard interactions. */
export const Button = React.forwardRef(function Button(
  props: ButtonProps,
  ref: React.ForwardedRef<HTMLButtonElement>
) {
  const {
    className,
    children,
    variant,
    icon,
    loading = false,
    isDisabled = loading,
    iconPosition = 'start',
    size = 'xsmall',
    fullWidth = false,
    rounding = 'large',
    tooltip,
    ...ariaProps
  } = props
  const focusChildProps = focusHooks.useFocusChild()

  const isLink = ariaProps.href != null

  const Tag = isLink ? aria.Link : aria.Button

  const goodDefaults = isLink ? { rel: 'noopener noreferrer' } : { type: 'button' }

  const tooltipElement = tooltip === false ? null : tooltip ?? ariaButtonProps['aria-label']

  const classes = clsx(
    DEFAULT_CLASSES,
    DISABLED_CLASSES,
    FOCUS_CLASSES,
    CLASSES_FOR_SIZE[size],
    CLASSES_FOR_ROUNDING[rounding],
    CLASSES_FOR_VARIANT[variant],
    { [LOADING_CLASSES]: loading, [FULL_WIDTH_CLASSES]: fullWidth }
  )

  const childrenFactory = (): React.ReactNode => {
    // Icon only button
    if (variant === 'icon' && icon != null) {
      return (
        <aria.Text className={EXTRA_CLICK_ZONE_CLASSES}>
          <SvgMask src={icon} className="flex-none" />
        </aria.Text>
      )
    } else {
      // Default button
      return (
        <aria.Text className={clsx('flex items-center gap-2', ICON_POSITION[iconPosition])}>
          {icon != null && <SvgMask src={icon} className="flex-none" />}
          <>{children}</>
        </aria.Text>
      )
    }
  }

  const button = (
    <Tag
      // @ts-expect-error eventhough typescript is complaining about the type of ariaProps, it is actually correct
      {...aria.mergeProps()(goodDefaults, ariaProps, focusChildProps, {
        ref,
        isDisabled,
      })}
      // @ts-expect-error eventhough typescript is complaining about the type of className, it is actually correct
      className={states =>
        tailwindMerge.twMerge(
          classes,
          // this is safe, because the type of states has correct types outside
          // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
          typeof className === 'function' ? className(states) : className
        )
      }
    >
      <aria.Text className="relative block">
        <aria.Text className={clsx('block', { invisible: loading })}>{childrenFactory()}</aria.Text>

        {loading && (
          <aria.Text className="absolute inset-0 flex items-center justify-center">
            <Spinner state={spinnerModule.SpinnerState.loadingMedium} size={16} />
          </aria.Text>
        )}
      </aria.Text>
    </Tag>
  )

  return tooltipElement == null ? (
    button
  ) : (
    <ariaComponents.TooltipTrigger>
      {button}
      <ariaComponents.Tooltip>{tooltipElement}</ariaComponents.Tooltip>
    </ariaComponents.TooltipTrigger>
  )
})
