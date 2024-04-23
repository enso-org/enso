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
export interface ButtonProps extends Readonly<aria.ButtonProps> {
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
export type Size = 'custom' | 'hero' | 'large' | 'medium' | 'small' | 'xsmall'

/**
 * The position of the icon in the button
 */
export type IconPosition = 'end' | 'start'
/**
 * The variant of the button
 */
export type Variant = 'cancel' | 'custom' | 'delete' | 'icon' | 'outline' | 'primary' | 'submit'

const DEFAULT_CLASSES =
  'flex whitespace-nowrap cursor-pointer border border-transparent transition-[opacity,outline-offset] duration-200 ease-in-out select-none text-center items-center justify-center'
const FOCUS_CLASSES =
  'focus-visible:outline-offset-2 focus:outline-none focus-visible:outline focus-visible:outline-primary'
const EXTRA_CLICK_ZONE_CLASSES = 'flex relative before:inset-[-12px] before:absolute before:z-10'
const DISABLED_CLASSES = 'disabled:opacity-50 disabled:cursor-not-allowed'
const LOADING_CLASSES = 'cursor-wait'
const FULL_WIDTH_CLASSES = 'w-full'

const CLASSES_FOR_SIZE: Record<Size, string> = {
  hero: 'px-8 py-4 text-lg',
  large: 'px-6 py-3 text-base',
  medium: 'px-4 py-2 text-sm',
  small: 'px-3 py-1 text-xs',
  xsmall: 'px-2 py-1 text-xs',
  custom: '',
}

const CLASSES_FOR_VARIANT: Record<Variant, string> = {
  custom: '',
  primary: 'bg-primary text-white hover:bg-primary-90',
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
export function Button(props: ButtonProps) {
  const {
    className,
    children,
    variant,
    icon,
    loading = false,
    isDisabled = loading,
    type = 'button',
    iconPosition = 'start',
    size = 'xsmall',
    fullWidth = false,
    rounding = 'large',
    tooltip,
    ...ariaButtonProps
  } = props
  const focusChildProps = focusHooks.useFocusChild()

  const tooltipElement = tooltip === false ? null : tooltip ?? ariaButtonProps['aria-label']

  const classes = clsx(
    DEFAULT_CLASSES,
    DISABLED_CLASSES,
    FOCUS_CLASSES,
    CLASSES_FOR_VARIANT[variant],
    CLASSES_FOR_SIZE[size],
    CLASSES_FOR_ROUNDING[rounding],
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
    <aria.Button
      {...aria.mergeProps<aria.ButtonProps>()(ariaButtonProps, focusChildProps, {
        type,
        isDisabled,
        className: values =>
          tailwindMerge.twMerge(
            classes,
            typeof className === 'function' ? className(values) : className
          ),
      })}
    >
      <aria.Text className="relative block">
        <aria.Text className={clsx('block', { invisible: loading })}>{childrenFactory()}</aria.Text>

        {loading && (
          <aria.Text className="absolute inset-0 flex items-center justify-center">
            <Spinner state={spinnerModule.SpinnerState.loadingMedium} size={16} />
          </aria.Text>
        )}
      </aria.Text>
    </aria.Button>
  )

  return tooltipElement == null ? (
    button
  ) : (
    <ariaComponents.TooltipTrigger>
      {button}
      <ariaComponents.Tooltip>{tooltipElement}</ariaComponents.Tooltip>
    </ariaComponents.TooltipTrigger>
  )
}
