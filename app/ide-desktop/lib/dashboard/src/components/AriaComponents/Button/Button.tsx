/**
 * @file Button.tsx
 *
 * Button component
 */
import * as React from 'react'

import clsx from 'clsx'
import * as reactAriaComponents from 'react-aria-components'
import * as tailwindMerge from 'tailwind-merge'

import Spinner, * as spinnerModule from '#/components/Spinner'
import SvgMask from '#/components/SvgMask'

/**
 * Props for the Button component
 */
export interface ButtonProps extends reactAriaComponents.ButtonProps {
  readonly loading?: boolean
  readonly variant: 'cancel' | 'delete' | 'icon' | 'submit'
  readonly icon?: string
  /**
   * FIXME: This is not yet implemented
   * The position of the icon in the button
   * @default 'start'
   */
  readonly iconPosition?: 'end' | 'start'
}

const DEFAULT_CLASSES =
  'flex cursor-pointer rounded-full border border-transparent transition-opacity duration-200 ease-in-out'
const FOCUS_CLASSES =
  'focus-visible:outline-offset-2 focus:outline-none focus-visible:outline focus-visible:outline-primary'
const SUBMIT_CLASSES = 'bg-invite text-white opacity-80 hover:opacity-100'
const CANCEL_CLASSES = 'bg-selected-frame opacity-80 hover:opacity-100'
const DELETE_CLASSES = 'bg-delete text-white'
const ICON_CLASSES = 'opacity-50 hover:opacity-100'
const EXTRA_CLICK_ZONE_CLASSES = 'flex relative before:inset-[-12px] before:absolute before:z-10'
const DISABLED_CLASSES = 'disabled:opacity-50 disabled:cursor-not-allowed'
const SIZE_CLASSES = 'px-2 py-1'

/**
 * A button allows a user to perform an action, with mouse, touch, and keyboard interactions.
 */
export function Button(props: ButtonProps) {
  const { className, children, variant, icon, loading = false, ...ariaButtonProps } = props

  const classes = clsx(DEFAULT_CLASSES, DISABLED_CLASSES, FOCUS_CLASSES, SIZE_CLASSES, {
    [SUBMIT_CLASSES]: variant === 'submit',
    [CANCEL_CLASSES]: variant === 'cancel',
    [DELETE_CLASSES]: variant === 'delete',
    [ICON_CLASSES]: variant === 'icon',
  })

  const childrenFactory = (): React.ReactNode => {
    if (loading) {
      return <Spinner state={spinnerModule.SpinnerState.loadingMedium} size={16} />
    } else if (variant === 'icon' && icon != null) {
      return (
        <>
          <div className={EXTRA_CLICK_ZONE_CLASSES}>
            <SvgMask src={icon} />
          </div>
        </>
      )
    } else {
      return <>{children}</>
    }
  }

  return (
    <reactAriaComponents.Button
      className={values =>
        tailwindMerge.twMerge(
          classes,
          typeof className === 'function' ? className(values) : className
        )
      }
      {...ariaButtonProps}
    >
      {childrenFactory()}
    </reactAriaComponents.Button>
  )
}
