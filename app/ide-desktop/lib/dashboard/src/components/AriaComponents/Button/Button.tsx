/** @file A styled button. */
import * as React from 'react'

import clsx from 'clsx'
import * as tailwindMerge from 'tailwind-merge'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import Spinner, * as spinnerModule from '#/components/Spinner'
import SvgMask from '#/components/SvgMask'

// ==============
// === Button ===
// ==============

/** Props for a {@link Button}. */
export interface ButtonProps extends Readonly<aria.ButtonProps> {
  readonly loading?: boolean
  readonly variant: 'cancel' | 'delete' | 'icon' | 'submit'
  readonly icon?: string
  /** FIXME: This is not yet implemented
   * The position of the icon in the button
   * @default 'start' */
  readonly iconPosition?: 'end' | 'start'
}

const DEFAULT_CLASSES =
  'selectable flex cursor-pointer rounded-sm border border-transparent transition-[opacity,outline-offset] duration-200 ease-in-out'
const FOCUS_CLASSES =
  'focus-visible:outline-offset-2 focus:outline-none focus-visible:outline focus-visible:outline-primary'
const SUBMIT_CLASSES = 'px-2 py-1 bg-invite text-white opacity-80 hover:opacity-100'
const CANCEL_CLASSES = 'px-2 py-1 bg-selected-frame opacity-80 hover:opacity-100'
const DELETE_CLASSES = 'px-2 py-1 bg-delete text-white'
const ICON_CLASSES = ''
const EXTRA_CLICK_ZONE_CLASSES = 'flex relative before:inset-[-12px] before:absolute before:z-10'

const CLASSES_FOR_VARIANT: Record<ButtonProps['variant'], string> = {
  cancel: CANCEL_CLASSES,
  delete: DELETE_CLASSES,
  icon: ICON_CLASSES,
  submit: SUBMIT_CLASSES,
}

/** A button allows a user to perform an action, with mouse, touch, and keyboard interactions. */
export function Button(props: ButtonProps) {
  const { className, children, variant, icon, loading = false, ...ariaButtonProps } = props
  const focusChildProps = focusHooks.useFocusChild()

  const classes = clsx(DEFAULT_CLASSES, FOCUS_CLASSES, CLASSES_FOR_VARIANT[variant])

  const childrenFactory = (): React.ReactNode => {
    if (loading) {
      return <Spinner state={spinnerModule.SpinnerState.loadingMedium} size={16} />
    } else if (variant === 'icon' && icon != null) {
      return (
        <div className={EXTRA_CLICK_ZONE_CLASSES}>
          <SvgMask src={icon} />
        </div>
      )
    } else {
      return <>{children}</>
    }
  }

  return (
    <aria.Button
      {...aria.mergeProps<aria.ButtonProps>()(ariaButtonProps, focusChildProps, {
        className: values =>
          tailwindMerge.twMerge(
            classes,
            typeof className === 'function' ? className(values) : className
          ),
      })}
    >
      {childrenFactory()}
    </aria.Button>
  )
}
