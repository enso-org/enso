/**
 * @file Button.tsx
 *
 * Button component
 */
import clsx from 'clsx'
import * as reactAriaComponents from 'react-aria-components'
import * as tailwindMerge from 'tailwind-merge'

import SvgMask from '#/components/SvgMask'

/**
 * Props for the Button component
 */
export interface ButtonProps extends reactAriaComponents.ButtonProps {
  readonly variant: 'icon'
  readonly icon?: string
  /**
   * FIXME: This is not yet implemented
   * The position of the icon in the button
   * @default 'start'
   */
  readonly iconPosition?: 'end' | 'start'
}

const DEFAULT_CLASSES =
  'flex cursor-pointer rounded-sm border border-transparent transition-opacity duration-200 ease-in-out'
const FOCUS_CLASSES =
  'focus-visible:outline-offset-2 focus:outline-none focus-visible:outline focus-visible:outline-primary'
const ICON_CLASSES = 'opacity-50 hover:opacity-100'
const EXTRA_CLICK_ZONE_CLASSES = 'flex relative before:inset-[-12px] before:absolute before:z-10'
const DISABLED_CLASSES = 'disabled:opacity-50 disabled:cursor-not-allowed'

/**
 * A button allows a user to perform an action, with mouse, touch, and keyboard interactions.
 */
export function Button(props: ButtonProps) {
  const { className, children, icon, ...ariaButtonProps } = props

  const classes = clsx(DEFAULT_CLASSES, DISABLED_CLASSES, FOCUS_CLASSES, ICON_CLASSES)

  const childrenFactory = () => {
    return icon != null ? (
      <>
        <div className={EXTRA_CLICK_ZONE_CLASSES}>
          <SvgMask src={icon} />
        </div>
      </>
    ) : (
      children
    )
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
