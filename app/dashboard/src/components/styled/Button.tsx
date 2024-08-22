/** @file A styled button. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

import { forwardRef } from '#/utilities/react'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// ==============
// === Button ===
// ==============

/** Props for a {@link Button}. */
export interface ButtonProps {
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: React.ReactNode
  readonly autoFocus?: boolean
  readonly mask?: boolean
  /** When `true`, the button uses a lighter color when it is not active. */
  readonly light?: boolean
  /** When `true`, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When `true`, the button is not clickable. */
  readonly isDisabled?: boolean
  readonly image: string
  readonly alt?: string
  readonly tooltipPlacement?: aria.Placement
  /** A title that is only shown when `disabled` is `true`. */
  readonly error?: string | null
  /** Class names for the icon itself. */
  readonly className?: string
  /** Extra class names for the `button` element wrapping the icon.
   * This is useful for things like positioning the entire button (e.g. `absolute`). */
  readonly buttonClassName?: string
  readonly onPress: (event: aria.PressEvent) => void
}

export default forwardRef(Button)

/** A styled button. */
function Button(props: ButtonProps, ref: React.ForwardedRef<HTMLButtonElement>) {
  const {
    tooltip,
    light = false,
    active = false,
    mask = true,
    image,
    error,
    alt,
    className,
    buttonClassName,
    tooltipPlacement,
    ...buttonProps
  } = props
  const { isDisabled = false } = buttonProps
  const focusChildProps = focusHooks.useFocusChild()

  const tooltipElement = tooltip === false ? null : tooltip ?? alt

  const Img = mask ? SvgMask : 'img'

  const button = (
    <FocusRing placement="after">
      <aria.Button
        {...aria.mergeProps<aria.ButtonProps & React.RefAttributes<HTMLButtonElement>>()(
          buttonProps,
          focusChildProps,
          {
            ref,
            className: tailwindMerge.twMerge(
              'relative after:pointer-events-none after:absolute after:inset after:rounded-button-focus-ring transition-colors hover:enabled:bg-primary/10 rounded-button-focus-ring -m-1 p-1',
              buttonClassName,
            ),
          },
        )}
      >
        <div
          className={tailwindMerge.twMerge(
            'group flex opacity-50 transition-all hover:opacity-75 disabled:cursor-not-allowed disabled:opacity-30 [&.disabled]:cursor-not-allowed [&.disabled]:opacity-30',
            light && 'opacity-25',
            isDisabled && 'disabled',
            active &&
              'opacity-100 hover:opacity-100 disabled:cursor-default disabled:opacity-100 [&.disabled]:cursor-default [&.disabled]:opacity-100',
          )}
        >
          <Img
            src={image}
            {...(!active && isDisabled && error != null ? { title: error } : {})}
            {...(alt != null ? { alt } : {})}
            className={className}
          />
        </div>
      </aria.Button>
    </FocusRing>
  )

  return tooltipElement == null ? button : (
      <ariaComponents.TooltipTrigger delay={0} closeDelay={0}>
        {button}
        <ariaComponents.Tooltip
          {...(tooltipPlacement != null ? { placement: tooltipPlacement } : {})}
        >
          {tooltipElement}
        </ariaComponents.Tooltip>
      </ariaComponents.TooltipTrigger>
    )
}
