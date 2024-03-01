/** @file A styled button. */
import * as React from 'react'

import SvgMask from '#/components/SvgMask'

/** Props for a {@link Button}. */
export interface ButtonProps {
  /** When `true`, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When `true`, the button is not clickable. */
  readonly disabled?: boolean
  readonly disabledOpacityClassName?: string
  readonly image: string
  readonly alt?: string
  /** A title that is only shown when `disabled` is `true`. */
  readonly error?: string | null
  readonly className?: string
  readonly onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
export default function Button(props: ButtonProps) {
  const { active = false, disabled = false, disabledOpacityClassName, image, alt, error } = props
  const { className, onClick } = props

  return (
    <button disabled={disabled} className="flex group">
      <SvgMask
        src={image}
        {...(!active && disabled && error != null ? { title: error } : {})}
        className={`${
          active
            ? ''
            : `group-disabled:cursor-not-allowed ${disabledOpacityClassName ?? 'opacity-disabled'}`
        } group-enabled:cursor-pointer group-enabled:hover:opacity-full transition-opacity ${
          className ?? ''
        }`}
        {...(alt != null ? { alt } : {})}
        {...(disabled ? {} : { onClick })}
      />
    </button>
  )
}
