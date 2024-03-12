/** @file A styled button. */
import * as React from 'react'

import SvgMask from '#/components/SvgMask'

/** Props for a {@link Button}. */
export interface ButtonProps {
  /** When true, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When true, the button is not clickable. */
  readonly disabled?: boolean
  readonly disabledOpacityClassName?: string
  readonly image: string
  readonly alt?: string
  /** A title that is only shown when `disabled` is true. */
  readonly error?: string | null
  /** The default title. */
  readonly title?: string
  readonly className?: string
  readonly onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
export default function Button(props: ButtonProps) {
  const { active = false, disabled = false, disabledOpacityClassName, image, alt, error } = props
  const { title, className, onClick } = props

  return (
    <SvgMask
      src={image}
      {...(!active && disabled && error != null
        ? { title: error }
        : title != null
          ? { title }
          : {})}
      className={`${active ? '' : disabledOpacityClassName ?? 'opacity-50'} ${
        disabled ? '' : 'cursor-pointer hover:opacity-100'
      } ${!active && disabled ? 'cursor-not-allowed' : ''} ${className ?? ''}`}
      {...(alt != null ? { alt } : {})}
      {...(disabled ? {} : { onClick })}
    />
  )
}
