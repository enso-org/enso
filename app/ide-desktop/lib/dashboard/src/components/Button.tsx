/** @file A styled button. */
import * as React from 'react'

import SvgMask from '#/components/SvgMask'

/** Props for a {@link Button}. */
export interface ButtonProps {
  /** When `true`, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When `true`, the button is not clickable. */
  readonly disabled?: boolean
  readonly image: string
  readonly alt?: string
  /** A title that is only shown when `disabled` is `true`. */
  readonly error?: string | null
  /** The default title. */
  readonly title?: string
  readonly className?: string
  readonly onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
export default function Button(props: ButtonProps) {
  const { active = false, disabled = false, image, error } = props
  const { title, alt, className, onClick } = props

  return (
    <button
      disabled={disabled}
      className={`group flex selectable ${active ? 'active' : ''}`}
      onClick={onClick}
    >
      <SvgMask
        src={image}
        {...(!active && disabled && error != null
          ? { title: error }
          : title != null
            ? { title }
            : {})}
        {...(alt != null ? { alt } : {})}
        className={className}
      />
    </button>
  )
}
