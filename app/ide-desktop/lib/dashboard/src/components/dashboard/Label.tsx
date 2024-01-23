/** @file An label that can be applied to an asset. */
import * as React from 'react'

import * as backend from '#/services/backend'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps
  extends React.PropsWithChildren,
    Omit<JSX.IntrinsicElements['button'], 'color' | 'onClick'>,
    Required<Pick<JSX.IntrinsicElements['button'], 'onClick'>> {
  /** When true, the button is not faded out even when not hovered. */
  active?: boolean
  /** When true, the button has a red border signifying that it will be deleted,
   * or that it is excluded from search. */
  negated?: boolean
  /** When true, the button cannot be clicked. */
  disabled?: boolean
  color: backend.LChColor
  /** When true, will turn opaque when the nearest ancestor `.group` is hovered.
   * Otherwise, will turn opaque only when itself is hovered. */
  group?: boolean
  className?: string
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
  const {
    active = false,
    disabled = false,
    color,
    negated = false,
    className = 'text-tag-text',
    children,
    group = true,
    ...passthrough
  } = props
  const textColorClassName = /\btext-/.test(className)
    ? '' // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    : color.lightness <= 50
    ? 'text-tag-text'
    : active
    ? 'text-primary'
    : 'text-not-selected'
  return (
    <button
      disabled={disabled}
      className={`flex items-center rounded-full whitespace-nowrap gap-1.5 h-6 px-2.25 transition-all ${className} ${
        negated
          ? 'relative before:absolute before:rounded-full before:border-2 before:border-delete before:inset-0 before:w-full before:h-full'
          : ''
      } ${active ? '' : 'opacity-50'} ${
        disabled ? '' : group ? 'group-hover:opacity-100' : 'hover:opacity-100'
      } ${textColorClassName}`}
      style={{ backgroundColor: backend.lChColorToCssColor(color) }}
      {...passthrough}
    >
      {children}
    </button>
  )
}
