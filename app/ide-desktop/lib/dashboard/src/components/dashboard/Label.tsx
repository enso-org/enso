/** @file An label that can be applied to an asset. */
import * as React from 'react'

import * as colorModule from '#/utilities/color'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps
  extends Readonly<React.PropsWithChildren>,
    Readonly<Omit<JSX.IntrinsicElements['button'], 'color' | 'onClick'>>,
    Readonly<Required<Pick<JSX.IntrinsicElements['button'], 'onClick'>>> {
  // This matches the capitalization of `data-` attributes in React.
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'data-testid'?: string
  /** When true, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When true, the button has a red border signifying that it will be deleted,
   * or that it is excluded from search. */
  readonly negated?: boolean
  /** When true, the button cannot be clicked. */
  readonly disabled?: boolean
  readonly color: colorModule.LChColor
  /** When true, will turn opaque when the nearest ancestor `.group` is hovered.
   * Otherwise, will turn opaque only when itself is hovered. */
  readonly group?: boolean
  readonly className?: string
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
  const {
    'data-testid': dataTestId,
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
      data-testid={dataTestId}
      disabled={disabled}
      className={`flex items-center rounded-full whitespace-nowrap gap-1.5 h-6 px-2.25 transition-all ${className} ${
        negated
          ? 'relative before:absolute before:rounded-full before:border-2 before:border-delete before:inset-0 before:w-full before:h-full'
          : ''
      } ${active ? '' : 'opacity-50'} ${
        disabled ? '' : group ? 'group-hover:opacity-100' : 'hover:opacity-100'
      } ${textColorClassName}`}
      style={{ backgroundColor: colorModule.lChColorToCssColor(color) }}
      {...passthrough}
    >
      {children}
    </button>
  )
}
