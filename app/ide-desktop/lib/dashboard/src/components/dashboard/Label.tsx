/** @file An label that can be applied to an asset. */
import * as React from 'react'

import * as backend from '#/services/Backend'

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
  readonly focusRing?: boolean
  /** When true, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When true, the button has a red border signifying that it will be deleted,
   * or that it is excluded from search. */
  readonly negated?: boolean
  /** When true, the button cannot be clicked. */
  readonly disabled?: boolean
  readonly color: backend.LChColor
  readonly className?: string
}

/** An label that can be applied to an asset. */
function Label(props: InternalLabelProps, ref: React.ForwardedRef<HTMLButtonElement>) {
  const {
    'data-testid': dataTestId,
    focusRing = false,
    active = false,
    disabled = false,
    color,
    negated = false,
    className = 'text-tag-text',
    children,
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
    <div
      className={`relative after:absolute after:inset after:rounded-full ${focusRing ? 'after:focus-ring' : ''}`}
    >
      <button
        data-testid={dataTestId}
        ref={ref}
        disabled={disabled}
        className={`selectable ${
          active ? 'active' : ''
        } relative flex h-text items-center whitespace-nowrap rounded-full px-label-x transition-all before:absolute before:inset before:rounded-full ${
          negated ? 'before:border-2 before:border-delete' : ''
        } ${className} ${textColorClassName}`}
        style={{ backgroundColor: backend.lChColorToCssColor(color) }}
        {...passthrough}
      >
        {children}
      </button>
    </div>
  )
}

export default React.forwardRef(Label)
