/** @file An label that can be applied to an asset. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import type * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'

import * as backend from '#/services/Backend'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps extends Readonly<React.PropsWithChildren> {
  // This matches the capitalization of `data-` attributes in React.
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'data-testid'?: string
  /** When true, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When true, the button has a red border signifying that it will be deleted,
   * or that it is excluded from search. */
  readonly negated?: boolean
  /** When true, the button cannot be clicked. */
  readonly isDisabled?: boolean
  readonly draggable?: boolean
  readonly color: backend.LChColor
  readonly title?: string
  readonly className?: string
  readonly onPress: (event: aria.PressEvent | React.MouseEvent<HTMLButtonElement>) => void
  readonly onContextMenu?: (event: React.MouseEvent<HTMLElement>) => void
  readonly onDragStart?: (event: React.DragEvent<HTMLElement>) => void
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
  const { active = false, isDisabled = false, color, negated = false, draggable, title } = props
  const { className = 'text-tag-text', children, onPress, onDragStart, onContextMenu } = props
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)
  const textClass = /\btext-/.test(className)
    ? '' // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    : color.lightness <= 50
      ? 'text-tag-text'
      : 'text-primary'

  return (
    <FocusRing within placement="after">
      <div
        className={`relative rounded-full after:pointer-events-none after:absolute after:inset after:rounded-inherit ${negated ? 'after:!outline-offset-0' : ''}`}
      >
        {/* An `aria.Button` MUST NOT be used here, as it breaks dragging. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <button
          type="button"
          data-testid={props['data-testid']}
          draggable={draggable}
          title={title}
          disabled={isDisabled}
          className={`focus-child selectable ${
            active ? 'active' : ''
          } relative flex h-text items-center whitespace-nowrap rounded-inherit px-label-x transition-all after:pointer-events-none after:absolute after:inset after:rounded-full ${
            negated ? 'after:border-2 after:border-delete' : ''
          } ${className} ${textClass}`}
          style={{ backgroundColor: backend.lChColorToCssColor(color) }}
          onClick={event => {
            event.stopPropagation()
            onPress(event)
          }}
          onDragStart={e => {
            onDragStart?.(e)
          }}
          onContextMenu={onContextMenu}
          onKeyDown={handleFocusMove}
        >
          {children}
        </button>
      </div>
    </FocusRing>
  )
}
