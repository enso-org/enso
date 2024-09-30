/** @file An label that can be applied to an asset. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import type * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import FocusRing from '#/components/styled/FocusRing'

import * as backend from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps extends Readonly<React.PropsWithChildren> {
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
  const { className = 'text-tag-text', onPress, onDragStart, onContextMenu } = props
  const { children: childrenRaw } = props
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)
  const textClass =
    /\btext-/.test(className) ?
      '' // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    : color.lightness <= 50 ? 'text-tag-text'
    : 'text-primary'

  const children =
    typeof childrenRaw !== 'string' ? childrenRaw : (
      <ariaComponents.Text truncate="1" className="max-w-24" color="invert" variant="body">
        {childrenRaw}
      </ariaComponents.Text>
    )

  return (
    <FocusRing within placement="after">
      <div
        className={tailwindMerge.twMerge(
          'relative rounded-full after:pointer-events-none after:absolute after:inset after:rounded-inherit',
          negated && 'after:!outline-offset-0',
        )}
      >
        {/* An `aria.Button` MUST NOT be used here, as it breaks dragging. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <button
          type="button"
          data-testid={props['data-testid']}
          draggable={draggable}
          title={title}
          disabled={isDisabled}
          className={tailwindMerge.twMerge(
            'focus-child relative flex h-6 items-center whitespace-nowrap rounded-inherit px-[7px] opacity-75 transition-all after:pointer-events-none after:absolute after:inset after:rounded-full hover:opacity-100 focus:opacity-100',
            active && 'active',
            negated && 'after:border-2 after:border-delete',
            className,
            textClass,
          )}
          style={{ backgroundColor: backend.lChColorToCssColor(color) }}
          onClick={(event) => {
            event.stopPropagation()
            onPress(event)
          }}
          onDragStart={(e) => {
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
