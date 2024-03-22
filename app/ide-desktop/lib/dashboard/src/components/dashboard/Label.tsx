/** @file An label that can be applied to an asset. */
import * as React from 'react'

import type * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import UnstyledButton from '#/components/styled/UnstyledButton'

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
  readonly className?: string
  readonly onPress: (event: aria.PressEvent) => void
  readonly onContextMenu?: (event: React.MouseEvent<HTMLElement>) => void
  readonly onDragStart?: (event: React.DragEvent<HTMLElement>) => void
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
  const { active = false, isDisabled = false, color, negated = false, draggable } = props
  const { className = 'text-tag-text', children, onPress, onDragStart, onContextMenu } = props
  const textColorClassName = /\btext-/.test(className)
    ? '' // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    : color.lightness <= 50
      ? 'text-tag-text'
      : 'text-primary'

  return (
    <FocusRing placement="after">
      <div
        draggable={draggable}
        className="relative after:pointer-events-none after:absolute after:inset after:rounded-full"
        onDragStart={onDragStart}
        onContextMenu={onContextMenu}
      >
        <UnstyledButton
          data-testid={props['data-testid']}
          isDisabled={isDisabled}
          className={`selectable ${
            active ? 'active' : ''
          } relative flex h-text items-center whitespace-nowrap rounded-full px-label-x transition-all before:absolute before:inset before:rounded-full ${
            negated ? 'before:border-2 before:border-delete' : ''
          } ${className} ${textColorClassName}`}
          style={{ backgroundColor: backend.lChColorToCssColor(color) }}
          onPress={onPress}
        >
          {children}
        </UnstyledButton>
      </div>
    </FocusRing>
  )
}
