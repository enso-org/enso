/** @file An label that can be applied to an asset. */
import type { DragEvent, MouseEvent, PropsWithChildren } from 'react'

import type { PressEvent } from '#/components/aria'
import { Text } from '#/components/AriaComponents'
import FocusRing from '#/components/styled/FocusRing'
import { useHandleFocusMove } from '#/hooks/focusHooks'
import { useFocusDirection } from '#/providers/FocusDirectionProvider'
import { lChColorToCssColor, type LChColor } from '#/services/Backend'
import { twMerge } from '#/utilities/tailwindMerge'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps extends Readonly<PropsWithChildren> {
  readonly 'data-testid'?: string
  /** When true, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /**
   * When true, the button has a red border signifying that it will be deleted,
   * or that it is excluded from search.
   */
  readonly negated?: boolean
  /** When true, the button cannot be clicked. */
  readonly isDisabled?: boolean
  readonly draggable?: boolean
  readonly color: LChColor
  readonly title?: string
  readonly onPress: (event: MouseEvent<HTMLButtonElement> | PressEvent) => void
  readonly onContextMenu?: (event: MouseEvent<HTMLElement>) => void
  readonly onDragStart?: (event: DragEvent<HTMLElement>) => void
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
  const { active = false, isDisabled = false, color, negated = false, draggable, title } = props
  const { onPress, onDragStart, onContextMenu } = props
  const { children: childrenRaw } = props
  const focusDirection = useFocusDirection()
  const handleFocusMove = useHandleFocusMove(focusDirection)
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  const isLight = color.lightness > 50

  return (
    <FocusRing within placement="after">
      <div
        className={twMerge(
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
          className={twMerge(
            'focus-child relative flex h-6 items-center whitespace-nowrap rounded-inherit px-[7px] opacity-75 transition-all after:pointer-events-none after:absolute after:inset after:rounded-full hover:opacity-100 focus:opacity-100',
            active && 'active',
            negated && 'after:border-2 after:border-delete',
          )}
          style={{ backgroundColor: lChColorToCssColor(color) }}
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
          {typeof childrenRaw !== 'string' ?
            childrenRaw
          : <Text
              truncate="1"
              className="max-w-24"
              color={isLight ? 'primary' : 'invert'}
              variant="body"
            >
              {childrenRaw}
            </Text>
          }
        </button>
      </div>
    </FocusRing>
  )
}
