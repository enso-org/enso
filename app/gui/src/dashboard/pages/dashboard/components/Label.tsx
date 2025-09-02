/** @file An label that can be applied to an asset. */
import { Button } from '#/components/Button'
import FocusRing from '#/components/styled/FocusRing'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { Label as BackendLabel } from '#/services/Backend'
import { lChColorToCssColor, type LChColor } from '#/services/Backend'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'
import {
  forwardRef,
  type DragEvent,
  type ForwardedRef,
  type MouseEvent,
  type PropsWithChildren,
} from 'react'

const MAXIMUM_LIGHTNESS_FOR_DARK_COLORS = 50

/** Props for a {@link Label}. */
interface InternalLabelProps extends Readonly<PropsWithChildren> {
  readonly 'data-testid'?: string
  /** When true, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When true, the button cannot be clicked. */
  readonly isDisabled?: boolean
  readonly draggable?: boolean
  readonly color: LChColor
  readonly title?: string
  readonly label?: BackendLabel
  readonly onPress?: (label?: BackendLabel) => void
  readonly onDelete?: () => Promise<void> | void
  readonly onContextMenu?: (event: MouseEvent<HTMLElement>) => void
  readonly onDragStart?: (event: DragEvent<HTMLElement>) => void
}

/** An label that can be applied to an asset. */
export default forwardRef(function Label(
  props: InternalLabelProps,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const { active = false, isDisabled = false, color, draggable, title } = props
  const { onPress, onDragStart, onContextMenu, label, onDelete } = props
  const { children: childrenRaw } = props
  const isLight = color.lightness > MAXIMUM_LIGHTNESS_FOR_DARK_COLORS

  const handleDelete = useEventCallback(onDelete)
  const onClick = useEventCallback((event: MouseEvent<HTMLButtonElement>) => {
    event.stopPropagation()
    onPress?.(label)
  })

  const onDragStartStableCallback = useEventCallback((e: DragEvent<HTMLElement>) => {
    e.stopPropagation()
    onDragStart?.(e)
  })

  return (
    <FocusRing within placement="after">
      <div ref={ref} className="relative rounded-full">
        {/* An `aria.Button` MUST NOT be used here, as it breaks dragging. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <button
          type="button"
          data-testid={props['data-testid']}
          draggable={draggable}
          title={title}
          disabled={isDisabled}
          className={twMerge(
            'relative flex h-6 items-center whitespace-nowrap rounded-inherit px-[7px] opacity-50 transition-all hover:opacity-100 focus:opacity-100',
            onPress == null && 'cursor-default',
            active && 'active',
          )}
          style={{ backgroundColor: lChColorToCssColor(color) }}
          onClick={onClick}
          onDragStart={onDragStartStableCallback}
          onContextMenu={onContextMenu}
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

          {onDelete && (
            <Button
              icon="tab_close"
              variant="icon"
              size="small"
              onPress={handleDelete}
              className={twJoin('ml-2', !isLight && 'text-white')}
            />
          )}
        </button>
      </div>
    </FocusRing>
  )
})
