/** @file An label that can be applied to an asset. */
import { Button } from '#/components/Button'
import FocusRing from '#/components/styled/FocusRing'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { Label as BackendLabel } from '#/services/Backend'
import { lChColorToCssColor, type LChColor } from '#/services/Backend'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'
import { forwardRef, type ForwardedRef, type MouseEvent, type PropsWithChildren } from 'react'

const MAXIMUM_LIGHTNESS_FOR_DARK_COLORS = 50

/** Props for a {@link Label}. */
interface LabelProps extends Readonly<PropsWithChildren> {
  readonly 'data-testid'?: string
  /** When true, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When true, the button cannot be clicked. */
  readonly isDisabled?: boolean
  readonly color: LChColor
  readonly title?: string
  readonly label?: BackendLabel
  readonly onPress?: (label?: BackendLabel) => void
  readonly onDelete?: () => Promise<void> | void
}

/** An label that can be applied to an asset. */
export default forwardRef(function Label(props: LabelProps, ref: ForwardedRef<HTMLDivElement>) {
  const { active = false, isDisabled = false, color, title, onPress, label, onDelete } = props
  const { children: childrenRaw } = props
  const isLight = color.lightness > MAXIMUM_LIGHTNESS_FOR_DARK_COLORS

  const handleDelete = useEventCallback(onDelete)
  const onClick = useEventCallback((event: MouseEvent<HTMLButtonElement>) => {
    event.stopPropagation()
    onPress?.(label)
  })

  return (
    <FocusRing within placement="after">
      <div ref={ref} className="relative rounded-full">
        <div
          title={title}
          className={twMerge(
            'relative flex h-6 items-center whitespace-nowrap rounded-inherit px-[7px] opacity-50 transition-all hover:opacity-100 focus:opacity-100',
            onPress == null && 'cursor-default',
            active && 'active',
          )}
          style={{ backgroundColor: lChColorToCssColor(color) }}
        >
          {/* An `aria.Button` MUST NOT be used here, as it breaks dragging. */}
          {/* eslint-disable-next-line no-restricted-syntax */}
          <button
            data-testid={props['data-testid']}
            type="button"
            disabled={isDisabled}
            onClick={onClick}
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
          {onDelete && (
            <Button
              icon="tab_close"
              variant="icon"
              size="small"
              onPress={handleDelete}
              className={twJoin('ml-2', !isLight && 'text-white')}
            />
          )}
        </div>
      </div>
    </FocusRing>
  )
})
