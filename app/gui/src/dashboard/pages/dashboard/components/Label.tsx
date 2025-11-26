/** @file An label that can be applied to an asset. */
import * as aria from '#/components/aria'
import { Button } from '#/components/Button'
import FocusRing from '#/components/styled/FocusRing'
import { Text } from '#/components/Text'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'
import type { Label as BackendLabel } from 'enso-common/src/services/Backend'
import { lChColorToCssColor, type LChColor } from 'enso-common/src/services/Backend'
import { forwardRef, type ForwardedRef, type PropsWithChildren } from 'react'

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
  const {
    children: childrenRaw,
    active = false,
    isDisabled = false,
    color,
    title,
    onPress,
    label,
    onDelete,
  } = props
  const isLight = color.lightness > MAXIMUM_LIGHTNESS_FOR_DARK_COLORS

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
          <aria.Button
            data-testid={props['data-testid']}
            type="button"
            isDisabled={isDisabled}
            onPress={() => {
              onPress?.(label)
            }}
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
          </aria.Button>
          {onDelete && (
            <Button
              icon="tab_close"
              variant="icon"
              size="small"
              onPress={onDelete}
              className={twJoin('ml-2', !isLight && 'text-white')}
            />
          )}
        </div>
      </div>
    </FocusRing>
  )
})
