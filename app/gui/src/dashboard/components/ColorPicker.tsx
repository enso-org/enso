/** @file A color picker to select from a predetermined list of colors. */
import * as React from 'react'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import RadioGroup from '#/components/styled/RadioGroup'

import * as backend from 'enso-common/src/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

/** Props for a {@link ColorPickerItem}. */
export interface InternalColorPickerItemProps {
  readonly color: backend.LChColor
}

/** An input in a {@link ColorPicker}. */
function ColorPickerItem(props: InternalColorPickerItemProps) {
  const { color } = props
  const cssColor = backend.lChColorToCssColor(color)

  return (
    <FocusRing within>
      <aria.Radio
        value={cssColor}
        className="group flex h-6 w-6 cursor-pointer items-center justify-center rounded-full"
        style={{ backgroundColor: cssColor }}
      >
        <div className="hidden aspect-square h-3 w-3 rounded-full bg-selected-frame group-selected:block" />
      </aria.Radio>
    </FocusRing>
  )
}

/** Props for a {@link ColorPicker}. */
export interface ColorPickerProps extends Readonly<Omit<aria.RadioGroupProps, 'className'>> {
  readonly children?: React.ReactNode
  readonly className?: string
  readonly pickerClassName?: string
  readonly setColor: (color: backend.LChColor) => void
}

/** A color picker to select from a predetermined list of colors. */
export default React.forwardRef(ColorPicker)

/** A color picker to select from a predetermined list of colors. */
function ColorPicker(props: ColorPickerProps, ref: React.ForwardedRef<HTMLDivElement>) {
  const { className, pickerClassName = '', children, setColor, ...radioGroupProps } = props
  return (
    <RadioGroup
      ref={ref}
      {...radioGroupProps}
      orientation="horizontal"
      className={tailwindMerge.twMerge('flex flex-col', className)}
      onChange={(value) => {
        const color = backend.COLOR_STRING_TO_COLOR.get(value)
        if (color != null) {
          setColor(color)
        }
      }}
    >
      {children}
      <div
        className={tailwindMerge.twMerge(
          'flex items-center justify-between gap-colors',
          pickerClassName,
        )}
      >
        {backend.COLORS.map((currentColor, i) => (
          <ColorPickerItem key={i} color={currentColor} />
        ))}
      </div>
    </RadioGroup>
  )
}
