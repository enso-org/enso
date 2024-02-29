/** @file A color picker to select from a predetermined list of colors. */
import * as React from 'react'

import * as color from '#/utilities/color'

/** Props for a {@link ColorPicker}. */
export interface ColorPickerProps {
  readonly setColor: (color: color.LChColor) => void
}

/** A color picker to select from a predetermined list of colors. */
export default function ColorPicker(props: ColorPickerProps) {
  const { setColor } = props
  return (
    <>
      {color.COLORS.map((currentColor, i) => (
        <label
          key={i}
          className="cursor-pointer rounded-full w-4 h-4"
          onClick={event => {
            event.stopPropagation()
            setColor(currentColor)
          }}
        >
          <input type="radio" name="new-label-color" className="peer hidden" />
          <button
            type="button"
            className="group pointer-events-none rounded-full p-1 w-4 h-4"
            style={{
              backgroundColor: color.lChColorToCssColor(currentColor),
            }}
          >
            <div className="hidden peer-checked:group-[]:block bg-frame-selected rounded-full w-2 h-2" />
          </button>
        </label>
      ))}
    </>
  )
}
