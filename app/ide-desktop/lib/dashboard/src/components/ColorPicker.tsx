/** @file A color picker to select from a predetermined list of colors. */
import * as React from 'react'

import * as backend from '#/services/Backend'

/** Props for a {@link ColorPicker}. */
export interface ColorPickerProps {
  readonly setColor: (color: backend.LChColor) => void
}

/** A color picker to select from a predetermined list of colors. */
export default function ColorPicker(props: ColorPickerProps) {
  const { setColor } = props
  return (
    <div className="flex items-center gap-colors">
      {backend.COLORS.map((currentColor, i) => (
        <label
          key={i}
          className="flex cursor-pointer rounded-full size-radio-button"
          onClick={event => {
            event.stopPropagation()
            setColor(currentColor)
          }}
        >
          <input type="radio" name="new-label-color" className="peer hidden" />
          <button
            type="button"
            className="group pointer-events-none rounded-full p-radio-button-dot size-radio-button"
            style={{ backgroundColor: backend.lChColorToCssColor(currentColor) }}
          >
            <div className="hidden peer-checked:group-[]:block bg-selected-frame rounded-full size-radio-button-dot" />
          </button>
        </label>
      ))}
    </div>
  )
}
