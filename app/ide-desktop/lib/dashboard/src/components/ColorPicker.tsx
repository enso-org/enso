/** @file A color picker to select from a predetermined list of colors. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import * as backend from '#/services/Backend'

/** Props for a {@link ColorPickerItem}. */
export interface InternalColorPickerItemProps {
  readonly color: backend.LChColor
  readonly setColor: (color: backend.LChColor) => void
}

/** An input in a {@link ColorPicker}. */
function ColorPickerItem(props: InternalColorPickerItemProps) {
  const { color, setColor } = props
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  return (
    <label
      className="flex size-radio-button cursor-pointer rounded-full"
      onClick={event => {
        event.stopPropagation()
        setColor(color)
      }}
      onKeyDown={handleFocusMove}
    >
      <input type="radio" name="new-label-color" className="peer hidden" />
      <button
        type="button"
        className="focus-child group pointer-events-none size-radio-button rounded-full p-radio-button-dot"
        style={{ backgroundColor: backend.lChColorToCssColor(color) }}
      >
        <div className="hidden size-radio-button-dot rounded-full bg-selected-frame peer-checked:group-[]:block" />
      </button>
    </label>
  )
}

// ===================
// === ColorPicker ===
// ===================

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
        <ColorPickerItem key={i} color={currentColor} setColor={setColor} />
      ))}
    </div>
  )
}
