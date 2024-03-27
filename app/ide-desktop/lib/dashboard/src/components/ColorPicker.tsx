/** @file A color picker to select from a predetermined list of colors. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import UnstyledButton from '#/components/styled/UnstyledButton'

import * as backend from '#/services/Backend'

/** Props for a {@link ColorPickerItem}. */
export interface InternalColorPickerItemProps {
  readonly id: string
  readonly color: backend.LChColor
  readonly setColor: (color: backend.LChColor) => void
}

/** An input in a {@link ColorPicker}. */
function ColorPickerItem(props: InternalColorPickerItemProps) {
  const { id, color, setColor } = props
  const inputRef = React.useRef<HTMLInputElement>(null)
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  const doSubmit = () => {
    inputRef.current?.click()
    setColor(color)
  }

  return (
    <FocusRing within>
      <aria.Label
        className="flex size-radio-button cursor-pointer rounded-full"
        onClick={event => {
          event.stopPropagation()
          doSubmit()
        }}
        onKeyDown={handleFocusMove}
      >
        <input type="radio" ref={inputRef} name={`color-picker-${id}`} className="peer hidden" />
        <UnstyledButton
          className="group pointer-events-none size-radio-button rounded-full p-radio-button-dot"
          style={{ backgroundColor: backend.lChColorToCssColor(color) }}
          onPress={doSubmit}
        >
          <div className="hidden size-radio-button-dot rounded-full bg-selected-frame peer-checked:group-[]:block" />
        </UnstyledButton>
      </aria.Label>
    </FocusRing>
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
  const id = React.useId()
  return (
    <div className="flex items-center gap-colors">
      {backend.COLORS.map((currentColor, i) => (
        <ColorPickerItem key={i} id={id} color={currentColor} setColor={setColor} />
      ))}
    </div>
  )
}
