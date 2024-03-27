/** @file A styled input specific to settings pages. */
import * as React from 'react'

import EyeCrossedIcon from 'enso-assets/eye_crossed.svg'
import EyeIcon from 'enso-assets/eye.svg'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

// =============
// === Input ===
// =============

/** Props for an {@link SettingsInput}. */
export interface SettingsInputProps {
  readonly initialValue: string
  readonly type?: string
  readonly placeholder?: string
  readonly onChange?: React.ChangeEventHandler<HTMLInputElement>
  readonly onSubmit?: (value: string) => void
}

/** A styled input specific to settings pages. */
function SettingsInput(props: SettingsInputProps, ref: React.ForwardedRef<HTMLInputElement>) {
  const { initialValue, type, placeholder, onChange, onSubmit } = props
  const [isShowingPassword, setIsShowingPassword] = React.useState(false)
  const cancelled = React.useRef(false)
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  const onKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    switch (event.key) {
      case 'Escape': {
        cancelled.current = true
        event.stopPropagation()
        event.currentTarget.value = initialValue
        event.currentTarget.blur()
        break
      }
      case 'Enter': {
        cancelled.current = false
        event.stopPropagation()
        event.currentTarget.blur()
        break
      }
      case 'Tab': {
        cancelled.current = false
        event.currentTarget.blur()
        break
      }
      default: {
        handleFocusMove(event)
        cancelled.current = false
        break
      }
    }
  }

  return (
    <div className="text my-auto grow font-bold">
      <FocusRing within placement="after">
        <aria.Group className="relative rounded-full after:pointer-events-none after:absolute after:inset after:rounded-full">
          <aria.Input
            ref={ref}
            className="focus-child settings-value w-full rounded-full bg-transparent font-bold placeholder-black/30 transition-colors invalid:border invalid:border-red-700 hover:bg-selected-frame focus:bg-selected-frame"
            type={isShowingPassword ? 'text' : type}
            size={1}
            defaultValue={initialValue}
            placeholder={placeholder}
            onKeyDown={onKeyDown}
            onChange={onChange}
            onBlur={event => {
              if (!cancelled.current) {
                onSubmit?.(event.currentTarget.value)
              }
            }}
          />
          {type === 'password' && (
            <SvgMask
              src={isShowingPassword ? EyeIcon : EyeCrossedIcon}
              className="absolute right-2 top-1 cursor-pointer rounded-full"
              onClick={() => {
                setIsShowingPassword(show => !show)
              }}
            />
          )}
        </aria.Group>
      </FocusRing>
    </div>
  )
}

export default React.forwardRef(SettingsInput)
