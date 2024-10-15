/** @file A styled input specific to settings pages. */
import {
  useContext,
  useRef,
  useState,
  type ChangeEventHandler,
  type ForwardedRef,
  type HTMLInputAutoCompleteAttribute,
  type KeyboardEvent,
  type RefAttributes,
  type SyntheticEvent,
} from 'react'

import EyeIcon from '#/assets/eye.svg'
import EyeCrossedIcon from '#/assets/eye_crossed.svg'
import { Group, Input, InputContext, mergeProps, type InputProps } from '#/components/aria'
import { Button } from '#/components/AriaComponents'
import FocusRing from '#/components/styled/FocusRing'
import { useFocusChild } from '#/hooks/focusHooks'
import { useText } from '#/providers/TextProvider'
import { forwardRef } from '#/utilities/react'
import { twMerge } from '#/utilities/tailwindMerge'

// =====================
// === SettingsInput ===
// =====================

/** Props for an {@link SettingsInput}. */
export interface SettingsInputProps {
  readonly isDisabled?: boolean
  readonly type?: string
  readonly placeholder?: string
  readonly autoComplete?: HTMLInputAutoCompleteAttribute
  readonly onChange?: ChangeEventHandler<HTMLInputElement>
  readonly onSubmit?: (event: SyntheticEvent<HTMLInputElement>) => void
}

export default forwardRef(SettingsInput)

/** A styled input specific to settings pages. */
function SettingsInput(props: SettingsInputProps, ref: ForwardedRef<HTMLInputElement>) {
  const { isDisabled = false, type, placeholder, autoComplete, onChange, onSubmit } = props
  const focusChildProps = useFocusChild()
  const { getText } = useText()
  // This is SAFE. The value of this context is never a `SlottedContext`.
  // eslint-disable-next-line no-restricted-syntax
  const inputProps = (useContext(InputContext) ?? null) as InputProps | null
  const [isShowingPassword, setIsShowingPassword] = useState(false)
  const cancelled = useRef(false)

  const onKeyDown = (event: KeyboardEvent<HTMLInputElement>) => {
    switch (event.key) {
      case 'Escape': {
        cancelled.current = true
        event.stopPropagation()
        event.currentTarget.value = String(inputProps?.defaultValue ?? '')
        event.currentTarget.blur()
        break
      }
      default: {
        cancelled.current = false
        break
      }
    }
  }

  return (
    <div className="text my-auto grow font-bold">
      <FocusRing within placement="after">
        <Group className="relative rounded-full after:pointer-events-none after:absolute after:inset after:rounded-full">
          <Input
            {...mergeProps<InputProps & RefAttributes<HTMLInputElement>>()(
              {
                ref,
                className: twMerge(
                  'w-full rounded-full bg-transparent font-bold placeholder-black/30 transition-colors invalid:border invalid:border-red-700 hover:bg-selected-frame focus:bg-selected-frame px-1 border-0.5 border-transparent',
                  !isDisabled && 'border-primary/20',
                ),
                ...(type == null ? {} : { type: isShowingPassword ? 'text' : type }),
                disabled: isDisabled,
                size: 1,
                autoComplete,
                placeholder,
                onKeyDown,
                onChange,
                onBlur: (event) => {
                  if (!cancelled.current) {
                    onSubmit?.(event)
                  }
                },
              },
              focusChildProps,
            )}
          />
          {type === 'password' && (
            <Button
              size="custom"
              variant="custom"
              isActive
              icon={isShowingPassword ? EyeIcon : EyeCrossedIcon}
              aria-label={isShowingPassword ? getText('hidePassword') : getText('showPassword')}
              className="absolute right-2 top-1 size-6 cursor-pointer rounded-full"
              onPress={() => {
                setIsShowingPassword((show) => !show)
              }}
            />
          )}
        </Group>
      </FocusRing>
    </div>
  )
}
