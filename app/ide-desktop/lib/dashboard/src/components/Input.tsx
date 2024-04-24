/** @file A styled input that includes an icon. */
import * as React from 'react'

import EyeCrossedIcon from 'enso-assets/eye_crossed.svg'
import EyeIcon from 'enso-assets/eye.svg'

import type * as controlledInput from '#/components/ControlledInput'
import ControlledInput from '#/components/ControlledInput'
import SvgIcon from '#/components/SvgIcon'

// =============
// === Input ===
// =============

/** Props for a {@link Input}. */
export interface InputProps extends controlledInput.ControlledInputProps {
  readonly allowShowingPassword?: boolean
  readonly icon: string
}

/** A styled input that includes an icon. */
export default function Input(props: InputProps) {
  const { allowShowingPassword = false, icon, type, ...passthrough } = props
  const [isShowingPassword, setIsShowingPassword] = React.useState(false)

  return (
    <div className="relative">
      <SvgIcon src={icon} />
      <ControlledInput {...passthrough} type={isShowingPassword ? 'text' : type} />
      {type === 'password' && allowShowingPassword && (
        <SvgIcon
          src={isShowingPassword ? EyeIcon : EyeCrossedIcon}
          className="cursor-pointer rounded-full"
          positionClassName="top right"
          onClick={() => {
            setIsShowingPassword(show => !show)
          }}
        />
      )}
    </div>
  )
}
