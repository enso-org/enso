/** @file A styled input that includes an icon. */
import * as React from 'react'

import EyeCrossedIcon from 'enso-assets/eye_crossed.svg'
import EyeIcon from 'enso-assets/eye.svg'

import type * as controlledInput from './controlledInput'
import ControlledInput from './controlledInput'
import SvgIcon from './svgIcon'

// =============
// === Input ===
// =============

/** Props for a {@link Input}. */
export interface InputProps extends controlledInput.ControlledInputProps {
    allowShowingPassword?: boolean
    label: string | null
    icon: string
    footer?: React.ReactNode
}

/** A styled input that includes an icon. */
export default function Input(props: InputProps) {
    const { allowShowingPassword = false, label, icon, footer, ...passthrough } = props
    const [isShowingPassword, setIsShowingPassword] = React.useState(false)
    const input = (
        <div className="relative">
            <SvgIcon src={icon} />
            <ControlledInput {...passthrough} type={isShowingPassword ? 'text' : props.type} />
            {props.type === 'password' && allowShowingPassword && (
                // FIXME:
                <SvgIcon
                    src={isShowingPassword ? EyeIcon : EyeCrossedIcon}
                    className="cursor-pointer rounded-full"
                    positionClassName="right-0 top-0"
                    onClick={() => {
                        setIsShowingPassword(show => !show)
                    }}
                />
            )}
        </div>
    )
    return label != null || footer != null ? (
        <label className="flex flex-col gap-1">
            {label}
            {input}
            {footer}
        </label>
    ) : (
        input
    )
}
