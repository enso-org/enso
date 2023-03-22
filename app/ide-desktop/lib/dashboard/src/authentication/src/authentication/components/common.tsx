/** @file Contains common React components that are used by multiple pages of the Dashboard.
 *
 * For example, this file contains the {@link SvgIcon} component, which is used by the
 * `Registration` and `Login` components. */

import * as fontawesome from '@fortawesome/react-fontawesome'
import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'

import * as icons from '../../components/svg'

// =============
// === Input ===
// =============

export function Input(props: React.InputHTMLAttributes<HTMLInputElement>) {
    return (
        <input
            {...props}
            className={
                'text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 ' +
                'w-full py-2 focus:outline-none focus:border-blue-400'
            }
        />
    )
}

// ===============
// === SvgIcon ===
// ===============

interface SvgIconProps {
    data: string
}

export function SvgIcon(props: SvgIconProps) {
    return (
        <div
            className={
                'inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 ' +
                'text-gray-400'
            }
        >
            <span>
                <icons.Svg {...props} />
            </span>
        </div>
    )
}

// =======================
// === FontAwesomeIcon ===
// =======================

interface FontAwesomeIconProps {
    icon: fontawesomeIcons.IconDefinition
}

export function FontAwesomeIcon(props: FontAwesomeIconProps) {
    return (
        <span
            className={
                'absolute left-0 top-0 flex items-center justify-center h-full w-10 ' +
                'text-blue-500'
            }
        >
            <fontawesome.FontAwesomeIcon icon={props.icon} />
        </span>
    )
}
