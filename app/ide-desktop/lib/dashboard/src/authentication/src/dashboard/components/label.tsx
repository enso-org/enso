/** @file A label, which may be either user-defined, or a system warning message. */
import * as react from 'react'

import * as svg from '../../components/svg'

// =============
// === Types ===
// =============

/** Status of the label. Determines the appearance of the label. */
export enum Status {
    none = 'none',
    warning = 'warning',
    severeWarning = 'severe-warning',
}

// =================
// === Constants ===
// =================

/** A mapping of label type to its corresponding tailwind classes. */
const CSS_CLASS: Record<Status, string> = {
    [Status.none]: 'bg-label',
    [Status.warning]: 'text-white bg-warning',
    [Status.severeWarning]: 'text-white bg-severe-warning',
} as const

/** A mapping of label type to its corresponding icon. */
const STATUS_ICON: Record<Status, JSX.Element | null> = {
    [Status.none]: null,
    [Status.warning]: <div className="m-1">{svg.EXCLAMATION_ICON}</div>,
    [Status.severeWarning]: <div className="m-1">{svg.EXCLAMATION_ICON}</div>,
}

// =================
// === Component ===
// =================

export interface LabelProps {
    status?: Status
}

/** A label, which may be either user-defined, or a system warning message. */
function Label({ status = Status.none, children }: react.PropsWithChildren<LabelProps>) {
    return (
        <div
            className={`${CSS_CLASS[status]} whitespace-nowrap rounded-full inline-flex flex-row flex-nowrap align-middle items-center leading-[27px] m-1`}
        >
            {STATUS_ICON[status]}
            <div className={STATUS_ICON[status] ? 'pr-2' : 'px-2'}>{children}</div>
        </div>
    )
}

export default Label
