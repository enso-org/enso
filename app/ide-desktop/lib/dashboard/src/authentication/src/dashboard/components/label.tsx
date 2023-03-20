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

/** A mapping of label type to its corresponding icon. */
const STATUS_ICON: Record<Status, JSX.Element> = {
    [Status.none]: svg.NO_ICON,
    [Status.warning]: svg.EXCLAMATION_ICON,
    [Status.severeWarning]: svg.EXCLAMATION_ICON,
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
            className={`status-${status} whitespace-nowrap rounded-full inline-flex flex-row flex-nowrap align-middle leading-[27px] p-1 m-1`}
        >
            {STATUS_ICON[status]}
            <div className="px-1">{children}</div>
        </div>
    )
}

export default Label
