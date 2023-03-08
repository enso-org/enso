/** @file A label, which may be either user-defined, or a system warning message. */
import * as react from "react";

// =============
// === Types ===
// =============

/** Status of the label. Determines the appearance of the label. */
export enum Status {
    none = "none",
    warning = "warning",
    severeWarning = "severe-warning",
}

// =================
// === Constants ===
// =================

/** No icon indicates normal status. */
const NO_ICON = <></>;

/** Icon used to indicate a warning. */
const EXCLAMATION_ICON = (
    <svg
        width={27}
        height={27}
        viewBox="0 0 18 18"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
    >
        <path
            fill="#f9fafb"
            fillOpacity={0.7}
            fillRule="evenodd"
            d="M9 0A9 9 0 1 1 9 18 9 9 0 1 1 9 0M7.5 3.5H10.5L10 10.5H8L7.5 3.5ZM8 12L10 12 10 14 8 14"
        />
    </svg>
);

/** A mapping of label type to its corresponding icon. */
const statusIcon: Record<Status, JSX.Element> = {
    [Status.none]: NO_ICON,
    [Status.warning]: EXCLAMATION_ICON,
    [Status.severeWarning]: EXCLAMATION_ICON,
};

// =================
// === Component ===
// =================

interface Props {
    status?: Status;
}

/** A label, which may be either user-defined, or a system warning message. */
const Label = ({
    status = Status.none,
    children,
}: react.PropsWithChildren<Props>) => {
    return (
        <div
            className={`status-${status} whitespace-nowrap rounded-full inline-flex flex-row flex-nowrap align-middle leading-[27px] p-1 m-1`}
        >
            {statusIcon[status]}
            <div className="px-1">{children}</div>
        </div>
    );
};

export default Label;
