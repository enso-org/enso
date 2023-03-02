/** @file A label that may have a warning symbol */
import * as react from "react";

export enum Status {
	none = "none",
	warning = "warning",
	severeWarning = "severe-warning",
}

const ICONS = {
	none: <></>,
	exclamation: <svg width="27" height="27" viewBox="0 0 18 18" fill="none" xmlns="http://www.w3.org/2000/svg">
		<path fill="#F9FAFB" fillOpacity="0.72" fillRule="evenodd" d="M9 0A9 9 0 1 1 9 18 9 9 0 1 1 9 0M7.5 3.5H10.5L10 10.5H8L7.5 3.5ZM8 12L10 12 10 14 8 14"/>
	</svg>,
};

const statusIcon: Record<Status, JSX.Element> = {
	[Status.none]: ICONS.none,
	[Status.warning]: ICONS.exclamation,
	[Status.severeWarning]: ICONS.exclamation,
};

interface Props {
	status?: Status;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const Label = ({ status = Status.none, children }: react.PropsWithChildren<Props>) => {
	return <div className={`status-${status} whitespace-nowrap rounded-full inline-flex flex-row flex-nowrap align-middle leading-[27px] p-1 m-1`}>{statusIcon[status]}<div className="px-1">{children}</div></div>;
}

export default Label;
