/** @file Colored border around icons and text indicating permissions. */
import * as react from "react";

export enum Permission {
	owner = "owner",
	admin = "admin",
	edit = "edit",
	read = "read",
	readNoExec = "read-no-exec",
	exec = "exec",
	none = "none",
}

interface Props {
	permission: Permission;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const PermissionDisplay = ({ permission, children }: react.PropsWithChildren<Props>) => {
	return <div className={`border-perm-${permission} rounded-full border-2 inline-block mx-1`}><div className="bg-label-bg rounded-full px-4 py-1 m-0.5">{children}</div></div>;
}


export default PermissionDisplay;