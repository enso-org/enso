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
	permission1: Permission;
	permission2: Permission;
	permission3: Permission;
	permission4: Permission;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const PermissionDisplay4Segment = ({ permission1, permission2, permission3, permission4, children }: react.PropsWithChildren<Props>) => {
	return <div className="mx-1 relative inline-block">
		<div className={`border-perm-${permission1} border-2 rounded-full absolute w-full h-full permission-top-left`}></div>
		<div className={`border-perm-${permission2} border-2 rounded-full absolute w-full h-full permission-top-right`}></div>
		<div className={`border-perm-${permission3} border-2 rounded-full absolute w-full h-full permission-bottom-left`}></div>
		<div className={`border-perm-${permission4} border-2 rounded-full absolute w-full h-full permission-bottom-right`}></div>
		<div className="bg-label-bg rounded-full px-4 py-1 m-0.5">{children}</div>
	</div>;
}


export default PermissionDisplay4Segment;