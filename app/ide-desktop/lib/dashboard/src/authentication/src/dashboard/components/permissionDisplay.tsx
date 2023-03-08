/** @file Colored border around icons and text indicating permissions. */
import * as react from "react";

// =============
// === Types ===
// =============

/** Type of permission. This determines what kind of border is displayed. */
export enum Permission {
    owner = "owner",
    admin = "admin",
    regular = "regular",
}

/** Detailed permission information. This is used to draw the border. */
export type Permissions =
    | { type: Permission.owner }
    | { type: Permission.admin }
    | {
          type: Permission.regular;
          read: boolean;
          write: boolean;
          docsWrite: boolean;
          exec: boolean;
      };

// =================
// === Component ===
// =================

interface Props {
    permissions: Permissions;
}

/** Colored border around icons and text indicating permissions. */
const PermissionDisplay = ({
    permissions,
    children,
}: react.PropsWithChildren<Props>) => {
    let permissionBorder;
    switch (permissions.type) {
        case Permission.owner: {
            permissionBorder = (
                <div className="border-perm-owner border-2 rounded-full absolute w-full h-full"></div>
            );
            break;
        }
        case Permission.admin: {
            permissionBorder = (
                <div className="border-perm-admin border-2 rounded-full absolute w-full h-full"></div>
            );
            break;
        }
        case Permission.regular: {
            permissionBorder = (
                <>
                    <div
                        className={`border-perm-${
                            permissions.write ? "write" : "none"
                        } quadrant-top-left border-2 rounded-full absolute w-full h-full`}
                    ></div>
                    <div
                        className={`border-perm-${
                            permissions.read ? "read" : "none"
                        } quadrant-top-right border-2 rounded-full absolute w-full h-full`}
                    ></div>
                    <div
                        className={`border-perm-${
                            permissions.exec ? "exec" : "none"
                        } quadrant-bottom-left border-2 rounded-full absolute w-full h-full`}
                    ></div>
                    <div
                        className={`border-perm-${
                            permissions.docsWrite ? "docs-write" : "none"
                        } quadrant-bottom-right border-2 rounded-full absolute w-full h-full`}
                    ></div>
                </>
            );
            break;
        }
    }

    return (
        <div className="m-1 relative inline-block">
            {permissionBorder}
            <div className="bg-label-bg rounded-full px-4 py-1 m-1">
                {children}
            </div>
        </div>
    );
};

export default PermissionDisplay;
