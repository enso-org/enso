/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import * as permissionsModule from '#/utilities/permissions'

// =================
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps extends React.PropsWithChildren {
  action: permissionsModule.PermissionAction
  className?: string
  onClick?: React.MouseEventHandler<HTMLDivElement>
  onMouseEnter?: React.MouseEventHandler<HTMLDivElement>
  onMouseLeave?: React.MouseEventHandler<HTMLDivElement>
}

/** Colored border around icons and text indicating permissions. */
export default function PermissionDisplay(props: PermissionDisplayProps) {
  const { action, className, onClick, onMouseEnter, onMouseLeave, children } = props
  const permission = permissionsModule.FROM_PERMISSION_ACTION[action]

  switch (permission.type) {
    case permissionsModule.Permission.owner:
    case permissionsModule.Permission.admin:
    case permissionsModule.Permission.edit: {
      return (
        <div
          className={`${
            permissionsModule.PERMISSION_CLASS_NAME[permission.type]
          } inline-block rounded-full h-6 px-1.75 py-0.5 ${className ?? ''}`}
          onClick={onClick}
          onMouseEnter={onMouseEnter}
          onMouseLeave={onMouseLeave}
        >
          {children}
        </div>
      )
    }
    case permissionsModule.Permission.read:
    case permissionsModule.Permission.view: {
      return (
        <div
          className={`relative inline-block rounded-full ${className ?? ''}`}
          onClick={onClick}
          onMouseEnter={onMouseEnter}
          onMouseLeave={onMouseLeave}
        >
          {permission.docs && (
            <div className="border-permission-docs clip-path-top border-2 rounded-full absolute w-full h-full" />
          )}
          {permission.execute && (
            <div className="border-permission-exec clip-path-bottom border-2 rounded-full absolute w-full h-full" />
          )}
          <div
            className={`${
              permissionsModule.PERMISSION_CLASS_NAME[permission.type]
            } rounded-full h-6 px-1.75 py-0.5 m-1`}
          >
            {children}
          </div>
        </div>
      )
    }
  }
}
