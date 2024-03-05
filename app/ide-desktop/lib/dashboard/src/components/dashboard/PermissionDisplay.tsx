/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import * as permissionsModule from '#/utilities/permissions'

// =================
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps extends Readonly<React.PropsWithChildren> {
  readonly action: permissionsModule.PermissionAction
  readonly className?: string
  readonly onClick?: React.MouseEventHandler<HTMLButtonElement>
  readonly onMouseEnter?: React.MouseEventHandler<HTMLButtonElement>
  readonly onMouseLeave?: React.MouseEventHandler<HTMLButtonElement>
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
        <button
          disabled={!onClick}
          className={`${
            permissionsModule.PERMISSION_CLASS_NAME[permission.type]
          } inline-block h-text whitespace-nowrap rounded-full px-permission-mini-button-x py-permission-mini-button-y ${
            className ?? ''
          }`}
          onClick={onClick}
          onMouseEnter={onMouseEnter}
          onMouseLeave={onMouseLeave}
        >
          {children}
        </button>
      )
    }
    case permissionsModule.Permission.read:
    case permissionsModule.Permission.view: {
      return (
        <button
          className={`relative inline-block whitespace-nowrap rounded-full ${className ?? ''}`}
          onClick={onClick}
          onMouseEnter={onMouseEnter}
          onMouseLeave={onMouseLeave}
        >
          {permission.docs && (
            <div className="clip-path-top absolute size-full rounded-full border-2 border-permission-docs" />
          )}
          {permission.execute && (
            <div className="clip-path-bottom absolute size-full rounded-full border-2 border-permission-exec" />
          )}
          <div
            className={`${
              permissionsModule.PERMISSION_CLASS_NAME[permission.type]
            } m-permission-with-border h-text rounded-full px-permission-mini-button-x py-permission-mini-button-y`}
          >
            {children}
          </div>
        </button>
      )
    }
  }
}
