/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import type * as aria from '#/components/aria'
import UnstyledButton from '#/components/UnstyledButton'

import * as permissionsModule from '#/utilities/permissions'

// =================
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps extends Readonly<React.PropsWithChildren> {
  readonly action: permissionsModule.PermissionAction
  readonly className?: string
  readonly onPress?: ((event: aria.PressEvent) => void) | null
}

/** Colored border around icons and text indicating permissions. */
export default function PermissionDisplay(props: PermissionDisplayProps) {
  const { action, className, onPress, children } = props
  const permission = permissionsModule.FROM_PERMISSION_ACTION[action]

  switch (permission.type) {
    case permissionsModule.Permission.owner:
    case permissionsModule.Permission.admin:
    case permissionsModule.Permission.edit: {
      return (
        <UnstyledButton
          isDisabled={!onPress}
          className={`${
            permissionsModule.PERMISSION_CLASS_NAME[permission.type]
          } inline-block h-text whitespace-nowrap rounded-full px-permission-mini-button-x py-permission-mini-button-y ${
            className ?? ''
          }`}
          onPress={onPress ?? (() => {})}
        >
          {children}
        </UnstyledButton>
      )
    }
    case permissionsModule.Permission.read:
    case permissionsModule.Permission.view: {
      return (
        <UnstyledButton
          className={`relative inline-block whitespace-nowrap rounded-full ${className ?? ''}`}
          onPress={onPress ?? (() => {})}
        >
          {permission.docs && (
            <div className="absolute size-full rounded-full border-2 border-permission-docs clip-path-top" />
          )}
          {permission.execute && (
            <div className="absolute size-full rounded-full border-2 border-permission-exec clip-path-bottom" />
          )}
          <div
            className={`${
              permissionsModule.PERMISSION_CLASS_NAME[permission.type]
            } m-permission-with-border h-text rounded-full px-permission-mini-button-x py-permission-mini-button-y`}
          >
            {children}
          </div>
        </UnstyledButton>
      )
    }
  }
}
