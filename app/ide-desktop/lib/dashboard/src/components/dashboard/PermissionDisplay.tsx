/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import type * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import * as permissionsModule from '#/utilities/permissions'
import * as tailwindMerge from '#/utilities/tailwindMerge'

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
        <ariaComponents.Button
          size="custom"
          variant="custom"
          isDisabled={!onPress}
          className={tailwindMerge.twMerge(
            'inline-block h-text whitespace-nowrap rounded-full px-permission-mini-button-x py-permission-mini-button-y',
            permissionsModule.PERMISSION_CLASS_NAME[permission.type],
            className
          )}
          onPress={onPress ?? (() => {})}
        >
          {children}
        </ariaComponents.Button>
      )
    }
    case permissionsModule.Permission.read:
    case permissionsModule.Permission.view: {
      return (
        <ariaComponents.Button
          size="custom"
          variant="custom"
          className={tailwindMerge.twMerge(
            'relative inline-block whitespace-nowrap rounded-full',
            className
          )}
          onPress={onPress ?? (() => {})}
        >
          {permission.docs && (
            <div className="absolute size-full rounded-full border-2 border-permission-docs clip-path-top" />
          )}
          {permission.execute && (
            <div className="absolute size-full rounded-full border-2 border-permission-exec clip-path-bottom" />
          )}
          <div
            className={tailwindMerge.twMerge(
              'm-permission-with-border h-text rounded-full px-permission-mini-button-x py-permission-mini-button-y',
              permissionsModule.PERMISSION_CLASS_NAME[permission.type],
              (permission.docs || permission.execute) && 'm-permission-with-border'
            )}
          >
            {children}
          </div>
        </ariaComponents.Button>
      )
    }
  }
}
