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
  const { action, className, onPress, children: childrenRaw } = props
  const permission = permissionsModule.FROM_PERMISSION_ACTION[action]

  const children =
    typeof childrenRaw !== 'string' ? childrenRaw : (
      <ariaComponents.Text truncate="1" className="max-w-24 text-inherit">
        {childrenRaw}
      </ariaComponents.Text>
    )

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
            'inline-block h-6 whitespace-nowrap rounded-full px-[7px]',
            permissionsModule.PERMISSION_CLASS_NAME[permission.type],
            className,
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
            className,
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
              'm-1 flex h-6 items-center rounded-full px-[7px]',
              permissionsModule.PERMISSION_CLASS_NAME[permission.type],
              (permission.docs || permission.execute) && 'm-1',
            )}
          >
            {children}
          </div>
        </ariaComponents.Button>
      )
    }
  }
}
