/** @file Colored border around icons and text indicating permissions. */
import type * as aria from '#/components/aria'
import { Button } from '#/components/Button'
import { Text } from '#/components/Text'
import { PERMISSION_CLASS_NAME } from '#/utilities/permissionsClasses'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import * as permissionsModule from 'enso-common/src/utilities/permissions'
import * as React from 'react'

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
      <Text truncate="1" className="max-w-24 text-inherit">
        {childrenRaw}
      </Text>
    )

  switch (permission.type) {
    case permissionsModule.Permission.owner:
    case permissionsModule.Permission.admin:
    case permissionsModule.Permission.edit: {
      return (
        <Button
          size="custom"
          variant="custom"
          isDisabled={!onPress}
          className={tailwindMerge.twMerge(
            'inline-block h-6 whitespace-nowrap rounded-full px-[7px]',
            PERMISSION_CLASS_NAME[permission.type],
            className,
          )}
          onPress={onPress}
        >
          {children}
        </Button>
      )
    }
    case permissionsModule.Permission.read:
    case permissionsModule.Permission.view: {
      return (
        <Button
          size="custom"
          variant="custom"
          className={tailwindMerge.twMerge(
            'relative inline-block whitespace-nowrap rounded-full',
            className,
          )}
          isDisabled={!onPress}
          onPress={onPress}
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
              PERMISSION_CLASS_NAME[permission.type],
              (permission.docs || permission.execute) && 'm-1',
            )}
          >
            {children}
          </div>
        </Button>
      )
    }
  }
}
