/** @file Colored border around icons and text indicating permissions. */
import * as React from 'react'

import * as tooltipHooks from '#/hooks/tooltipHooks'

import type * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import UnstyledButton from '#/components/UnstyledButton'

import * as permissionsModule from '#/utilities/permissions'

// =================
// === Component ===
// =================

/** Props for a {@link PermissionDisplay}. */
export interface PermissionDisplayProps extends Readonly<React.PropsWithChildren> {
  readonly action: permissionsModule.PermissionAction
  readonly className?: string
  readonly onPress?: (event: aria.PressEvent) => void
}

/** Colored border around icons and text indicating permissions. */
export default function PermissionDisplay(props: PermissionDisplayProps) {
  const { action, className, onPress: onPress, children } = props
  const { needsTooltip, tooltipTargetRef } = tooltipHooks.useNeedsTooltip()
  const permission = permissionsModule.FROM_PERMISSION_ACTION[action]

  switch (permission.type) {
    case permissionsModule.Permission.owner:
    case permissionsModule.Permission.admin:
    case permissionsModule.Permission.edit: {
      return (
        <ariaComponents.TooltipTrigger>
          <UnstyledButton
            ref={tooltipTargetRef}
            isDisabled={!onPress}
            className={`${
              permissionsModule.PERMISSION_CLASS_NAME[permission.type]
            } inline-block h-text max-w-40 shrink-0 overflow-hidden text-ellipsis whitespace-nowrap rounded-full px-permission-mini-button-x py-permission-mini-button-y ${
              className ?? ''
            }`}
            onPress={onPress ?? (() => {})}
          >
            {children}
          </UnstyledButton>
          {needsTooltip && <ariaComponents.Tooltip>{children}</ariaComponents.Tooltip>}
        </ariaComponents.TooltipTrigger>
      )
    }
    case permissionsModule.Permission.read:
    case permissionsModule.Permission.view: {
      return (
        <ariaComponents.TooltipTrigger>
          <UnstyledButton
            ref={tooltipTargetRef}
            className={`relative inline-block max-w-40 shrink-0 overflow-hidden text-ellipsis whitespace-nowrap rounded-full ${className ?? ''}`}
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
          {needsTooltip && <ariaComponents.Tooltip>{children}</ariaComponents.Tooltip>}
        </ariaComponents.TooltipTrigger>
      )
    }
  }
}
