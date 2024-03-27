/** @file A selector for all possible permissions. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import PermissionTypeSelector from '#/components/dashboard/PermissionTypeSelector'
import Modal from '#/components/Modal'
import FocusRing from '#/components/styled/FocusRing'
import UnstyledButton from '#/components/styled/UnstyledButton'

import type * as backend from '#/services/Backend'

import type * as permissions from '#/utilities/permissions'
import * as permissionsModule from '#/utilities/permissions'

// =================
// === Constants ===
// =================

/** The horizontal offset of the {@link PermissionTypeSelector} from its parent element. */
const TYPE_SELECTOR_X_OFFSET_PX = -8
/** The vertical offset of the {@link PermissionTypeSelector} from its parent element. */
const TYPE_SELECTOR_Y_OFFSET_PX = 28
/** The vertical offset of the label's clip path from its parent element.
 * Optimized for 100% zoom. */
const LABEL_CLIP_Y_OFFSET_PX = 0.5
/** The border radius of the permission label. */
const LABEL_BORDER_RADIUS_PX = 12
/** The width of the straight section of the permission label. */
const LABEL_STRAIGHT_WIDTH_PX = 97

// ==========================
// === PermissionSelector ===
// ==========================

/** Props for a {@link PermissionSelector}. */
export interface PermissionSelectorProps {
  readonly showDelete?: boolean
  /** When `true`, the button is not clickable. */
  readonly isDisabled?: boolean
  /** When `true`, the button has lowered opacity when it is disabled. */
  readonly input?: boolean
  /** Overrides the vertical offset of the {@link PermissionTypeSelector}. */
  readonly typeSelectorYOffsetPx?: number
  readonly error?: string | null
  readonly selfPermission: permissions.PermissionAction
  /** If this prop changes, the internal state will be updated too. */
  readonly action: permissions.PermissionAction
  readonly assetType: backend.AssetType
  readonly className?: string
  readonly onChange: (action: permissions.PermissionAction) => void
  readonly doDelete?: () => void
}

/** A horizontal selector for all possible permissions. */
export default function PermissionSelector(props: PermissionSelectorProps) {
  const { showDelete = false, isDisabled = false, input = false, typeSelectorYOffsetPx } = props
  const { error, selfPermission, action: actionRaw, assetType, className } = props
  const { onChange, doDelete } = props
  const { getText } = textProvider.useText()
  const [action, setActionRaw] = React.useState(actionRaw)
  const [TheChild, setTheChild] = React.useState<(() => JSX.Element) | null>()
  const permission = permissionsModule.FROM_PERMISSION_ACTION[action]

  const setAction = (newAction: permissions.PermissionAction) => {
    setActionRaw(newAction)
    onChange(newAction)
  }

  const doShowPermissionTypeSelector = (event: React.SyntheticEvent<HTMLElement>) => {
    const position = event.currentTarget.getBoundingClientRect()
    const originalLeft = position.left + window.scrollX
    const originalTop = position.top + window.scrollY
    const left = originalLeft + TYPE_SELECTOR_X_OFFSET_PX
    const top = originalTop + (typeSelectorYOffsetPx ?? TYPE_SELECTOR_Y_OFFSET_PX)
    // The border radius of the label. This is half of the label's height.
    const r = LABEL_BORDER_RADIUS_PX
    const clipPath =
      // A rectangle covering the entire screen
      'path(evenodd, "M0 0L3840 0 3840 2160 0 2160Z' +
      // Move to top left of label
      `M${originalLeft + LABEL_BORDER_RADIUS_PX} ${originalTop + LABEL_CLIP_Y_OFFSET_PX}` +
      // Top straight edge of label
      `h${LABEL_STRAIGHT_WIDTH_PX}` +
      // Right semicircle of label
      `a${r} ${r} 0 0 1 0 ${r * 2}` +
      // Bottom straight edge of label
      `h-${LABEL_STRAIGHT_WIDTH_PX}` +
      // Left semicircle of label
      `a${r} ${r} 0 0 1 0 -${r * 2}Z")`
    setTheChild(oldTheChild =>
      oldTheChild != null
        ? null
        : function Child() {
            return (
              <Modal
                className="fixed size-full overflow-auto"
                onClick={() => {
                  setTheChild(null)
                }}
              >
                <div style={{ clipPath }} className="absolute size-full bg-dim" />
                <PermissionTypeSelector
                  showDelete={showDelete}
                  type={permission.type}
                  assetType={assetType}
                  selfPermission={selfPermission}
                  style={{ left, top }}
                  onChange={type => {
                    setTheChild(null)
                    if (type === permissionsModule.Permission.delete) {
                      doDelete?.()
                    } else {
                      const newAction = permissionsModule.TYPE_TO_PERMISSION_ACTION[type]
                      const newPermissions = permissionsModule.FROM_PERMISSION_ACTION[newAction]
                      if ('docs' in permission && 'docs' in newPermissions) {
                        setAction(permissionsModule.toPermissionAction({ ...permission, type }))
                      } else {
                        setAction(permissionsModule.TYPE_TO_PERMISSION_ACTION[type])
                      }
                    }
                  }}
                />
              </Modal>
            )
          }
    )
  }

  let permissionDisplay: JSX.Element

  switch (permission.type) {
    case permissionsModule.Permission.read:
    case permissionsModule.Permission.view: {
      permissionDisplay = (
        <div className="flex w-permission-display gap-px">
          <FocusRing>
            {/* This CANNOT be an `UnstyledButton` as its click handler needs to access
             * `event.currentTarget`.*/}
            {/* eslint-disable-next-line no-restricted-syntax */}
            <button
              type="button"
              disabled={isDisabled}
              {...(isDisabled && error != null ? { title: error } : {})}
              className={`focus-child selectable ${!isDisabled || !input ? 'active' : ''} ${
                permissionsModule.PERMISSION_CLASS_NAME[permission.type]
              } h-text grow rounded-l-full px-permission-mini-button-x py-permission-mini-button-y`}
              onClick={doShowPermissionTypeSelector}
            >
              <aria.Text>{getText(permissionsModule.TYPE_TO_TEXT_ID[permission.type])}</aria.Text>
            </button>
          </FocusRing>
          <UnstyledButton
            isDisabled={isDisabled}
            focusRingPlacement="after"
            {...(isDisabled && error != null ? { title: error } : {})}
            className="relative h-text grow after:absolute after:inset"
            onPress={() => {
              setAction(
                permissionsModule.toPermissionAction({
                  type: permission.type,
                  execute: false,
                  docs: !permission.docs,
                })
              )
            }}
          >
            <aria.Text
              className={`selectable ${permission.docs && (!isDisabled || !input) ? 'active' : ''} ${
                permissionsModule.DOCS_CLASS_NAME
              } h-text grow px-permission-mini-button-x py-permission-mini-button-y`}
            >
              {getText('docsPermissionModifier')}
            </aria.Text>
          </UnstyledButton>
          <UnstyledButton
            isDisabled={isDisabled}
            focusRingPlacement="after"
            {...(isDisabled && error != null ? { title: error } : {})}
            className="relative h-text grow rounded-r-full after:absolute after:inset after:rounded-r-full"
            onPress={() => {
              setAction(
                permissionsModule.toPermissionAction({
                  type: permission.type,
                  execute: !permission.execute,
                  docs: false,
                })
              )
            }}
          >
            <aria.Text
              className={`selectable ${permission.execute && (!isDisabled || !input) ? 'active' : ''} ${
                permissionsModule.EXEC_CLASS_NAME
              } rounded-r-full px-permission-mini-button-x py-permission-mini-button-y`}
            >
              {getText('execPermissionModifier')}
            </aria.Text>
          </UnstyledButton>
        </div>
      )
      break
    }
    default: {
      permissionDisplay = (
        // This CANNOT be an `UnstyledButton` as its click handler needs to access
        // `event.currentTarget`.
        // eslint-disable-next-line no-restricted-syntax
        <button
          type="button"
          disabled={isDisabled}
          {...(isDisabled && error != null ? { title: error } : {})}
          className={`focus-child selectable ${!isDisabled || !input ? 'active' : ''} ${
            permissionsModule.PERMISSION_CLASS_NAME[permission.type]
          } h-text w-permission-display rounded-full`}
          onClick={doShowPermissionTypeSelector}
        >
          {getText(permissionsModule.TYPE_TO_TEXT_ID[permission.type])}
        </button>
      )
      break
    }
  }

  return (
    <div className={className}>
      {permissionDisplay}
      {TheChild && <TheChild />}
    </div>
  )
}
