/** @file A selector for all possible permissions. */
import * as React from 'react'

import * as permissionsModule from '../permissions'
import Modal from './modal'
import PermissionTypeSelector from './permissionTypeSelector'

// =================
// === Constants ===
// =================

/** The horizontal offset of the {@link PermissionTypeSelector} from its parent element. */
const TYPE_SELECTOR_HORIZONTAL_OFFSET_PX = -8
/** The vertical offset of the {@link PermissionTypeSelector} from its parent element. */
const TYPE_SELECTOR_VERTICAL_OFFSET_PX = 28
/** The border radius of the permission label. */
const LABEL_BORDER_RADIUS_PX = 12
/** The width of the straight section of the permission label. */
const LABEL_STRAIGHT_WIDTH_PX = 97

// ==========================
// === PermissionSelector ===
// ==========================

/** Props for a {@link PermissionSelector}. */
export interface PermissionSelectorProps {
    disabled?: boolean
    /** If this prop changes, the internal state will be updated too. */
    initialPermissions?: permissionsModule.Permissions | null
    className?: string
    onChange: (permissions: permissionsModule.Permissions) => void
}

/** A horizontal selector for all possible permissions. */
export default function PermissionSelector(props: PermissionSelectorProps) {
    const { disabled = false, initialPermissions, className, onChange } = props
    const [permissions, rawSetPermissions] = React.useState<permissionsModule.Permissions>(
        initialPermissions ?? permissionsModule.DEFAULT_PERMISSIONS
    )
    const [TheChild, setTheChild] = React.useState<(() => JSX.Element) | null>()

    const setPermissions = (newPermissions: permissionsModule.Permissions) => {
        rawSetPermissions(newPermissions)
        onChange(newPermissions)
    }

    const doShowPermissionTypeSelector = (event: React.SyntheticEvent<HTMLElement>) => {
        const position = event.currentTarget.getBoundingClientRect()
        const originalLeft = position.left + window.scrollX
        const originalTop = position.top + window.scrollY
        const left = originalLeft + TYPE_SELECTOR_HORIZONTAL_OFFSET_PX
        const top = originalTop + TYPE_SELECTOR_VERTICAL_OFFSET_PX
        // The border radius of the label. This is half of the label's height.
        const r = LABEL_BORDER_RADIUS_PX
        const clipPath =
            // A rectangle covering the entire screen
            'path(evenodd, "M0 0L3840 0 3840 2160 0 2160Z' +
            // Move to top left of label
            `M${originalLeft + LABEL_BORDER_RADIUS_PX} ${originalTop}` +
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
                              className="fixed w-full h-full z-10"
                              onClick={() => {
                                  setTheChild(null)
                              }}
                          >
                              <div style={{ clipPath }} className="absolute bg-dim w-full h-full" />
                              <PermissionTypeSelector
                                  type={permissions.type}
                                  style={{ left, top }}
                                  onChange={type => {
                                      setTheChild(null)
                                      let newPermissions: permissionsModule.Permissions
                                      switch (type) {
                                          case permissionsModule.Permission.read:
                                          case permissionsModule.Permission.view: {
                                              newPermissions = {
                                                  type,
                                                  docs: false,
                                                  execute: false,
                                              }
                                              break
                                          }
                                          default: {
                                              newPermissions = { type }
                                              break
                                          }
                                      }
                                      setPermissions(newPermissions)
                                  }}
                              />
                          </Modal>
                      )
                  }
        )
    }

    let permissionDisplay: JSX.Element

    switch (permissions.type) {
        case permissionsModule.Permission.read:
        case permissionsModule.Permission.view: {
            permissionDisplay = (
                <div className="flex gap-px w-30.25">
                    <button
                        disabled={disabled}
                        className={`${
                            permissionsModule.PERMISSION_CLASS_NAME[permissions.type]
                        } grow rounded-l-full h-6 px-1.75 py-0.5 disabled:opacity-30`}
                        onClick={doShowPermissionTypeSelector}
                    >
                        {permissions.type}
                    </button>
                    <button
                        disabled={disabled}
                        className={`${
                            permissionsModule.DOCS_CLASS_NAME
                        } grow h-6 px-1.75 py-0.5 disabled:opacity-30 ${
                            permissions.docs ? '' : 'opacity-30'
                        }`}
                        onClick={event => {
                            event.stopPropagation()
                            setPermissions({ ...permissions, docs: !permissions.docs })
                        }}
                    >
                        docs
                    </button>
                    <button
                        disabled={disabled}
                        className={`${
                            permissionsModule.EXEC_CLASS_NAME
                        } grow rounded-r-full h-6 px-1.75 py-0.5 disabled:opacity-30 ${
                            permissions.execute ? '' : 'opacity-30'
                        }`}
                        onClick={event => {
                            event.stopPropagation()
                            setPermissions({ ...permissions, execute: !permissions.execute })
                        }}
                    >
                        exec
                    </button>
                </div>
            )
            break
        }
        default: {
            permissionDisplay = (
                <button
                    disabled={disabled}
                    className={`${
                        permissionsModule.PERMISSION_CLASS_NAME[permissions.type]
                    } rounded-full h-6 w-30.25 disabled:opacity-30`}
                    onClick={doShowPermissionTypeSelector}
                >
                    {permissions.type}
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
