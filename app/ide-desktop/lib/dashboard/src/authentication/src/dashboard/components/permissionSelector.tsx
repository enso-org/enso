/** @file A selector for all possible permissions. */
import * as React from 'react'

import * as backend from '../backend'
import * as permissionsModule from '../permissions'

import Modal from './modal'
import PermissionTypeSelector from './permissionTypeSelector'

// =================
// === Constants ===
// =================

/** The horizontal offset of the {@link PermissionTypeSelector} from its parent element. */
const TYPE_SELECTOR_X_OFFSET_PX = -8
/** The vertical offset of the {@link PermissionTypeSelector} from its parent element. */
const TYPE_SELECTOR_Y_OFFSET_PX = 28
/** The vertical offset of the label's clip path from its parent element. */
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
    showDelete?: boolean
    /** When true, the button is not clickable. */
    disabled?: boolean
    /** Overrides the vertical offset of the {@link PermissionTypeSelector}. */
    typeSelectorYOffsetPx?: number
    error?: string | null
    selfPermission: backend.PermissionAction
    /** If this prop changes, the internal state will be updated too. */
    action: backend.PermissionAction
    assetType: backend.AssetType
    className?: string
    onChange: (action: backend.PermissionAction) => void
    doDelete?: () => void
}

/** A horizontal selector for all possible permissions. */
export default function PermissionSelector(props: PermissionSelectorProps) {
    const {
        showDelete = false,
        disabled = false,
        typeSelectorYOffsetPx,
        error,
        selfPermission,
        action: actionRaw,
        assetType,
        className,
        onChange,
        doDelete,
    } = props
    const [action, setActionRaw] = React.useState(actionRaw)
    const [TheChild, setTheChild] = React.useState<(() => JSX.Element) | null>()
    const permission = permissionsModule.FROM_PERMISSION_ACTION[action]

    const setAction = (newAction: backend.PermissionAction) => {
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
                              className="fixed w-full h-full z-1"
                              onClick={() => {
                                  setTheChild(null)
                              }}
                          >
                              <div style={{ clipPath }} className="absolute bg-dim w-full h-full" />
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
                                          setAction(
                                              permissionsModule.TYPE_TO_PERMISSION_ACTION[type]
                                          )
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
                <div className="flex gap-px w-30.25">
                    <button
                        type="button"
                        disabled={disabled}
                        {...(disabled && error != null ? { title: error } : {})}
                        className={`${
                            permissionsModule.PERMISSION_CLASS_NAME[permission.type]
                        } grow rounded-l-full h-6 px-1.75 py-0.5 disabled:opacity-30`}
                        onClick={doShowPermissionTypeSelector}
                    >
                        {permission.type}
                    </button>
                    <button
                        type="button"
                        disabled={disabled}
                        {...(disabled && error != null ? { title: error } : {})}
                        className={`${
                            permissionsModule.DOCS_CLASS_NAME
                        } grow h-6 px-1.75 py-0.5 disabled:opacity-30 ${
                            permission.docs ? '' : 'opacity-30'
                        }`}
                        onClick={event => {
                            event.stopPropagation()
                            setAction(
                                permissionsModule.toPermissionAction({
                                    type: permission.type,
                                    execute: false,
                                    docs: !permission.docs,
                                })
                            )
                        }}
                    >
                        docs
                    </button>
                    <button
                        type="button"
                        disabled={disabled}
                        {...(disabled && error != null ? { title: error } : {})}
                        className={`${
                            permissionsModule.EXEC_CLASS_NAME
                        } grow rounded-r-full h-6 px-1.75 py-0.5 disabled:opacity-30 ${
                            permission.execute ? '' : 'opacity-30'
                        }`}
                        onClick={event => {
                            event.stopPropagation()
                            setAction(
                                permissionsModule.toPermissionAction({
                                    type: permission.type,
                                    execute: !permission.execute,
                                    docs: false,
                                })
                            )
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
                    type="button"
                    disabled={disabled}
                    {...(disabled && error != null ? { title: error } : {})}
                    className={`${
                        permissionsModule.PERMISSION_CLASS_NAME[permission.type]
                    } rounded-full h-6 w-30.25 disabled:opacity-30`}
                    onClick={doShowPermissionTypeSelector}
                >
                    {permission.type}
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
