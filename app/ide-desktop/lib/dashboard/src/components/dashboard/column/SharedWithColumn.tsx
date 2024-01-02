/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import Plus2Icon from 'enso-assets/plus2.svg'

import * as assetEvent from '#/events/assetEvent'
import * as categorySwitcherUtils from '#/layouts/dashboard/CategorySwitcher/categorySwitcherUtils'
import ManagePermissionsModal from '#/layouts/dashboard/ManagePermissionsModal'
import * as authProvider from '#/providers/authProvider'
import * as modalProvider from '#/providers/modalProvider'
import type * as backendModule from '#/services/backend'
import * as permissions from '#/utilities/permissions'
import * as uniqueString from '#/utilities/uniqueString'

import type * as column from '#/components/dashboard/column'
import PermissionDisplay from '#/components/dashboard/PermissionDisplay'

// ========================
// === SharedWithColumn ===
// ========================

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp {
    category: column.AssetColumnProps['state']['category']
    dispatchAssetEvent: column.AssetColumnProps['state']['dispatchAssetEvent']
}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<column.AssetColumnProps, 'item' | 'setItem'> {
    state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
export default function SharedWithColumn(props: SharedWithColumnPropsInternal) {
    const {
        item: { item: asset },
        setItem,
        state: { category, dispatchAssetEvent },
    } = props
    const session = authProvider.useNonPartialUserSession()
    const { setModal } = modalProvider.useSetModal()
    const [isHovered, setIsHovered] = React.useState(false)
    const self = asset.permissions?.find(
        permission => permission.user.user_email === session.organization?.email
    )
    const managesThisAsset =
        category !== categorySwitcherUtils.Category.trash &&
        (self?.permission === permissions.PermissionAction.own ||
            self?.permission === permissions.PermissionAction.admin)
    const setAsset = React.useCallback(
        (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
            if (typeof valueOrUpdater === 'function') {
                setItem(oldItem => ({
                    ...oldItem,
                    item: valueOrUpdater(oldItem.item),
                }))
            } else {
                setItem(oldItem => ({ ...oldItem, item: valueOrUpdater }))
            }
        },
        [/* should never change */ setItem]
    )
    return (
        <div
            className="flex items-center gap-1"
            onMouseEnter={() => {
                setIsHovered(true)
            }}
            onMouseLeave={() => {
                setIsHovered(false)
            }}
        >
            {(asset.permissions ?? []).map(user => (
                <PermissionDisplay key={user.user.pk} action={user.permission}>
                    {user.user.user_name}
                </PermissionDisplay>
            ))}
            {managesThisAsset && (
                <button
                    className={`h-4 w-4 ${isHovered ? '' : 'invisible pointer-events-none'}`}
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <ManagePermissionsModal
                                key={uniqueString.uniqueString()}
                                item={asset}
                                setItem={setAsset}
                                self={self}
                                eventTarget={event.currentTarget}
                                doRemoveSelf={() => {
                                    dispatchAssetEvent({
                                        type: assetEvent.AssetEventType.removeSelf,
                                        id: asset.id,
                                    })
                                }}
                            />
                        )
                    }}
                >
                    <img className="w-4.5 h-4.5" src={Plus2Icon} />
                </button>
            )}
        </div>
    )
}
