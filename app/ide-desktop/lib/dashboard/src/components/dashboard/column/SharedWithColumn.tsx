/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import Plus2Icon from 'enso-assets/plus2.svg'

import AssetEventType from '#/events/AssetEventType'
import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import ManagePermissionsModal from '#/layouts/dashboard/ManagePermissionsModal'
import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import type * as backendModule from '#/services/backend'
import * as object from '#/utilities/object'
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
  const { item, setItem, state } = props
  const { category, dispatchAssetEvent } = state
  const asset = item.item
  const session = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const self = asset.permissions?.find(
    permission => permission.user.user_email === session.organization?.email
  )
  const managesThisAsset =
    category !== Category.trash &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)
  const setAsset = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
      setItem(oldItem =>
        object.merge(oldItem, {
          item:
            typeof valueOrUpdater !== 'function' ? valueOrUpdater : valueOrUpdater(oldItem.item),
        })
      )
    },
    [/* should never change */ setItem]
  )
  return (
    <div className="flex items-center gap-1">
      {(asset.permissions ?? []).map(user => (
        <PermissionDisplay key={user.user.pk} action={user.permission}>
          {user.user.user_name}
        </PermissionDisplay>
      ))}
      {managesThisAsset && (
        <button
          className="h-4 w-4 invisible pointer-events-none group-hover:visible group-hover:pointer-events-auto"
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
                    type: AssetEventType.removeSelf,
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
