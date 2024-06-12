/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import Plus2Icon from 'enso-assets/plus2.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'

import AssetEventType from '#/events/AssetEventType'

import Category from '#/layouts/CategorySwitcher/Category'

import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import PermissionDisplay from '#/components/dashboard/PermissionDisplay'

import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'

import * as permissions from '#/utilities/permissions'
import * as uniqueString from '#/utilities/uniqueString'

// ========================
// === SharedWithColumn ===
// ========================

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp
  extends Pick<column.AssetColumnProps['state'], 'category' | 'dispatchAssetEvent' | 'setQuery'> {}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<column.AssetColumnProps, 'item' | 'setItem'> {
  readonly isReadonly?: boolean
  readonly state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
export default function SharedWithColumn(props: SharedWithColumnPropsInternal) {
  const { item, setItem, state, isReadonly = false } = props
  const { category, dispatchAssetEvent, setQuery } = state
  const asset = item.item
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const self = asset.permissions?.find(
    backendModule.isUserPermissionAnd(permission => permission.user.userId === user?.userId)
  )
  const plusButtonRef = React.useRef<HTMLButtonElement>(null)
  const managesThisAsset =
    !isReadonly &&
    category !== Category.trash &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)
  const setAsset = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
      setItem(oldItem =>
        oldItem.with({
          item:
            typeof valueOrUpdater !== 'function' ? valueOrUpdater : valueOrUpdater(oldItem.item),
        })
      )
    },
    [setItem]
  )

  return (
    <div className="group flex items-center gap-column-items">
      {(asset.permissions ?? []).map(other => (
        <PermissionDisplay
          key={backendModule.getAssetPermissionId(other)}
          action={other.permission}
          onPress={event => {
            setQuery(oldQuery =>
              oldQuery.withToggled(
                'owners',
                'negativeOwners',
                backendModule.getAssetPermissionName(other),
                event.shiftKey
              )
            )
          }}
        >
          {backendModule.getAssetPermissionName(other)}
        </PermissionDisplay>
      ))}
      {managesThisAsset && (
        <div className="relative inline-flex shrink-0 transition-all transparent before:absolute before:inset-0 before:rounded-full before:border before:border-primary/10 before:transition-all before:hover:bg-primary/5 group-hover:opacity-100">
          <ariaComponents.Button
            ref={plusButtonRef}
            size="medium"
            variant="outline"
            icon={Plus2Icon}
            className="relative inline-flex min-w-max rounded-full transition-all selectable focus-visible:opacity-100"
            onPress={() => {
              setModal(
                <ManagePermissionsModal
                  key={uniqueString.uniqueString()}
                  item={asset}
                  setItem={setAsset}
                  self={self}
                  eventTarget={plusButtonRef.current}
                  doRemoveSelf={() => {
                    dispatchAssetEvent({
                      type: AssetEventType.removeSelf,
                      id: asset.id,
                    })
                  }}
                />
              )
            }}
          />
        </div>
      )}
    </div>
  )
}
