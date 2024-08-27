/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import Plus2Icon from '#/assets/plus2.svg'

import * as billingHooks from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'

import AssetEventType from '#/events/AssetEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'

import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import PermissionDisplay from '#/components/dashboard/PermissionDisplay'
import * as paywall from '#/components/Paywall'

import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'

import * as permissions from '#/utilities/permissions'
import * as uniqueString from '#/utilities/uniqueString'

// ========================
// === SharedWithColumn ===
// ========================

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp extends Pick<column.AssetColumnProps['state'], 'category'> {
  readonly setQuery: column.AssetColumnProps['state']['setQuery'] | null
}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<column.AssetColumnProps, 'item' | 'setItem'> {
  readonly isReadonly?: boolean
  readonly state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
export default function SharedWithColumn(props: SharedWithColumnPropsInternal) {
  const { item, setItem, state, isReadonly = false } = props
  const { category, setQuery } = state
  const asset = item.item
  const { user } = authProvider.useFullUserSession()
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })
  const isUnderPaywall = isFeatureUnderPaywall('share')
  const { setModal } = modalProvider.useSetModal()
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const plusButtonRef = React.useRef<HTMLButtonElement>(null)
  const managesThisAsset =
    !isReadonly &&
    category.type !== categoryModule.CategoryType.trash &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)
  const setAsset = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
      setItem((oldItem) =>
        oldItem.with({
          item:
            typeof valueOrUpdater !== 'function' ? valueOrUpdater : valueOrUpdater(oldItem.item),
        }),
      )
    },
    [setItem],
  )

  return (
    <div className="group flex items-center gap-column-items">
      {(asset.permissions ?? []).map((other) => (
        <PermissionDisplay
          key={backendModule.getAssetPermissionId(other)}
          action={other.permission}
          onPress={
            setQuery == null ? null : (
              (event) => {
                setQuery((oldQuery) =>
                  oldQuery.withToggled(
                    'owners',
                    'negativeOwners',
                    backendModule.getAssetPermissionName(other),
                    event.shiftKey,
                  ),
                )
              }
            )
          }
        >
          {backendModule.getAssetPermissionName(other)}
        </PermissionDisplay>
      ))}
      {isUnderPaywall && (
        <paywall.PaywallDialogButton
          feature="share"
          variant="icon"
          size="medium"
          className="opacity-0 group-hover:opacity-100"
          children={false}
        />
      )}
      {managesThisAsset && !isUnderPaywall && (
        <ariaComponents.Button
          ref={plusButtonRef}
          size="medium"
          variant="ghost"
          icon={Plus2Icon}
          showIconOnHover
          onPress={() => {
            setModal(
              <ManagePermissionsModal
                key={uniqueString.uniqueString()}
                item={asset}
                setItem={setAsset}
                self={self}
                eventTarget={plusButtonRef.current}
                doRemoveSelf={() => {
                  dispatchAssetEvent({ type: AssetEventType.removeSelf, id: asset.id })
                }}
              />,
            )
          }}
        />
      )}
    </div>
  )
}
