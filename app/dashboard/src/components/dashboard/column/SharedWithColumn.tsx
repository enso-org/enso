/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import Plus2Icon from '#/assets/plus2.svg'

import * as billingHooks from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'

import AssetEventType from '#/events/AssetEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import PermissionDisplay from '#/components/dashboard/PermissionDisplay'
import * as paywall from '#/components/Paywall'

import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'

import { useAssetPassiveListenerStrict } from '#/hooks/backendHooks'
import * as permissions from '#/utilities/permissions'

// ========================
// === SharedWithColumn ===
// ========================

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp
  extends Pick<column.AssetColumnProps['state'], 'backend' | 'category'> {
  readonly setQuery: column.AssetColumnProps['state']['setQuery'] | null
}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<column.AssetColumnProps, 'item'> {
  readonly isReadonly?: boolean
  readonly state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
export default function SharedWithColumn(props: SharedWithColumnPropsInternal) {
  const { item, state, isReadonly = false } = props
  const { backend, category, setQuery } = state
  const asset = useAssetPassiveListenerStrict(backend.type, item.item.id, item.item.parentId)
  const { user } = authProvider.useFullUserSession()
  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const { isFeatureUnderPaywall } = billingHooks.usePaywall({ plan: user.plan })
  const isUnderPaywall = isFeatureUnderPaywall('share')
  const assetPermissions = asset.permissions ?? []
  const { setModal } = modalProvider.useSetModal()
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const plusButtonRef = React.useRef<HTMLButtonElement>(null)
  const managesThisAsset =
    !isReadonly &&
    category.type !== 'trash' &&
    (self?.permission === permissions.PermissionAction.own ||
      self?.permission === permissions.PermissionAction.admin)

  return (
    <div className="group flex items-center gap-column-items">
      {(category.type === 'trash' ?
        assetPermissions.filter(
          (permission) => permission.permission === permissions.PermissionAction.own,
        )
      : assetPermissions
      ).map((other, idx) => (
        <PermissionDisplay
          key={backendModule.getAssetPermissionId(other) + idx}
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
                backend={backend}
                item={asset}
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
