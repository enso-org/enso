/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import Plus2Icon from '#/assets/plus2.svg'
import { Button } from '#/components/AriaComponents'
import type { AssetColumnProps } from '#/components/dashboard/column'
import PermissionDisplay from '#/components/dashboard/PermissionDisplay'
import { PaywallDialogButton } from '#/components/Paywall'
import AssetEventType from '#/events/AssetEventType'
import { useAssetPassiveListenerStrict } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useDispatchAssetEvent } from '#/layouts/AssetsTable/EventListProvider'
import ManagePermissionsModal from '#/modals/ManagePermissionsModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { getAssetPermissionId, getAssetPermissionName } from '#/services/Backend'
import { PermissionAction, tryFindSelfPermission } from '#/utilities/permissions'

// ========================
// === SharedWithColumn ===
// ========================

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp
  extends Pick<AssetColumnProps['state'], 'backend' | 'category'> {
  readonly setQuery: AssetColumnProps['state']['setQuery'] | null
}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<AssetColumnProps, 'item'> {
  readonly isReadonly?: boolean
  readonly state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
export default function SharedWithColumn(props: SharedWithColumnPropsInternal) {
  const { item, state, isReadonly = false } = props
  const { backend, category, setQuery } = state
  const asset = useAssetPassiveListenerStrict(
    backend.type,
    item.item.id,
    item.item.parentId,
    category,
  )
  const { user } = useFullUserSession()
  const dispatchAssetEvent = useDispatchAssetEvent()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })
  const isUnderPaywall = isFeatureUnderPaywall('share')
  const assetPermissions = asset.permissions ?? []
  const { setModal } = useSetModal()
  const self = tryFindSelfPermission(user, asset.permissions)
  const plusButtonRef = React.useRef<HTMLButtonElement>(null)
  const managesThisAsset =
    !isReadonly &&
    category.type !== 'trash' &&
    (self?.permission === PermissionAction.own || self?.permission === PermissionAction.admin)

  return (
    <div className="group flex items-center gap-column-items">
      {(category.type === 'trash' ?
        assetPermissions.filter((permission) => permission.permission === PermissionAction.own)
      : assetPermissions
      ).map((other, idx) => (
        <PermissionDisplay
          key={getAssetPermissionId(other) + idx}
          action={other.permission}
          onPress={
            setQuery == null ? null : (
              (event) => {
                setQuery((oldQuery) =>
                  oldQuery.withToggled(
                    'owners',
                    'negativeOwners',
                    getAssetPermissionName(other),
                    event.shiftKey,
                  ),
                )
              }
            )
          }
        >
          {getAssetPermissionName(other)}
        </PermissionDisplay>
      ))}
      {isUnderPaywall && (
        <PaywallDialogButton
          feature="share"
          variant="icon"
          size="medium"
          className="opacity-0 group-hover:opacity-100"
          children={false}
        />
      )}
      {managesThisAsset && !isUnderPaywall && (
        <Button
          ref={plusButtonRef}
          size="medium"
          variant="ghost"
          icon={Plus2Icon}
          showIconOnHover
          onPress={() => {
            setModal(
              <ManagePermissionsModal
                backend={backend}
                category={category}
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
