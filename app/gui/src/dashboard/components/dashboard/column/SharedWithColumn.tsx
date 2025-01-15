/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import Plus2Icon from '#/assets/plus2.svg'
import { Button } from '#/components/AriaComponents'
import type { AssetColumnProps } from '#/components/dashboard/column'
import PermissionDisplay from '#/components/dashboard/PermissionDisplay'
import { useRemoveSelfPermissionMutation } from '#/hooks/backendHooks'
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

  const { user } = useFullUserSession()

  const removeSelfPermissionMutation = useRemoveSelfPermissionMutation(backend)

  const assetPermissions = item.permissions ?? []
  const { setModal } = useSetModal()
  const self = tryFindSelfPermission(user, item.permissions)
  const plusButtonRef = React.useRef<HTMLButtonElement>(null)
  const managesThisAsset =
    !isReadonly &&
    category.type !== 'trash' &&
    (self?.permission === PermissionAction.own || self?.permission === PermissionAction.admin)

  return (
    <div className="group flex items-center gap-column-items [content-visibility:auto]">
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
      {managesThisAsset && (
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
                item={item}
                self={self}
                eventTarget={plusButtonRef.current}
                doRemoveSelf={() => {
                  removeSelfPermissionMutation.mutate(item.id)
                }}
              />,
            )
          }}
        />
      )}
    </div>
  )
}
