/** @file A column listing the users with which this asset is shared. */
import type { AssetColumnProps } from '#/components/dashboard/column'
import PermissionDisplay from '#/components/dashboard/PermissionDisplay'
import { getAssetPermissionId, getAssetPermissionName } from '#/services/Backend'
import { PermissionAction } from '#/utilities/permissions'

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
  const { item, state } = props
  const { category, setQuery } = state

  const assetPermissions = item.permissions ?? []

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
    </div>
  )
}
