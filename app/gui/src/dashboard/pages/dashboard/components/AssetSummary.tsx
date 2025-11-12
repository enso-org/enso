/** @file Displays a few details of an asset. */
import BreadcrumbArrowIcon from '#/assets/breadcrumb_arrow.svg'
import { Text } from '#/components/Text'
import AssetIcon from '#/pages/dashboard/components/AssetIcon'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import type * as backend from 'enso-common/src/services/Backend'
import * as dateTime from 'enso-common/src/utilities/data/dateTime'
import { Badge } from '../../../components/Badge'
import { Icon } from '../../../components/Icon'

/** Props for an {@link AssetSummary}. */
export interface AssetSummaryProps {
  readonly asset: backend.AnyAsset
  readonly new?: boolean
  readonly newName?: string
  readonly className?: string
}

/** Displays a few details of an asset. */
export default function AssetSummary(props: AssetSummaryProps) {
  const { asset, new: isNew = false, newName, className } = props

  const { getText } = useText()

  return (
    <div
      className={tailwindMerge.twMerge(
        'flex items-center gap-3 rounded-4xl bg-frame px-4 py-1.5',
        className,
      )}
    >
      <div className="grid size-4 place-items-center">
        <AssetIcon asset={asset} />
      </div>

      <div className="flex min-w-0 flex-col">
        <div className="flex items-center gap-1">
          <Text variant="subtitle" nowrap truncate>
            {asset.title}

            {newName != null && (
              <>
                <img src={BreadcrumbArrowIcon} />
                {newName}
              </>
            )}
          </Text>

          {isNew && (
            <Badge variant="outline" color="primary" className="flex-none">
              {getText('new')}
            </Badge>
          )}

          {!isNew && (
            <Badge variant="outline" color="primary" className="flex-none">
              {getText('existing')}
            </Badge>
          )}
        </div>

        <span className="flex items-center gap-1">
          <Icon icon="calendar" size="small" />

          <Text variant="body-sm" truncate>
            {getText('lastModifiedOn', dateTime.toReadableIsoString(new Date(asset.modifiedAt)))}
          </Text>
        </span>
      </div>
    </div>
  )
}
