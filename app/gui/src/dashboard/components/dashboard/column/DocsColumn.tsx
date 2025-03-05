/** @file A column listing the users with which this asset is shared. */
import type { AssetColumnProps } from '#/components/dashboard/column'

/** A column listing the users with which this asset is shared. */
export default function DocsColumn(props: AssetColumnProps) {
  const { item } = props

  return (
    <div className="flex max-w-drive-docs-column items-center gap-column-items overflow-hidden whitespace-nowrap">
      {item.description}
    </div>
  )
}
