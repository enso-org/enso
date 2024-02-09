/** @file A column listing the users with which this asset is shared. */
import * as React from 'react'

import type * as column from '#/components/dashboard/column'

/** A column listing the users with which this asset is shared. */
export default function DocsColumn(props: column.AssetColumnProps) {
  const { item } = props
  return <div className="flex items-center gap-1">{item.item.description}</div>
}
