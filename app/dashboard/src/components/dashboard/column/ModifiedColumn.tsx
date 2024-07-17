/** @file A column displaying the time at which the asset was last modified. */
import * as React from 'react'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'

import * as dateTime from '#/utilities/dateTime'

// ======================
// === ModifiedColumn ===
// ======================

/** A column displaying the time at which the asset was last modified. */
export default function ModifiedColumn(props: column.AssetColumnProps) {
  return <aria.Text>{dateTime.formatDateTime(new Date(props.item.item.modifiedAt))}</aria.Text>
}
