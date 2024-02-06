/** @file A column displaying the time at which the asset was last modified. */
import * as React from 'react'

import type * as column from '#/components/dashboard/column'

import * as dateTime from '#/utilities/dateTime'

/** A column displaying the time at which the asset was last modified. */
export default function LastModifiedColumn(props: column.AssetColumnProps) {
  return <>{dateTime.formatDateTime(new Date(props.item.item.modifiedAt))}</>
}
