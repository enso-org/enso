/** @file A column displaying the time at which the asset was last modified. */
import { Text } from '#/components/aria'
import type { AssetColumnProps } from '#/components/dashboard/column'
import { formatDateTime } from '#/utilities/dateTime'

/** A column displaying the time at which the asset was last modified. */
export default function ModifiedColumn(props: AssetColumnProps) {
  const { item } = props

  return <Text>{formatDateTime(new Date(item.modifiedAt))}</Text>
}
