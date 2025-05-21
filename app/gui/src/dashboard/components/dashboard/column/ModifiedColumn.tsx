/** @file A column displaying the time at which the asset was last modified. */
import { Text } from '#/components/AriaComponents'
import type { AssetColumnProps } from '#/components/dashboard/column'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'

/** A column displaying the time at which the asset was last modified. */
export default function ModifiedColumn(props: AssetColumnProps) {
  const { item } = props

  return <Text nowrap>{toReadableIsoString(new Date(item.modifiedAt))}</Text>
}
