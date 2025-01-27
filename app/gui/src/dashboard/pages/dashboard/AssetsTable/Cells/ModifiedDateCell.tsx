/** @file Modified date cell */

import { Text } from '#/components/AriaComponents'
import { formatDateTime } from 'enso-common/src/utilities/data/dateTime'
import type { AnyCellProps } from './types'

/**
 * Props for {@link ModifiedDateCell}
 */
export interface ModifiedDateCellProps extends AnyCellProps {}

/**
 * Modified date cell
 */
export function ModifiedDateCell(props: ModifiedDateCellProps) {
  const { state } = props

  return <Text>{formatDateTime(new Date(state.row.original.item.modifiedAt))} </Text>
}
