/**
 * @file
 *
 * InvoicesCell component
 */

import * as React from 'react'

import * as aria from '#/components/aria'

/**
 *
 */
export interface InvoicesCellProps extends aria.CellProps {}

/**
 *
 */
export function InvoicesCell(props: InvoicesCellProps) {
  return <aria.Cell {...props} />
}
