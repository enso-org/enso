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
  return (
    <aria.Cell
      className="border-x-2 border-transparent bg-clip-padding px-cell-x py-1 first:rounded-l-full last:rounded-r-full last:border-r-0"
      {...props}
    />
  )
}
