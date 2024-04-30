import * as React from 'react'

import * as aria from '#/components/aria'

/**
 *
 */
export interface InvoicesColumnProps extends aria.ColumnProps {}

/**
 *
 */
export function InvoicesColumn(props: InvoicesColumnProps) {
  return <aria.Column {...props} />
}
