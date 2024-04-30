import * as React from 'react'

import * as aria from '#/components/aria'

/**
 *
 */
export interface InvoicesRowProps<T> extends aria.RowProps<T> {}

/**
 *
 */
export function InvoicesRow<T extends object>(props: InvoicesRowProps<T>) {
  return <aria.Row<T> {...props} />
}
