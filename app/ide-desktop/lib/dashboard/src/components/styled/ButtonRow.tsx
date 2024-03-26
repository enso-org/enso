/** @file A styled horizontal button row. Does not have padding; does not have a background. */
import * as React from 'react'

import FocusArea from '#/components/styled/FocusArea'

/** Props for a {@link ButtonRow}. */
export interface ButtonRowProps extends Readonly<React.PropsWithChildren> {}

/** A styled horizontal button row. Does not have padding; does not have a background. */
export default function ButtonRow(props: ButtonRowProps) {
  const { children } = props

  return (
    <FocusArea direction="horizontal">
      {(ref, innerProps) => (
        <div ref={ref} className="relative flex gap-buttons self-start" {...innerProps}>
          {children}
        </div>
      )}
    </FocusArea>
  )
}
