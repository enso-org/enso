/** @file A styled checkbox. */
import * as React from 'react'

import * as aria from '#/components/aria'

/** Props for a {@link Checkbox}. */
export interface CheckboxProps extends Omit<Readonly<aria.CheckboxProps>, 'className'> {}

/** A styled checkbox. */
export default function Checkbox(props: CheckboxProps) {
  return <aria.Checkbox {...props} />
}
