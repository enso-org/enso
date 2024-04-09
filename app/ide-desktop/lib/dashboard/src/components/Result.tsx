/**
 * @file A component for displaying the result of an operation.
 */
import * as React from 'react'

import * as aria from './aria'

/**
 * The props for the Result component.
 */
export interface ResultProps extends React.PropsWithChildren {
  readonly title?: React.JSX.Element | string
}

/**
 * A component for displaying the result of an operation.
 */
export function Result(props: ResultProps) {
  const { title, children } = props
  return <section>{typeof title === 'string' ? <h1>{title}</h1> : title}</section>
}
