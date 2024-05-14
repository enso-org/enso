/** @file Barrel re-export of `react-aria` and `react-aria-components`. */
import * as React from 'react'

import * as aria from 'react-aria'
import * as ariaComponents from 'react-aria-components'

import * as string from '#/utilities/string'

export type * from '@react-types/shared'
export * from 'react-aria'
// @ts-expect-error The conflicting exports are props types ONLY.
export * from 'react-aria-components'

/** Merges multiple props objects together.
 * Event handlers are chained, classNames are combined, and ids are deduplicated -
 * different ids will trigger a side-effect and re-render components hooked up with `useId`.
 * For all other props, the last prop object overrides all previous ones.
 *
 * The constraint is defaulted to `never` to make an explicit constraint mandatory. */
export function mergeProps<Constraint extends object = never>() {
  // eslint-disable-next-line no-restricted-syntax
  return <T extends (Partial<Constraint> | null | undefined)[]>(...args: T) =>
    aria.mergeProps(...args)
}

/** Display text. */
export function Text(props: ariaComponents.TextProps) {
  const { children, ...textProps } = props
  const childrenToDisplayGlyphs = (child: React.ReactNode): React.ReactNode => {
    if (typeof child === 'string') {
      return string.displayGlyphs(child)
    } else if (typeof child === 'object' && child != null && Symbol.iterator in child) {
      return Array.from(child, childrenToDisplayGlyphs)
    } else {
      return child
    }
  }
  return (
    <ariaComponents.Text {...textProps}>{childrenToDisplayGlyphs(children)}</ariaComponents.Text>
  )
}
