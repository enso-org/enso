/** @file Utilities related to debugging. */
import * as React from 'react'

import * as debugHooks from '#/hooks/debugHooks'

/* eslint-disable no-restricted-properties */

// =============
// === Debug ===
// =============

/** Props for a {@Link Debug}. */
interface DebugProps {
  readonly name?: string
  readonly monitorProps?: boolean
  readonly monitorRender?: boolean
  readonly children: JSX.Element
}

/** A component that adds debugging info to its direct child. */
export default function Debug(props: DebugProps) {
  const { name, monitorProps = false, monitorRender = false, children } = props
  const childPropsRaw: unknown = children.props
  const childProps: object = typeof childPropsRaw === 'object' ? childPropsRaw ?? {} : {}
  const propsValues: unknown[] = Object.values(childProps)
  const typeRaw: unknown = children.type
  const typeName = name ?? (typeof typeRaw === 'function' ? typeRaw.name : String(children.type))
  debugHooks.useMonitorDependencies(
    [children.key, ...propsValues],
    typeName,
    ['key', ...Object.keys(childProps)],
    monitorProps
  )

  const patchedChildProps = Object.fromEntries(
    Object.entries(childProps).map(([key, value]: [string, unknown]) => [
      key,
      typeof value !== 'function'
        ? value
        : (...args: unknown[]) => {
            console.group(`[Debug(${typeName})] Prop '${key}' called with args: [`, ...args, ']')
            const result: unknown = value(...args)
            console.log('Returned', result)
            console.groupEnd()
            return result
          },
    ])
  )

  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const Component = children.type
  if (monitorRender) {
    console.group(`[Debug(${typeName})] Rendering`, Component, childProps)
  }
  const element = <Component {...patchedChildProps} />
  React.useEffect(() => {
    if (monitorRender) {
      console.log(`[Debug(${typeName})] Finished rendering`)
      console.groupEnd()
    }
  })
  return element
}
