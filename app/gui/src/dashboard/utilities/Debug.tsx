/** @file Utilities related to debugging. */
import * as React from 'react'

import * as debugHooks from '#/hooks/debugHooks'

/* eslint-disable no-restricted-properties */

// =============
// === Debug ===
// =============

let nextMountId = 0
let nextRenderId = 0

/** Props for a {@Link Debug}. */
interface DebugProps {
  readonly name?: string
  readonly monitorAll?: boolean
  readonly monitorMountUnmount?: boolean
  readonly monitorRender?: boolean
  readonly monitorProps?: boolean
  readonly monitorPropCalls?: boolean
  readonly children: JSX.Element
}

/** A component that adds debugging info to its direct child. */
export default function Debug(props: DebugProps) {
  const {
    name,
    monitorAll = false,
    monitorMountUnmount = monitorAll,
    monitorRender = monitorAll,
    monitorProps = monitorAll,
    monitorPropCalls = monitorAll,
    children,
  } = props
  const childPropsRaw: unknown = children.props
  const childProps: object = typeof childPropsRaw === 'object' ? childPropsRaw ?? {} : {}
  const propsValues: unknown[] = Object.values(childProps)
  const typeRaw: unknown = children.type
  const typeName =
    name ??
    (typeof typeRaw === 'function' ? typeRaw.name
    : typeof typeRaw === 'object' && typeRaw != null && '$$typeof' in typeRaw ?
      String(typeRaw.$$typeof)
    : String(children.type))
  const typeNameRef = React.useRef(typeName)
  typeNameRef.current = typeName
  const monitorMountUnmountRef = React.useRef(monitorMountUnmount)
  monitorMountUnmountRef.current = monitorMountUnmount
  const [mountId] = React.useState(() => `(Mount #${nextMountId++})`)
  const renderId = `(Render #${nextRenderId++})`

  React.useEffect(() => {
    if (monitorMountUnmountRef.current) {
      console.log(`[Debug(${typeNameRef.current})] Mounted ${mountId}`)
    }
    return () => {
      if (monitorMountUnmountRef.current) {
        console.log(`[Debug(${typeNameRef.current})] Unmounted ${mountId}`)
      }
    }
  }, [mountId])

  debugHooks.useMonitorDependencies(
    [children.key, ...propsValues],
    typeName,
    ['key', ...Object.keys(childProps)],
    monitorProps,
  )

  const patchedChildProps =
    !monitorPropCalls ? childProps : (
      Object.fromEntries(
        Object.entries(childProps).map(([key, value]: [string, unknown]) => [
          key,
          typeof value !== 'function' ? value : (
            (...args: unknown[]) => {
              console.group(`[Debug(${typeName})] Prop '${key}' called with args: [`, ...args, ']')
              // eslint-disable-next-line @typescript-eslint/no-unsafe-call
              const result: unknown = value(...args)
              console.log('Returned', result)
              console.groupEnd()
              return result
            }
          ),
        ]),
      )
    )

  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const Component = children.type
  if (monitorRender) {
    console.group(`[Debug(${typeName})] Rendering ${renderId}`, Component, childProps)
  }
  const element = <Component {...patchedChildProps} />
  React.useEffect(() => {
    if (monitorRender) {
      console.log(`[Debug(${typeName})] Finished rendering ${renderId}`)
      console.groupEnd()
    }
  })
  return element
}
