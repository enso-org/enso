/**
 * @file Activity component
 *
 * This component is used to suspend the rendering of a subtree until a promise is resolved.
 */
import { unsafeWriteValue } from '#/utilities/write'
import { Suspense, useEffect, useLayoutEffect, useRef, useState } from 'react'
import { useAwait } from './Await'

/**
 * Props for {@link Activity}
 */
export interface ActivityProps {
  /**
   * The mode of the activity.
   * - `active`: The subtree is active (default).
   * - `inactive`: The activity of the subtree is paused.
   * - `inactive-hidden`: The activity of the subtree is paused, and the subtree is hidden.
   * @default 'active'
   */
  readonly mode: 'active' | 'inactive-hidden' | 'inactive'
  readonly children: React.ReactNode
}

/**
 * A component that pauses all activity inside it's subtree.
 *
 * ---
 * ## The component is EXPERIMENTAL, please use with caution.
 * ---
 */
export function Activity(props: ActivityProps) {
  const { mode, children } = props

  const contentRef = useRef<HTMLDivElement>(null)

  const [promise, setPromise] = useState<Promise<void> | null>(null)

  const isActive = mode === 'active'
  const fallback =
    mode === 'inactive-hidden' ? null : <UnhideSuspendedTree contentRef={contentRef} />

  useEffect(() => {
    if (isActive) {
      return
    }

    let resolve = () => {}

    setPromise(
      new Promise((res) => {
        resolve = res
      }),
    )

    return () => {
      resolve()
      setPromise(null)
    }
  }, [isActive])

  return (
    <div ref={contentRef} className="contents">
      <Suspense fallback={fallback}>
        <ActivityInner promise={promise}>{children}</ActivityInner>
      </Suspense>
    </div>
  )
}

/**
 * Props for {@link ActivityInner}
 */
interface ActivityInnerProps {
  readonly children: React.ReactNode
  readonly promise?: Promise<unknown> | null | undefined
}

/**
 * A component that suspends the tree using promises.
 * @param props - The props of the component.
 * @returns The children of the component.
 */
function ActivityInner(props: ActivityInnerProps) {
  const { promise, children } = props

  // Suspend the subtree
  useAwait(promise)

  return children
}

/**
 * Props for {@link UnhideSuspendedTree}
 */
interface UnhideSuspendedTreeProps {
  readonly contentRef: React.RefObject<HTMLDivElement>
}

/**
 * Hack, that unhides the suspended tree.
 */
function UnhideSuspendedTree(props: UnhideSuspendedTreeProps) {
  const { contentRef } = props

  useLayoutEffect(() => {
    const element: HTMLDivElement | null = contentRef.current

    if (element == null) {
      return
    }

    const chidlren = element.childNodes

    for (let i = 0; i < chidlren.length; i++) {
      const child = chidlren[i]

      if (child instanceof HTMLElement) {
        unsafeWriteValue(child.style, 'display', '')
      }
    }
  }, [contentRef])

  return null
}
