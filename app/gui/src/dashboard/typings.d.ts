/** @file Module augmentations. */
import type { ReactNode, Ref, RefAttributes } from 'react'

declare module 'react' {
  function forwardRef<T, P = object>(
    render: (props: P, ref: Ref<T>) => ReactNode,
  ): (props: P & RefAttributes<T>) => ReactNode

  // Technically incorrect, as internally it is a `MemoExoticComponent`,
  // however in practice it should be indistinguishable from the original component.
  function memo<P = object>(
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Component: (props: P) => ReactNode,
    propsAreEqual?: (prevProps: Readonly<P>, nextProps: Readonly<P>) => boolean,
  ): (props: P) => ReactNode
}
