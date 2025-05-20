/** @file Module augmentations. */
import type { ReactNode, Ref, RefAttributes } from 'react'

declare module 'react' {
  function forwardRef<T, P = object>(
    render: (props: P, ref: Ref<T>) => ReactNode | null,
  ): (props: P & RefAttributes<T>) => ReactNode | null

  // Technically incorrect, as internally it is a `MemoExoticComponent`,
  // however in practice it should be indistinguishable from the original component.
  function memo<P = object>(
    Component: (props: P) => ReactNode | null,
    propsAreEqual?: (prevProps: Readonly<P>, nextProps: Readonly<P>) => boolean,
  ): (props: P) => ReactNode | null
}
