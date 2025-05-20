/** @file Module augmentations. */
import type { ReactNode, Ref, RefAttributes } from 'react'

declare module 'react' {
  function forwardRef<T, P = object>(
    render: (props: P, ref: Ref<T>) => ReactNode | null,
  ): (props: P & RefAttributes<T>) => ReactNode | null
}
