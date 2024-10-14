/** @file Helper functions related to React. */
import { type ForwardRefRenderFunction, forwardRef as reactForwardRef, type ReactNode } from 'react'

// This is *technically* not correct as it has the extra parameter `ref`.
// However, it is not possible to specify a second parameter in JSX so this is not an issue in practice.
/** A type-safe wrapper around {@link reactForwardRef} that preserves generics. */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function forwardRef<F extends ForwardRefRenderFunction<any, any>>(
  component: F,
  // A union of functions becomes a single function with parameters that are the intersection
  // of their types due to function parmeter contravariance.
): F | ((props: { readonly ref?: Parameters<F>[1] }) => ReactNode) {
  // This is SAFE as
  // eslint-disable-next-line no-restricted-syntax
  return reactForwardRef(component) as never
}
