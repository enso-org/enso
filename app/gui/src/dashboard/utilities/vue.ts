/** @file Helpers for using Vue components from React. */
import * as React from 'react'

import { applyPureVueInReact } from 'veaury'
import type { AllowedComponentProps, VNodeProps } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

/** Extracts the properties defined by a component, excluding various Vue internals. */
type VueComponentProps<T> = Omit<ComponentProps<T>, keyof AllowedComponentProps | keyof VNodeProps>

/**
 * Creates a lazy React component wrapping a Vue component.
 *
 * This adds type information and lazy-loading to {@link applyPureVueInReact}.
 */
export function lazyVueComponent<T extends { default?: unknown }>(
  lazyImport: () => Promise<T>,
): React.LazyExoticComponent<(props: VueComponentProps<T['default']>) => React.JSX.Element> {
  return React.lazy(async () => {
    const { default: component } = await lazyImport()
    return {
      // applyPureVueInReact returns Function, but this is not enough to satisfy TSX.
      // eslint-disable-next-line no-restricted-syntax
      default: applyPureVueInReact(component) as (props: unknown) => React.JSX.Element,
    }
  })
}
