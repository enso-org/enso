/** @file Helpers for using Vue components from React. */
import type { PropsWithChildren } from 'react'
import { applyPureVueInReact } from 'veaury'
import type { AllowedComponentProps, VNodeProps } from 'vue'
import type { ComponentProps, ComponentSlots } from 'vue-component-type-helpers'

/** Extracts the properties defined by a component, excluding various Vue internals. */
export type VueComponentProps<T> = Omit<
  ComponentProps<T>,
  keyof AllowedComponentProps | keyof VNodeProps
>

/** Specify what are React properties of wrapped Vue component. */
export type VueInReactProps<T> =
  ComponentSlots<T> extends { default?: unknown } ? PropsWithChildren<VueComponentProps<T>>
  : VueComponentProps<T>

/**
 * Creates a React component wrapping a Vue component.
 *
 * This adds type information to {@link applyPureVueInReact}.
 */
export function vueComponent<T>(vue: T): { default: React.ComponentType<VueInReactProps<T>> } {
  // applyPureVueInReact returns Function, but this is not enough to satisfy TSX.
  // eslint-disable-next-line no-restricted-syntax
  return { default: applyPureVueInReact(vue) as never }
}
