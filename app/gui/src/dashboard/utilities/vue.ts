/** @file Helpers for using Vue components from React. */
import type * as React from 'react'

import { applyPureVueInReact } from 'veaury'
import type { AllowedComponentProps, ShallowRef, VNodeProps } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

/** Extracts the properties defined by a component, excluding various Vue internals. */
type VueComponentProps<T> = Omit<ComponentProps<T>, keyof AllowedComponentProps | keyof VNodeProps>

/** A type alias useful when declaring usage of ref values coming from `useTemplateRef` declaration. */
export type TemplateRef<T = HTMLElement> = Readonly<ShallowRef<T | null>>

/**
 * Creates a React component wrapping a Vue component.
 *
 * This adds type information to {@link applyPureVueInReact}.
 */
export function vueComponent<T>(vue: T): { default: React.ComponentType<VueComponentProps<T>> } {
  // applyPureVueInReact returns Function, but this is not enough to satisfy TSX.
  // eslint-disable-next-line no-restricted-syntax
  return { default: applyPureVueInReact(vue) as never }
}
