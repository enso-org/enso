/** @file Type-safe `motion` from `framer-motion`. */
import {
  motion as originalMotion,
  type ForwardRefComponent,
  type HTMLMotionProps,
  type MotionProps,
  type SVGMotionProps,
} from 'framer-motion'

import type {
  ComponentType,
  DetailedHTMLFactory,
  ForwardRefExoticComponent,
  PropsWithChildren,
  PropsWithoutRef,
  ReactHTML,
  RefAttributes,
  SVGProps,
} from 'react'

/** The options parameter for {@link motion}. */
interface CustomMotionComponentConfig {
  readonly forwardMotionProps?: boolean
}

/** Get the inner type of a {@link DetailedHTMLFactory}. */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
type UnwrapFactoryElement<F> = F extends DetailedHTMLFactory<any, infer P> ? P : never
/** Get the inner type of a {@link SVGProps}. */
type UnwrapSVGFactoryElement<F> = F extends SVGProps<infer P> ? P : never

export * from 'framer-motion'

/**
 * HTML & SVG components, optimised for use with gestures and animation.
 * These can be used as drop-in replacements for any HTML & SVG component -
 * all CSS & SVG properties are supported.
 */
// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const motion = originalMotion as unknown as (<Props extends object>(
  Component: ComponentType<PropsWithChildren<Props>> | string,
  customMotionComponentConfig?: CustomMotionComponentConfig,
) => ForwardRefExoticComponent<
  PropsWithoutRef<
    Omit<MotionProps & Props, 'children' | 'style'> &
      (Props extends { readonly children?: infer Children } ?
        // `Props` has a key `Children` but it may be optional.
        // Use a homomorphic mapped type (a mapped type with `keyof T` in the key set)
        // to preserve modifiers (optional and readonly).
        {
          [K in keyof Props as K extends 'children' ? K : never]: Children | MotionProps['children']
        }
      : // `Props` has no key `Children`.
        { children?: MotionProps['children'] }) &
      (Props extends { readonly style?: infer Style } ?
        // `Props` has a key `Style` but it may be optional.
        // Use a homomorphic mapped type (a mapped type with `keyof T` in the key set)
        // to preserve modifiers (optional and readonly).
        { [K in keyof Props as K extends 'style' ? K : never]: MotionProps['style'] | Style }
      : // `Props` has no key `Style`.
        { style?: MotionProps['style'] })
  > &
    RefAttributes<HTMLElement | SVGElement>
>) & {
  [K in keyof HTMLElementTagNameMap]: ForwardRefComponent<
    UnwrapFactoryElement<ReactHTML[K]>,
    HTMLMotionProps<K>
  >
} & {
  [K in keyof SVGElementTagNameMap]: ForwardRefComponent<
    UnwrapSVGFactoryElement<JSX.IntrinsicElements[K]>,
    SVGMotionProps<UnwrapSVGFactoryElement<JSX.IntrinsicElements[K]>>
  >
}
