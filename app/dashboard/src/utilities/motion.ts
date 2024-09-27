import {
  CustomDomComponent,
  ForwardRefComponent,
  HTMLMotionProps,
  MotionProps,
  MotionValue,
  SVGMotionProps,
  motion as originalMotion,
} from 'framer-motion'
import {
  ComponentType,
  DetailedHTMLFactory,
  ForwardRefExoticComponent,
  PropsWithChildren,
  PropsWithoutRef,
  ReactHTML,
  RefAttributes,
} from 'react'

interface CustomMotionComponentConfig {
  forwardMotionProps?: boolean
}

type UnwrapFactoryElement<F> = F extends DetailedHTMLFactory<any, infer P> ? P : never
type UnwrapSVGFactoryElement<F> = F extends React.SVGProps<infer P> ? P : never

export const motion = originalMotion as unknown as (<Props extends {}>(
  Component: string | ComponentType<PropsWithChildren<Props>>,
  customMotionComponentConfig?: CustomMotionComponentConfig,
) => ForwardRefExoticComponent<
  PropsWithoutRef<
    Omit<Props & MotionProps, 'children' | 'style'> &
      (Props extends { readonly style?: infer Style } ?
        // `Props` has a key `Style` but it may be optional.
        // Use a homomorphic mapped type (a mapped type with `keyof T` in the key set)
        // to preserve modifiers (optional and readonly).
        { [K in keyof Props as K extends 'style' ? K : never]: Style | MotionProps['style'] }
      : // `Props` has no key `Style`.
        { style?: MotionProps['style'] }) &
      (Props extends { readonly children?: infer Children } ?
        // `Props` has a key `Children` but it may be optional.
        // Use a homomorphic mapped type (a mapped type with `keyof T` in the key set)
        // to preserve modifiers (optional and readonly).
        {
          [K in keyof Props as K extends 'children' ? K : never]: Children | MotionProps['children']
        }
      : // `Props` has no key `Children`.
        { children?: MotionProps['children'] })
  > &
    RefAttributes<SVGElement | HTMLElement>
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
