/** @file Typed version of veaury's `VueContainer` */
import type { VueComponentProps } from '#/utilities/vue'
import type * as react from 'react'
import { VueContainer as VueContainerUntyped } from 'veaury'

/** Propertied of veaury\s `VueContainer` */
type VueContainerProps<V> = VueComponentProps<V> & { component: V }

/** Typed version of veaury's `VueContainer` */
export default function VueContainer<V>(
  props: react.PropsWithChildren<VueContainerProps<V>>,
): react.ReactNode {
  // eslint-disable-next-line no-restricted-syntax
  const VueContainerTyped = VueContainerUntyped as unknown as react.ExoticComponent<
    VueContainerProps<V>
  >
  return <VueContainerTyped {...props} />
}
