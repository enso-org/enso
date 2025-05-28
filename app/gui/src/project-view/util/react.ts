import { Suspense } from '#/components/Suspense'
import type { ReactNode } from 'react'
import { applyPureReactInVue } from 'veaury'
import type { DefineComponent } from 'vue'

/**
 * Creates a Vue component wrapping a React component.
 *
 * This adds type information to {@link applyPureReactInVue}.
 */
export function reactComponent<Props extends object>(
  react: (props: Props) => unknown,
): DefineComponent<Props> {
  return applyPureReactInVue(react)
}

/** Creates a Vue component wrapping a React component inside {@link Suspense} element. */
export function suspendedReactComponent<Props extends object>(react: (props: Props) => ReactNode) {
  return reactComponent((props: Props) => Suspense({ children: react(props) }))
}
