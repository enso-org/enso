import { Dialog as DialogReact } from '#/components/Dialog'
import { Result as ResultReact } from '#/components/Result'
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
  component: (props: Props) => unknown,
): DefineComponent<Props> {
  return applyPureReactInVue(component)
}

/** Creates a Vue component wrapping a React component inside {@link Suspense} element. */
export function suspendedReactComponent<Props extends object>(
  Component: (props: Props) => ReactNode,
) {
  return reactComponent((props: Props) => (
    <Suspense>
      <Component {...props} />
    </Suspense>
  ))
}

// Common components
export const ResultComponent = reactComponent(ResultReact)
export const Dialog = reactComponent(DialogReact)
