import { Dialog as DialogReact } from '#/components/Dialog/Dialog'
import { Loader as LoaderReact } from '#/components/Loader'
import { Result as ResultReact } from '#/components/Result'
import { Suspense } from '#/components/Suspense'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import type { ReactNode } from 'react'
// Imported here to implement the safer wrapper.
// eslint-disable-next-line no-restricted-imports
import { applyPureReactInVue, type magicOptions } from 'veaury'
import type { DefineComponent } from 'vue'

/**
 * Creates a Vue component wrapping a React component.
 *
 * This should always be used in preference to {@link applyPureReactInVue}; its type information is
 * more precise, and it works around a bug.
 */
export function reactComponent<Props extends object>(
  component: (props: Props) => unknown,
  options?: Opt<magicOptions>,
): DefineComponent<Props> {
  const vueComponent = applyPureReactInVue(component, options)
  const cleanup = vueComponent.beforeUnmount
  return {
    ...vueComponent,
    beforeUnmount() {
      // Veaury's `beforeUnmount` hook fails with an exception if it is called when the `mounted`
      // hook was not called. Check for a property set by the `mounted` hook and skip the cleanup if
      // mounting did not occur
      if (this.__veauryLast__) cleanup.call(this)
    },
  }
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
export const Loader = reactComponent(LoaderReact)
export const ResultComponent = reactComponent(ResultReact)
export const Dialog = reactComponent(DialogReact)
