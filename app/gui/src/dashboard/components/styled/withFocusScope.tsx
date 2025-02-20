/** @file A higher order component wrapping the inner component with a {@link aria.FocusScope}. */
import * as React from 'react'

import * as aria from '#/components/aria'

// ======================
// === withFocusScope ===
// ======================

/**
 * Wrap a component in a {@link aria.FocusScope}. This allows {@link aria.useFocusManager} to be
 * used in the component.
 */
// This is not a React component, even though it contains JSX.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function withFocusScope<ComponentType extends (props: any) => React.ReactNode>(
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Child: ComponentType,
) {
  // eslint-disable-next-line no-restricted-syntax
  return function WithFocusScope(props: never) {
    return (
      <aria.FocusScope>
        {/* eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any */}
        <Child {...(props as any)} />
      </aria.FocusScope>
    )
    // This type assertion is REQUIRED in order to preserve generics.
  } as unknown as ComponentType
}
