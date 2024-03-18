/**
 * @file Portal component
 * Renders its children outside the current DOM hierarchy
 */

import * as React from 'react'

import * as reactDom from 'react-dom'

import type { PortalProps } from './types'
import { usePortal } from './usePortal'

/**
 * This component renders its children outside the current DOM hierarchy.
 *
 * React [doesn't support](https://github.com/facebook/react/issues/13097) portal API in SSR, so, if you want to
 * render a Portal in SSR, use prop `disabled`.
 *
 * By default, Portal's children render under the `<Root />` component.
 *
 * ***Important***: Since React doesn't support portals on SSR, `<Portal />` children renders in the next tick.
 * If you need to make some computations, use the `onMount` callback
 * @see https://reactjs.org/docs/portals.html
 * @example ```jsx
 *  <div>
 *    Portal will be rendered outside me!
 *
 *    <Portal>
 *      <div>some content will be showed outside of parent container</div>
 *    </Portal>
 *  </div>
 * ```
 */
export function Portal(props: PortalProps): React.JSX.Element | null {
  const { children, mountRoot, isDisabled } = usePortal(props)

  if (isDisabled) return <>{children}</>
  return mountRoot ? reactDom.createPortal(children, mountRoot) : null
}
