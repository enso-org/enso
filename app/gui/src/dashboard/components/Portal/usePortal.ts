/**
 * @file
 * The hook contains the logic for mounting the children into the portal.
 */
import * as React from 'react'

import invariant from 'tiny-invariant'

import { useUNSAFE_PortalContext } from '#/components/aria'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type * as types from './types'

/**
 * The hook contains the logic for mounting the children into the portal.
 * @internal
 */
export function usePortal(props: types.PortalProps) {
  const { children, isDisabled = false, root = null, onMount = () => {} } = props

  const portalContext = useUNSAFE_PortalContext()
  const [mountRoot, setMountRoot] = React.useState<Element | null>(null)

  const onMountEventCallback = useEventCallback(onMount)

  React.useEffect(() => {
    if (!isDisabled) {
      const contextRoot = portalContext.getContainer?.()
      const currentRoot = root?.current ?? null
      const combinedRoot = currentRoot ?? contextRoot

      invariant(
        combinedRoot,
        'Before using Portal, you need to specify a root, where the component should be mounted or put the component under the <Root /> component',
      )

      setMountRoot(combinedRoot)
    }
  }, [root, portalContext, isDisabled])

  React.useEffect(() => {
    if (isDisabled || mountRoot) {
      onMountEventCallback()
    }
  }, [isDisabled, mountRoot, onMountEventCallback])

  return {
    isDisabled,
    children,
    mountRoot,
  }
}
