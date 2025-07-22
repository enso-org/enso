/** @file Logic for mounting children into a portal. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useEffect } from 'react'
import { usePortalContext } from './PortalProvider'
import type { PortalProps } from './types'

/**
 * Logic for mounting children into a portal.
 * @internal
 */
export function usePortal(props: PortalProps) {
  const { children, isDisabled = false, root = null, onMount = () => {} } = props

  const contextRoot = usePortalContext()
  const onMountEventCallback = useEventCallback(onMount)

  useEffect(() => {
    onMountEventCallback()
  }, [isDisabled, onMountEventCallback])

  return {
    isDisabled,
    children,
    mountRoot: root?.current ?? contextRoot,
  }
}
