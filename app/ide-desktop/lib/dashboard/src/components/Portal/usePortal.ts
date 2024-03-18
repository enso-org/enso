import { useEffect, useRef, useState } from 'react'

import invariant from 'tiny-invariant'

import { usePortalContext } from './PortalProvider'
import type { PortalProps } from './types'

/**
 * @internal
 */
export function usePortal(props: PortalProps) {
  const { children, isDisabled = false, root = null, onMount } = props

  const onMountRef = useRef(onMount)
  const portalContext = usePortalContext()
  const [mountRoot, setMountRoot] = useState<Element | null>(null)
  onMountRef.current = onMount

  useEffect(() => {
    if (!isDisabled) {
      const contextRoot = portalContext.root
      const currentRoot = root?.current ?? null
      const currentContextRoot = contextRoot?.current ?? null

      invariant(
        !(contextRoot == null && currentRoot == null),
        'Before using Portal, you need to specify a root, where the component should be mounted or put the component under the <Root /> component'
      )

      setMountRoot(currentRoot ?? currentContextRoot)
    }
  }, [root, portalContext.root, isDisabled])

  useEffect(() => {
    if (isDisabled || mountRoot) {
      onMountRef.current?.()
    }
  }, [isDisabled, mountRoot])

  return {
    isDisabled,
    children,
    mountRoot,
  }
}
