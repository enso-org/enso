/**
 * @file The React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context.
 */
import * as React from 'react'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type LocalBackend from '#/services/LocalBackend'
import { ProjectManagerEvents } from '#/services/ProjectManager'
import type RemoteBackend from '#/services/RemoteBackend'

import { BackendContext, ProjectManagerContext } from './hooks'

/** Props for a {@link BackendProvider}. */
export interface BackendProviderProps extends Readonly<React.PropsWithChildren> {
  readonly remoteBackend: RemoteBackend | null
  readonly localBackend: LocalBackend | null
}

/** A React Provider that lets components get and set the current backend. */
export function BackendProvider(props: BackendProviderProps) {
  const { remoteBackend, localBackend, children } = props
  const [didLoadingProjectManagerFail, setDidLoadingProjectManagerFail] = React.useState(false)

  React.useEffect(() => {
    const onProjectManagerLoadingFailed = () => {
      setDidLoadingProjectManagerFail(true)
    }

    document.addEventListener(ProjectManagerEvents.loadingFailed, onProjectManagerLoadingFailed)
    return () => {
      document.removeEventListener(
        ProjectManagerEvents.loadingFailed,
        onProjectManagerLoadingFailed,
      )
    }
  }, [])

  const reconnectToProjectManager = useEventCallback(() => {
    setDidLoadingProjectManagerFail(false)
    void localBackend?.reconnectProjectManager()
  })

  return (
    <BackendContext.Provider value={{ remoteBackend, localBackend }}>
      <ProjectManagerContext.Provider
        value={{ didLoadingProjectManagerFail, reconnectToProjectManager }}
      >
        {children}
      </ProjectManagerContext.Provider>
    </BackendContext.Provider>
  )
}
