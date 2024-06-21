/** @file The container that launches the IDE. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as appUtils from '#/appUtils'

import * as gtagHooks from '#/hooks/gtagHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as suspense from '#/components/Suspense'

import type * as backendModule from '#/services/Backend'

import type * as types from '../../../types/types'

// =================
// === Component ===
// =================

/** Props for an {@link Editor}. */
export interface EditorProps {
  readonly hidden: boolean
  readonly ydocUrl: string | null
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly appRunner: types.EditorRunner | null
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
  const { hidden, projectStartupInfo } = props

  const editor = projectStartupInfo && (
    <EditorInternal {...props} projectStartupInfo={projectStartupInfo} />
  )

  return hidden ? (
    <React.Suspense>
      <errorBoundary.ErrorBoundary FallbackComponent={() => null}>
        {editor}
      </errorBoundary.ErrorBoundary>
    </React.Suspense>
  ) : (
    <suspense.Suspense loaderProps={{ minHeight: 'full' }}>
      <errorBoundary.ErrorBoundary>{editor}</errorBoundary.ErrorBoundary>
    </suspense.Suspense>
  )
}

// ======================
// === EditorInternal ===
// ======================

/** Props for an {@link EditorInternal}. */
interface EditorInternalProps extends EditorProps {
  readonly projectStartupInfo: backendModule.ProjectStartupInfo
}

/** An internal editor. */
function EditorInternal(props: EditorInternalProps) {
  const { hidden, ydocUrl, projectStartupInfo, appRunner: AppRunner } = props
  const { getText } = textProvider.useText()
  const gtagEvent = gtagHooks.useGtagEvent()
  const gtagEventRef = React.useRef(gtagEvent)
  gtagEventRef.current = gtagEvent
  const remoteBackend = backendProvider.useRemoteBackend()

  const projectQuery = reactQuery.useSuspenseQuery({
    queryKey: ['editorProject'],
    queryFn: () => projectStartupInfo.project,
    staleTime: 0,
    meta: { persist: false },
  })
  const project = projectQuery.data

  const logEvent = React.useCallback(
    (message: string, projectId?: string | null, metadata?: object | null) => {
      if (remoteBackend) {
        void remoteBackend.logEvent(message, projectId, metadata)
      }
    },
    [remoteBackend]
  )

  React.useEffect(() => {
    if (hidden) {
      return
    } else {
      return gtagHooks.gtagOpenCloseCallback(gtagEventRef, 'open_workflow', 'close_workflow')
    }
  }, [projectStartupInfo, hidden])

  const appProps: types.EditorProps | null = React.useMemo(() => {
    const projectId = projectStartupInfo.projectAsset.id
    const jsonAddress = project.jsonAddress
    const binaryAddress = project.binaryAddress
    const ydocAddress = ydocUrl ?? ''
    if (jsonAddress == null) {
      throw new Error(getText('noJSONEndpointError'))
    } else if (binaryAddress == null) {
      throw new Error(getText('noBinaryEndpointError'))
    } else {
      return {
        config: {
          engine: {
            rpcUrl: jsonAddress,
            dataUrl: binaryAddress,
            ydocUrl: ydocAddress,
          },
          startup: {
            project: project.packageName,
            displayedProjectName: project.name,
          },
          window: {
            topBarOffset: '0',
          },
        },
        projectId,
        hidden,
        ignoreParamsRegex: new RegExp(`^${appUtils.SEARCH_PARAMS_PREFIX}(.+)$`),
        logEvent,
      }
    }
  }, [
    projectStartupInfo.projectAsset.id,
    project.jsonAddress,
    project.binaryAddress,
    project.packageName,
    project.name,
    ydocUrl,
    getText,
    hidden,
    logEvent,
  ])

  if (AppRunner == null) {
    return <></>
  } else {
    // Currently the GUI component needs to be fully rerendered whenever the project is changed. Once
    // this is no longer necessary, the `key` could be removed.
    return <AppRunner key={appProps.projectId} {...appProps} />
  }
}
