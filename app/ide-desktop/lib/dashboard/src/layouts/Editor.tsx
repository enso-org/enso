/** @file The container that launches the IDE. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as appUtils from '#/appUtils'

import * as gtagHooks from '#/hooks/gtagHooks'
import * as syncRefHooks from '#/hooks/syncRefHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as loader from '#/components/Loader'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

import type * as types from '../../../types/types'

// =================
// === Component ===
// =================

/** Props for an {@link Editor}. */
export interface EditorProps {
  readonly hidden: boolean
  readonly ydocUrl: string | null
  readonly setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly setProjectStartupInfo: React.Dispatch<
    React.SetStateAction<backendModule.ProjectStartupInfo | null>
  >
  readonly appRunner: types.EditorRunner | null
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
  const { hidden, projectStartupInfo } = props

  const editor = projectStartupInfo && (
    <EditorInternal {...props} projectStartupInfo={projectStartupInfo} />
  )

  return (
    <React.Suspense fallback={hidden ? undefined : <loader.Loader minHeight="full" />}>
      {/* eslint-disable-next-line @typescript-eslint/naming-convention */}
      <errorBoundary.ErrorBoundary {...(hidden ? { FallbackComponent: () => null } : {})}>
        {editor}
      </errorBoundary.ErrorBoundary>
    </React.Suspense>
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
  const { hidden, ydocUrl, projectStartupInfo, setProjectStartupInfo, appRunner: AppRunner } = props
  const { setProjectAsset } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const gtagEvent = gtagHooks.useGtagEvent()
  const gtagEventRef = React.useRef(gtagEvent)
  gtagEventRef.current = gtagEvent
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const projectTitleRef = syncRefHooks.useSyncRef(projectStartupInfo.projectAsset.title)

  const projectQuery = reactQuery.useSuspenseQuery({
    queryKey: ['editorProject', projectStartupInfo.projectAsset],
    queryFn: () =>
      // Wrap in a new Promise, otherwise React Suspense forgets to unsuspend.
      new Promise<backendModule.Project>(resolve => {
        setTimeout(() => {
          resolve(projectStartupInfo.project)
        })
      }),
    staleTime: 0,
    gcTime: 0,
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

  const renameProject = React.useCallback(
    (newName: string) => {
      let backend: Backend | null
      switch (projectStartupInfo.backendType) {
        case backendModule.BackendType.local:
          backend = localBackend
          break
        case backendModule.BackendType.remote:
          backend = remoteBackend
          break
      }
      const projectId = projectStartupInfo.projectAsset.id
      const parentId = projectStartupInfo.projectAsset.parentId
      backend
        ?.updateProject(
          projectId,
          { projectName: newName, ami: null, ideVersion: null, parentId },
          projectTitleRef.current
        )
        .then(
          () => {
            setProjectAsset(object.merger({ title: newName }))
            setProjectStartupInfo(currentInfo =>
              currentInfo == null
                ? null
                : object.merge(currentInfo, {
                    project: currentInfo.project.then(currentProject =>
                      object.merge(currentProject, { name: newName })
                    ),
                    projectAsset: object.merge(currentInfo.projectAsset, { title: newName }),
                  })
            )
          },
          e => toastAndLog('renameProjectError', e)
        )
    },
    [
      localBackend,
      projectStartupInfo.backendType,
      projectStartupInfo.projectAsset.id,
      projectStartupInfo.projectAsset.parentId,
      setProjectAsset,
      projectTitleRef,
      remoteBackend,
      setProjectStartupInfo,
      toastAndLog,
    ]
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
        renameProject,
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
    renameProject,
  ])

  if (AppRunner == null) {
    return null
  } else {
    // Currently the GUI component needs to be fully rerendered whenever the project is changed. Once
    // this is no longer necessary, the `key` could be removed.
    return <AppRunner key={appProps.projectId} {...appProps} />
  }
}
