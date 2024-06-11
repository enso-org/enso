/** @file The container that launches the IDE. */
import * as React from 'react'

import * as appUtils from '#/appUtils'

import * as gtagHooks from '#/hooks/gtagHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as remoteBackendProvider from '#/providers/RemoteBackendProvider'

import type * as backendModule from '#/services/Backend'

import type * as types from '../../../types/types'

// =================
// === Constants ===
// =================

/** The horizontal offset of the editor's top bar from the left edge of the window. */
const TOP_BAR_X_OFFSET_PX = 96

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
  const { hidden, ydocUrl, projectStartupInfo, appRunner: AppRunner } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const gtagEvent = gtagHooks.useGtagEvent()
  const gtagEventRef = React.useRef(gtagEvent)
  const remoteBackend = remoteBackendProvider.useRemoteBackend()
  const { backend } = backendProvider.useStrictBackend()

  const logEvent = React.useCallback(
    (message: string, projectId?: string | null, metadata?: object | null) => {
      if (remoteBackend) {
        void remoteBackend.logEvent(message, projectId, metadata)
      }
    },
    [remoteBackend]
  )

  gtagEventRef.current = gtagEvent

  React.useEffect(() => {
    if (hidden) {
      return
    } else {
      return gtagHooks.gtagOpenCloseCallback(gtagEventRef, 'open_workflow', 'close_workflow')
    }
  }, [projectStartupInfo, hidden])

  const appProps: types.EditorProps | null = React.useMemo(() => {
    // eslint-disable-next-line no-restricted-syntax
    if (projectStartupInfo == null) return null
    const { project } = projectStartupInfo
    const { id: projectId, parentId, title } = projectStartupInfo.projectAsset
    const jsonAddress = project.jsonAddress
    const binaryAddress = project.binaryAddress
    const ydocAddress = ydocUrl ?? ''
    if (jsonAddress == null) {
      toastAndLog('noJSONEndpointError')
      return null
    } else if (binaryAddress == null) {
      toastAndLog('noBinaryEndpointError')
      return null
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
            topBarOffset: `${TOP_BAR_X_OFFSET_PX}`,
          },
        },
        projectId,
        hidden,
        ignoreParamsRegex: new RegExp(`^${appUtils.SEARCH_PARAMS_PREFIX}(.+)$`),
        logEvent,
        renameProject: newName => {
          backend
            .updateProject(
              projectId,
              { projectName: newName, ami: null, ideVersion: null, parentId },
              title
            )
            .catch(e => toastAndLog('renameProjectError', e))
        },
      }
    }
  }, [projectStartupInfo, toastAndLog, hidden, logEvent, ydocUrl, backend])

  if (projectStartupInfo == null || AppRunner == null || appProps == null) {
    return <></>
  } else {
    // Currently the GUI component needs to be fully rerendered whenever the project is changed. Once
    // this is no longer necessary, the `key` could be removed.
    return <AppRunner key={appProps.projectId} {...appProps} />
  }
}
