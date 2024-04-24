/** @file The container that launches the IDE. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import * as gtagHooks from '#/hooks/gtagHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendModule from '#/services/Backend'

import * as load from '#/utilities/load'

// =================
// === Constants ===
// =================

/** The horizontal offset of the editor's top bar from the left edge of the window. */
const TOP_BAR_X_OFFSET_PX = 96
/** The `id` attribute of the element into which the IDE will be rendered. */
const IDE_ELEMENT_ID = 'app'
const IDE_CDN_BASE_URL = 'https://cdn.enso.org/ide'
const JS_EXTENSION: Readonly<Record<backendModule.BackendType, string>> = {
  [backendModule.BackendType.remote]: '.js.gz',
  [backendModule.BackendType.local]: '.js',
}

// =================
// === Component ===
// =================

/** Props for an {@link Editor}. */
export interface EditorProps {
  readonly hidden: boolean
  readonly supportsLocalBackend: boolean
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly appRunner: AppRunner
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
  const { hidden, supportsLocalBackend, projectStartupInfo, appRunner } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const gtagEvent = gtagHooks.useGtagEvent()
  const gtagEventRef = React.useRef(gtagEvent)
  gtagEventRef.current = gtagEvent
  const [initialized, setInitialized] = React.useState(supportsLocalBackend)

  React.useEffect(() => {
    const ideElement = document.getElementById(IDE_ELEMENT_ID)
    if (ideElement != null) {
      ideElement.style.display = hidden ? 'none' : ''
    }
  }, [hidden])

  React.useEffect(() => {
    if (hidden) {
      return
    } else {
      const params = {
        platform: detect.platform(),
        architecture: detect.architecture(),
      }
      const gtagEventCurrent = gtagEventRef.current
      gtagEventCurrent('open_workflow', params)
      const onBeforeUnload = () => {
        gtagEventCurrent('close_workflow', params)
      }
      window.addEventListener('beforeunload', onBeforeUnload)
      return () => {
        window.removeEventListener('beforeunload', onBeforeUnload)
        gtagEventCurrent('close_workflow', params)
      }
    }
  }, [projectStartupInfo, hidden])

  let hasEffectRun = false

  React.useEffect(() => {
    // This is a hack to work around the IDE WASM not playing nicely with React Strict Mode.
    // This is unavoidable as the WASM must fully set up to be able to properly drop its assets,
    // but React re-executes this side-effect faster tha the WASM can do so.
    if (hasEffectRun) {
      // eslint-disable-next-line no-restricted-syntax
      return
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
    hasEffectRun = true
    if (projectStartupInfo != null) {
      const { project, backendType, accessToken } = projectStartupInfo
      void (async () => {
        const jsonAddress = project.jsonAddress
        const binaryAddress = project.binaryAddress
        if (jsonAddress == null) {
          toastAndLog('noJSONEndpointError')
        } else if (binaryAddress == null) {
          toastAndLog('noBinaryEndpointError')
        } else {
          let assetsRoot: string
          switch (backendType) {
            case backendModule.BackendType.remote: {
              if (project.ideVersion == null) {
                toastAndLog('noIdeVersionError')
                // This is too deeply nested to easily return from
                // eslint-disable-next-line no-restricted-syntax
                return
              }
              assetsRoot = `${IDE_CDN_BASE_URL}/${project.ideVersion.value}/`
              break
            }
            case backendModule.BackendType.local: {
              assetsRoot = ''
              break
            }
          }
          const runNewProject = async () => {
            const engineConfig = {
              rpcUrl: jsonAddress,
              dataUrl: binaryAddress,
            }
            const originalUrl = window.location.href
            if (backendType === backendModule.BackendType.remote) {
              // The URL query contains commandline options when running in the desktop,
              // which will break the entrypoint for opening a fresh IDE instance.
              history.replaceState(null, '', new URL('.', originalUrl))
            }
            try {
              await appRunner.runApp(
                {
                  loader: {
                    assetsUrl: `${assetsRoot}dynamic-assets`,
                    wasmUrl: `${assetsRoot}pkg-opt.wasm`,
                    jsUrl: `${assetsRoot}pkg${JS_EXTENSION[backendType]}`,
                  },
                  engine: engineConfig,
                  startup: {
                    project: project.packageName,
                    displayedProjectName: project.name,
                  },
                  window: {
                    topBarOffset: `${TOP_BAR_X_OFFSET_PX}`,
                  },
                },
                accessToken,
                {
                  projectId: project.projectId,
                  ignoreParamsRegex: new RegExp(`^${appUtils.SEARCH_PARAMS_PREFIX}(.+)$`),
                }
              )
            } catch (error) {
              toastAndLog('openEditorError', error)
            }
            if (backendType === backendModule.BackendType.remote) {
              // Restore original URL so that initialization works correctly on refresh.
              history.replaceState(null, '', originalUrl)
            }
          }
          if (supportsLocalBackend) {
            await runNewProject()
          } else {
            if (!initialized) {
              await Promise.all([
                load.loadStyle(`${assetsRoot}style.css`),
                load
                  .loadScript(`${assetsRoot}entrypoint.js.gz`)
                  .catch(() => load.loadScript(`${assetsRoot}index.js.gz`)),
              ])
              setInitialized(true)
            }
            await runNewProject()
          }
        }
      })()
      return () => {
        appRunner.stopApp()
      }
    } else {
      return
    }
  }, [projectStartupInfo, toastAndLog, /* should never change */ appRunner])

  return <></>
}
