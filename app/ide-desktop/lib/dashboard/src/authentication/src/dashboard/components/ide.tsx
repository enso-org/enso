/** @file Container that launches the IDE. */
import * as React from 'react'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'

import GLOBAL_CONFIG from '../../../../../../../../gui/config.yaml' assert { type: 'yaml' }

// =================
// === Constants ===
// =================

const IDE_CDN_URL = 'https://cdn.enso.org/ide'
const JS_EXTENSION: Record<backendModule.BackendType, string> = {
    [backendModule.BackendType.remote]: '.js.gz',
    [backendModule.BackendType.local]: '.js',
} as const

// =================
// === Component ===
// =================

/** Props for an {@link Ide}. */
export interface IdeProps {
    project: backendModule.Project
    appRunner: AppRunner
}

/** The container that launches the IDE. */
function Ide(props: IdeProps) {
    const { project, appRunner } = props
    const { backend } = backendProvider.useBackend()
    const { accessToken } = auth.useNonPartialUserSession()

    React.useEffect(() => {
        void (async () => {
            const ideVersion =
                project.ideVersion?.value ??
                ('listVersions' in backend
                    ? await backend.listVersions({
                          versionType: backendModule.VersionType.ide,
                          default: true,
                      })
                    : null)?.[0].number.value
            const engineVersion =
                project.engineVersion?.value ??
                ('listVersions' in backend
                    ? await backend.listVersions({
                          versionType: backendModule.VersionType.backend,
                          default: true,
                      })
                    : null)?.[0].number.value
            const jsonAddress = project.jsonAddress
            const binaryAddress = project.binaryAddress
            if (ideVersion == null) {
                throw new Error('Could not get the IDE version of the project.')
            } else if (engineVersion == null) {
                throw new Error('Could not get the engine version of the project.')
            } else if (jsonAddress == null) {
                throw new Error("Could not get the address of the project's JSON endpoint.")
            } else if (binaryAddress == null) {
                throw new Error("Could not get the address of the project's binary endpoint.")
            } else {
                const assetsRoot = (() => {
                    switch (backend.type) {
                        case backendModule.BackendType.remote:
                            return `${IDE_CDN_URL}/${ideVersion}/`
                        case backendModule.BackendType.local:
                            return ''
                    }
                })()
                const runNewProject = async () => {
                    const engineConfig =
                        backend.type === backendModule.BackendType.remote
                            ? {
                                  rpcUrl: jsonAddress,
                                  dataUrl: binaryAddress,
                              }
                            : {
                                  projectManagerUrl: GLOBAL_CONFIG.projectManagerEndpoint,
                              }
                    await appRunner.runApp(
                        {
                            loader: {
                                assetsUrl: `${assetsRoot}dynamic-assets`,
                                wasmUrl: `${assetsRoot}pkg-opt.wasm`,
                                jsUrl: `${assetsRoot}pkg${JS_EXTENSION[backend.type]}`,
                            },
                            engine: {
                                ...engineConfig,
                                preferredVersion: engineVersion,
                            },
                            startup: {
                                project: project.packageName,
                            },
                        },
                        // Here we actually need explicit undefined.
                        // eslint-disable-next-line no-restricted-syntax
                        accessToken ?? undefined
                    )
                }
                if (backend.type === backendModule.BackendType.local) {
                    await runNewProject()
                    return
                } else {
                    const script = document.createElement('script')
                    script.crossOrigin = 'anonymous'
                    script.src = `${IDE_CDN_URL}/${engineVersion}/index.js.gz`
                    script.onload = async () => {
                        document.body.removeChild(script)
                        const originalUrl = window.location.href
                        // The URL query contains commandline options when running in the desktop,
                        // which will break the entrypoint for opening a fresh IDE instance.
                        history.replaceState(null, '', new URL('.', originalUrl))
                        await runNewProject()
                        // Restore original URL so that initialization works correctly on refresh.
                        history.replaceState(null, '', originalUrl)
                    }
                    document.body.appendChild(script)
                    const style = document.createElement('link')
                    style.crossOrigin = 'anonymous'
                    style.rel = 'stylesheet'
                    style.href = `${IDE_CDN_URL}/${engineVersion}/style.css`
                    document.body.appendChild(style)
                    return () => {
                        style.remove()
                    }
                }
            }
        })()
        // The backend MUST NOT be a dependency, since the IDE should only be recreated when a new
        // project is opened.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [appRunner, project])

    return <></>
}

export default Ide
