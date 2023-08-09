/** @file Container that launches the editor. */
import * as React from 'react'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'

import GLOBAL_CONFIG from '../../../../../../../../gui/config.yaml' assert { type: 'yaml' }

// =================
// === Constants ===
// =================

/** The horizontal offset of the editor's top bar from the left edge of the window. */
const TOP_BAR_X_OFFSET_PX = 96
/** The `id` attribute of the element into which the IDE will be rendered. */
const IDE_ELEMENT_ID = 'root'
const IDE_CDN_URL = 'https://cdn.enso.org/ide'
const JS_EXTENSION: Record<backendModule.BackendType, string> = {
    [backendModule.BackendType.remote]: '.js.gz',
    [backendModule.BackendType.local]: '.js',
} as const

// =================
// === Component ===
// =================

/** Props for an {@link Editor}. */
export interface EditorProps {
    visible: boolean
    project: backendModule.Project | null
    appRunner: AppRunner
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
    const { visible, project, appRunner } = props
    const { backend } = backendProvider.useBackend()
    const { accessToken } = auth.useNonPartialUserSession()

    React.useEffect(() => {
        const ideElement = document.getElementById(IDE_ELEMENT_ID)
        if (ideElement) {
            if (visible) {
                ideElement.style.top = ''
                ideElement.style.display = 'absolute'
            } else {
                ideElement.style.top = '-100vh'
                ideElement.style.display = 'fixed'
            }
        }
    }, [visible])

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
        if (project != null) {
            void (async () => {
                const ideVersion =
                    project.ideVersion?.value ??
                    (backend.type === backendModule.BackendType.remote
                        ? await backend.listVersions({
                              versionType: backendModule.VersionType.ide,
                              default: true,
                          })
                        : null)?.[0].number.value
                const engineVersion =
                    project.engineVersion?.value ??
                    (backend.type === backendModule.BackendType.remote
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
                    let assetsRoot: string
                    switch (backend.type) {
                        case backendModule.BackendType.remote: {
                            assetsRoot = `${IDE_CDN_URL}/${ideVersion}/`
                            break
                        }
                        case backendModule.BackendType.local: {
                            assetsRoot = ''
                            break
                        }
                    }
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
                                window: {
                                    topBarOffset: `${TOP_BAR_X_OFFSET_PX}`,
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
            return () => {
                appRunner.stopApp()
            }
        }
    }, [project, /* should never change */ appRunner])

    return <></>
}
