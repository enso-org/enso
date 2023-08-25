/** @file Container that launches the editor. */
import * as React from 'react'

import * as backendModule from '../backend'
import * as hooks from '../../hooks'
import * as load from '../load'

import GLOBAL_CONFIG from '../../../../../../../../gui/config.yaml' assert { type: 'yaml' }

// =================
// === Constants ===
// =================

/** The `id` attribute of the loading spinner element created by the wasm entrypoint. */
const LOADER_ELEMENT_ID = 'loader'

/** The horizontal offset of the editor's top bar from the left edge of the window. */
const TOP_BAR_X_OFFSET_PX = 96
/** The `id` attribute of the element into which the IDE will be rendered. */
const IDE_ELEMENT_ID = 'root'
const IDE_CDN_BASE_URL = 'https://cdn.enso.org/ide'
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
    supportsLocalBackend: boolean
    projectStartupInfo: backendModule.ProjectStartupInfo | null
    appRunner: AppRunner
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
    const { visible, supportsLocalBackend, projectStartupInfo, appRunner } = props
    const toastAndLog = hooks.useToastAndLog()
    const [initialized, setInitialized] = React.useState(supportsLocalBackend)

    React.useEffect(() => {
        const ideElement = document.getElementById(IDE_ELEMENT_ID)
        if (ideElement != null) {
            if (visible) {
                ideElement.style.top = ''
                ideElement.style.display = 'absolute'
            } else {
                ideElement.style.top = '-100vh'
                ideElement.style.display = 'fixed'
            }
        }
    }, [visible])

    React.useEffect(() => {
        if (projectStartupInfo != null && !visible) {
            // A workaround to hide the spinner, when the previous project is being loaded in
            // the background. This `MutationObserver` is disconnected when the loader is
            // removed from the DOM.
            const observer = new MutationObserver(mutations => {
                for (const mutation of mutations) {
                    for (const node of Array.from(mutation.addedNodes)) {
                        if (node instanceof HTMLElement && node.id === LOADER_ELEMENT_ID) {
                            document.body.style.cursor = 'auto'
                            node.style.display = 'none'
                        }
                    }
                    for (const node of Array.from(mutation.removedNodes)) {
                        if (node instanceof HTMLElement && node.id === LOADER_ELEMENT_ID) {
                            document.body.style.cursor = 'auto'
                            observer.disconnect()
                        }
                    }
                }
            })
            observer.observe(document.body, { childList: true })
        }
    }, [projectStartupInfo, visible])

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
                    toastAndLog("Could not get the address of the project's JSON endpoint")
                } else if (binaryAddress == null) {
                    toastAndLog("Could not get the address of the project's binary endpoint")
                } else {
                    let assetsRoot: string
                    switch (backendType) {
                        case backendModule.BackendType.remote: {
                            assetsRoot = `${IDE_CDN_BASE_URL}/${project.ideVersion.value}/`
                            break
                        }
                        case backendModule.BackendType.local: {
                            assetsRoot = ''
                            break
                        }
                    }
                    const runNewProject = async () => {
                        const engineConfig =
                            backendType === backendModule.BackendType.remote
                                ? {
                                      rpcUrl: jsonAddress,
                                      dataUrl: binaryAddress,
                                  }
                                : {
                                      projectManagerUrl: GLOBAL_CONFIG.projectManagerEndpoint,
                                  }
                        const originalUrl = window.location.href
                        if (backendType === backendModule.BackendType.remote) {
                            // The URL query contains commandline options when running in the desktop,
                            // which will break the entrypoint for opening a fresh IDE instance.
                            history.replaceState(null, '', new URL('.', originalUrl))
                        }
                        await appRunner.runApp(
                            {
                                loader: {
                                    assetsUrl: `${assetsRoot}dynamic-assets`,
                                    wasmUrl: `${assetsRoot}pkg-opt.wasm`,
                                    jsUrl: `${assetsRoot}pkg${JS_EXTENSION[backendType]}`,
                                },
                                engine: {
                                    ...engineConfig,
                                    ...(project.engineVersion != null
                                        ? { preferredVersion: project.engineVersion.value }
                                        : {}),
                                },
                                startup: {
                                    project: project.packageName,
                                },
                                window: {
                                    topBarOffset: `${TOP_BAR_X_OFFSET_PX}`,
                                },
                            },
                            accessToken,
                            { projectId: project.projectId }
                        )
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
                                load.loadScript(`${assetsRoot}index.js.gz`),
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
    }, [
        projectStartupInfo,
        /* should never change */ appRunner,
        /* should never change */ toastAndLog,
    ])

    return <></>
}
