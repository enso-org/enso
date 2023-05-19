/** @file Container that launches the IDE. */
import * as react from 'react'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as platformModule from '../../platform'

// =================
// === Constants ===
// =================

const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'
const JS_EXTENSION: Record<platformModule.Platform, string> = {
    [platformModule.Platform.cloud]: '.js.gz',
    [platformModule.Platform.desktop]: '.js',
} as const

// =================
// === Component ===
// =================

/** Props for an {@link Ide}. */
export interface IdeProps {
    project: backendModule.Project
    appRunner: AppRunner | null
}

/** The ontainer that launches the IDE. */
function Ide(props: IdeProps) {
    const { project, appRunner } = props
    const { backend } = backendProvider.useBackend()

    react.useEffect(() => {
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
                    switch (backend.platform) {
                        case platformModule.Platform.cloud:
                            return `${IDE_CDN_URL}/${ideVersion}/`
                        case platformModule.Platform.desktop:
                            return ''
                    }
                })()
                const runNewProject = async () => {
                    await appRunner?.runApp({
                        loader: {
                            assetsUrl: `${assetsRoot}dynamic-assets`,
                            wasmUrl: `${assetsRoot}pkg-opt.wasm`,
                            jsUrl: `${assetsRoot}pkg${JS_EXTENSION[backend.platform]}`,
                        },
                        engine: {
                            rpcUrl: jsonAddress,
                            dataUrl: binaryAddress,
                            preferredVersion: engineVersion,
                        },
                        startup: {
                            project: project.packageName,
                        },
                    })
                }
                if (backend.platform === platformModule.Platform.desktop) {
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
    }, [project])

    return <></>
}

export default Ide
