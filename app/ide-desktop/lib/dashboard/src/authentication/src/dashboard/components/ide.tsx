/** @file Container that launches the IDE. */
import * as react from 'react'

import * as backendProvider from '../../providers/backend'
import * as cloudService from '../cloudService'
import * as platformModule from '../../platform'

// =================
// === Constants ===
// =================

/** The `id` attribute of the element into which the IDE will be rendered. */
const IDE_ELEMENT_ID = 'root'
const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'
const JS_EXTENSION: Record<platformModule.Platform, string> = {
    [platformModule.Platform.cloud]: '.js.gz',
    [platformModule.Platform.desktop]: '.js',
} as const

// =================
// === Component ===
// =================

interface Props {
    project: cloudService.Project
    backendPlatform: platformModule.Platform
}

/** Container that launches the IDE. */
function Ide(props: Props) {
    const { project, backendPlatform } = props
    const { backend } = backendProvider.useBackend()

    react.useEffect(() => {
        document.getElementById(IDE_ELEMENT_ID)?.classList.remove('hidden')
        return () => {
            document.getElementById(IDE_ELEMENT_ID)?.classList.add('hidden')
        }
    }, [])

    react.useEffect(() => {
        void (async () => {
            const ideVersion =
                project.ideVersion?.value ??
                ('listVersions' in backend
                    ? await backend.listVersions({
                          versionType: cloudService.VersionType.ide,
                          default: true,
                      })
                    : null)?.[0].number.value
            const engineVersion =
                project.engineVersion?.value ??
                ('listVersions' in backend
                    ? await backend.listVersions({
                          versionType: cloudService.VersionType.backend,
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
                    switch (backendPlatform) {
                        case platformModule.Platform.cloud:
                            return `${IDE_CDN_URL}/${ideVersion}/`
                        case platformModule.Platform.desktop:
                            return ''
                    }
                })()
                const runNewProject = async () => {
                    const originalUrl = window.location.href
                    // The URL query contains commandline options when running in the desktop,
                    // which will break the entrypoint for opening a fresh IDE instance.
                    history.replaceState(null, '', new URL('.', originalUrl))
                    await window.runProject({
                        loader: {
                            assetsUrl: `${assetsRoot}dynamic-assets`,
                            wasmUrl: `${assetsRoot}pkg-opt.wasm`,
                            jsUrl: `${assetsRoot}pkg${JS_EXTENSION[backendPlatform]}`,
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
                    // Restore original URL so that initialization works correctly on refresh.
                    history.replaceState(null, '', originalUrl)
                }
                if (backendPlatform === platformModule.Platform.desktop) {
                    await runNewProject()
                } else {
                    const script = document.createElement('script')
                    script.src = `${IDE_CDN_URL}/${engineVersion}/index.js.gz`
                    script.onload = async () => {
                        document.body.removeChild(script)
                        await runNewProject()
                    }
                    document.body.appendChild(script)
                }
                return
            }
        })()
    }, [project])

    return <></>
}

export default Ide
