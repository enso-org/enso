/** @file Container that launches the IDE. */
import * as react from 'react'

import * as cloudService from '../cloudService'
import * as projectManagerService from '../localService'

import * as platformModule from '../../platform'

// =================
// === Constants ===
// =================

const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'
const FALLBACK_VERSION = '2023.1.1-nightly.2023.4.13'

// =================
// === Component ===
// =================

interface Props {
    platform: platformModule.Platform
    project: cloudService.Project
    backendService: cloudService.Backend | projectManagerService.Backend
}

/** Container that launches the IDE. */
function Ide(props: Props) {
    const { project, backendService } = props
    const [[loaded, resolveLoaded]] = react.useState<[Promise<void>, () => void]>(() => {
        let doResolve!: () => void
        const promise = new Promise<void>(resolve => (doResolve = resolve))
        return [promise, doResolve]
    })

    react.useEffect(() => {
        document.querySelector('body > #root')?.remove()
        const script = document.createElement('script')
        script.src = `${IDE_CDN_URL}/${FALLBACK_VERSION}/index.js.gz`
        script.onload = resolveLoaded
        document.body.appendChild(script)
    }, [])

    react.useEffect(() => {
        void (async () => {
            const ideVersion = (
                'listVersions' in backendService
                    ? await backendService.listVersions({
                          versionType: cloudService.VersionType.ide,
                          default: true,
                      })
                    : null
            )?.[0].number.value
            const backendVersion = (
                'listVersions' in backendService
                    ? await backendService.listVersions({
                          versionType: cloudService.VersionType.backend,
                          default: true,
                      })
                    : null
            )?.[0].number.value
            const projectIdeVersion = project.ideVersion?.value ?? ideVersion ?? FALLBACK_VERSION
            const projectEngineVersion =
                project.engineVersion?.value ?? backendVersion ?? FALLBACK_VERSION
            await loaded
            const freshRoot = document.createElement('div')
            freshRoot.id = 'root'
            document.querySelector('#root')?.replaceWith(freshRoot)
            await window.enso.main({
                loader: {
                    assetsUrl: `${IDE_CDN_URL}/${projectIdeVersion}/dynamic-assets`,
                    wasmUrl: `${IDE_CDN_URL}/${projectIdeVersion}/pkg-opt.wasm`,
                    jsUrl: `${IDE_CDN_URL}/${projectIdeVersion}/pkg.js.gz`,
                },
                engine: {
                    rpcUrl: `${project.address!}json`,
                    dataUrl: `${project.address!}binary`,
                    preferredVersion: projectEngineVersion,
                },
                startup: {
                    project: project.packageName,
                },
            })
        })()
    }, [project])

    return <div id="root" />
}

export default Ide
