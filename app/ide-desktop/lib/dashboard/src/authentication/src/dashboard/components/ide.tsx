/** @file Container that launches the IDE. */
import * as react from 'react'

import * as cloudService from '../service'
import * as projectManagerService from '../projectManagerService'

// =================
// === Constants ===
// =================

const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'

// =================
// === Component ===
// =================

interface Props {
    project: cloudService.Project
    backendService: cloudService.Backend | projectManagerService.Backend
}

/** Container that launches the IDE. */
function Ide({ project, backendService }: Props) {
    const [[loaded, resolveLoaded]] = react.useState((): [Promise<void>, () => void] => {
        let resolve!: () => void
        const promise = new Promise<void>(innerResolve => {
            resolve = innerResolve
        })
        return [promise, resolve]
    })

    react.useEffect(() => {
        void (async () => {
            const ideVersion = (
                await backendService.listVersions({
                    versionType: cloudService.VersionType.ide,
                    default: true,
                })
            )[0]
            const projectIdeVersion = project.ideVersion?.value ?? ideVersion.number.value
            const stylesheetLink = document.createElement('link')
            stylesheetLink.rel = 'stylesheet'
            stylesheetLink.href = `${IDE_CDN_URL}/${projectIdeVersion}/style.css`
            const indexScript = document.createElement('script')
            indexScript.src = `${IDE_CDN_URL}/${projectIdeVersion}/index.js.gz`
            indexScript.addEventListener('load', () => {
                console.log('loaded')
                resolveLoaded()
            })
            document.head.append(stylesheetLink)
            document.body.append(indexScript)
        })()
    }, [])

    react.useEffect(() => {
        void (async () => {
            const ideVersion = (
                await backendService.listVersions({
                    versionType: cloudService.VersionType.ide,
                    default: true,
                })
            )[0]
            const backendVersion = (
                await backendService.listVersions({
                    versionType: cloudService.VersionType.backend,
                    default: true,
                })
            )[0]
            const projectIdeVersion = project.ideVersion?.value ?? ideVersion.number.value
            const projectEngineVersion = project.engineVersion?.value ?? backendVersion.number.value
            await loaded
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
