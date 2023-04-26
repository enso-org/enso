/** @file Container that launches the IDE. */
import * as react from 'react'

import * as service from '../service'

// =================
// === Constants ===
// =================

/** The `id` attribute of the element that the IDE will be rendered into. */
const IDE_ELEMENT_ID = 'root'
const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'

// =================
// === Component ===
// =================

interface Props {
    project: service.Project
    backendService: service.Backend
}

/** Container that launches the IDE. */
function Ide(props: Props) {
    const { project, backendService } = props
    const [ideElement] = react.useState(() => document.querySelector(IDE_ELEMENT_ID))
    const [[loaded, resolveLoaded]] = react.useState((): [Promise<void>, () => void] => {
        let resolve!: () => void
        const promise = new Promise<void>(innerResolve => {
            resolve = innerResolve
        })
        return [promise, resolve]
    })

    react.useEffect(() => {
        document.getElementById(IDE_ELEMENT_ID)?.classList.remove('hidden')
        return () => {
            document.getElementById(IDE_ELEMENT_ID)?.classList.add('hidden')
        }
    }, [])

    react.useEffect(() => {
        void (async () => {
            const ideVersion = (
                await backendService.listVersions({
                    versionType: service.VersionType.ide,
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
            while (ideElement?.firstChild) {
                ideElement.removeChild(ideElement.firstChild)
            }
            const ideVersion = (
                await backendService.listVersions({
                    versionType: service.VersionType.ide,
                    default: true,
                })
            )[0]
            const backendVersion = (
                await backendService.listVersions({
                    versionType: service.VersionType.backend,
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

    return <></>
}

export default Ide
