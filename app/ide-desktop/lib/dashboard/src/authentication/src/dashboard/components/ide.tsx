/** @file Container that launches the IDE. */
import * as react from 'react'

import * as cloudService from '../cloudService'
import * as dashboard from './dashboard'
import * as platformModule from '../../platform'
import * as projectManagerService from '../localService'
import * as svg from '../../components/svg'

// =================
// === Constants ===
// =================

/** The `id` attribute of the element into which the IDE will be rendered. */
const IDE_ELEMENT_ID = 'root'
const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'

// =================
// === Component ===
// =================

interface Props {
    platform: platformModule.Platform
    project: cloudService.Project
    backendService: cloudService.Backend | projectManagerService.Backend
    setTab: (tab: dashboard.Tab) => void
}

/** Container that launches the IDE. */
function Ide(props: Props) {
    const { project, backendService, setTab } = props

    react.useEffect(() => {
        void (async () => {
            // This is safe; this element exists in `index.html`.
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            const ideElement = document.getElementById(IDE_ELEMENT_ID)!
            while (ideElement.firstChild) {
                ideElement.removeChild(ideElement.firstChild)
            }
            const ideVersion =
                project.ideVersion?.value ??
                ('listVersions' in backendService
                    ? await backendService.listVersions({
                          versionType: cloudService.VersionType.ide,
                          default: true,
                      })
                    : null)?.[0].number.value
            const engineVersion =
                project.engineVersion?.value ??
                ('listVersions' in backendService
                    ? await backendService.listVersions({
                          versionType: cloudService.VersionType.backend,
                          default: true,
                      })
                    : null)?.[0].number.value
            const script = document.createElement('script')
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
                script.src = `${IDE_CDN_URL}/${engineVersion}/index.js.gz`
                script.onload = async () => {
                    await window.enso.main({
                        loader: {
                            assetsUrl: `${IDE_CDN_URL}/${ideVersion}/dynamic-assets`,
                            wasmUrl: `${IDE_CDN_URL}/${ideVersion}/pkg-opt.wasm`,
                            jsUrl: `${IDE_CDN_URL}/${ideVersion}/pkg.js.gz`,
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
                document.body.appendChild(script)
                return
            }
        })()
    }, [project])

    return (
        <>
            <div id="root" />
            <button
                onClick={() => {
                    setTab(dashboard.Tab.dashboard)
                }}
                className="fixed top-4 right-8 z-10"
            >
                {svg.CLOSE_ICON}
            </button>
        </>
    )
}

export default Ide
